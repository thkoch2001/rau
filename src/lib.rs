use std::{
    os::fd::AsFd,
    sync::{
        Arc,
        mpsc::{Receiver, Sender, channel},
    },
};

use anyhow::Context;
use emacs::{Env, FromLisp, IntoLisp, Result, Value, defun};
use nix::{
    poll::PollTimeout,
    sys::{
        eventfd::{EfdFlags, EventFd},
        signal,
    },
    unistd::Pid,
};
use wayland_client::{Connection, Dispatch, protocol::wl_registry};

use crate::river_wm::river_window_manager_v1::RiverWindowManagerV1;

mod river_wm {
    pub extern crate wayland_client;
    pub use wayland_client::protocol::wl_surface;

    mod interfaces {
        pub use wayland_client::protocol::__interfaces::*;
        wayland_scanner::generate_interfaces!("./protocol/river-window-management-v1.xml");
    }

    use self::interfaces::*;
    wayland_scanner::generate_client_code!("./protocol/river-window-management-v1.xml");
}

emacs::plugin_is_GPL_compatible!();

#[emacs::module(name = "libreka", defun_prefix = "reka")]
fn init(_: &Env) -> Result<()> {
    Ok(())
}

#[derive(Debug)]
enum FromEmacs {
    Whatever,
}

impl<'e> FromLisp<'e> for FromEmacs {
    fn from_lisp(_value: Value<'e>) -> Result<Self> {
        Ok(FromEmacs::Whatever)
    }
}

#[derive(Debug)]
enum ToEmacs {
    Next(usize),
}

impl<'e> IntoLisp<'e> for ToEmacs {
    fn into_lisp(self, env: &'e Env) -> Result<Value<'e>> {
        match self {
            ToEmacs::Next(n) => n.into_lisp(env),
        }
    }
}

struct Handle {
    tx: Sender<FromEmacs>,
    rx: Receiver<ToEmacs>,
    fd: Arc<EventFd>,
}

#[defun]
fn read_command<'e>(env: &'e Env, handle: &Handle) -> Result<Value<'e>> {
    if let Ok(value) = handle.rx.try_recv() {
        return value.into_lisp(env);
    }

    ().into_lisp(env)
}

#[defun]
fn send_command<'e>(env: &'e Env, handle: &Handle, val: Value<'e>) -> Result<Value<'e>> {
    handle.tx.send(FromEmacs::from_lisp(val)?)?;
    handle.fd.write(1)?;
    log::info!("sent command from Emacs");
    ().into_lisp(env)
}

#[defun(user_ptr)]
fn start_wm(env: &Env) -> Result<Handle> {
    let (tx, rx) = channel::<FromEmacs>();
    let (tx_e, rx_e) = channel::<ToEmacs>();

    let emacs_fd = Arc::new(EventFd::from_value_and_flags(0, EfdFlags::EFD_NONBLOCK)?);
    let emacs_fd_wmside = emacs_fd.clone();

    std::thread::spawn(move || {
        let result = wm_loop(rx, tx_e, emacs_fd_wmside);
        if let Err(e) = result {
            log::error!("reka window manager thread crashed: {:?}", e);
        }
    });

    env.message("launched reka window manager! have fun ...")?;
    Ok(Handle {
        tx,
        rx: rx_e,
        fd: emacs_fd,
    })
}

fn wm_loop(rx: Receiver<FromEmacs>, tx: Sender<ToEmacs>, emacs_fd: Arc<EventFd>) -> Result<()> {
    env_logger::Builder::from_env(env_logger::Env::default().default_filter_or("info"))
        .target(env_logger::Target::Stderr)
        .init();

    let pid = std::process::id() as i32;

    log::info!("starting reka window manager thread (PID {}) ...", pid);
    let conn = Connection::connect_to_env().context("setting up Wayland connection")?;
    let display = conn.display();
    let mut event_queue = conn.new_event_queue();
    let qh = event_queue.handle();
    let mut wm = Reka {
        pid,
        rx,
        tx,
        emacs_fd,
        seat: None,
        frames: vec![],
        outputs: vec![],
        river_wm: None,
    };
    let _registry = display.get_registry(&qh, ());

    let mut counter = 0;

    loop {
        event_queue.flush()?;
        log::debug!("awaiting event from emacs or river");
        let guard = match event_queue.prepare_read() {
            Some(g) => g,
            None => {
                event_queue.dispatch_pending(&mut wm)?;
                continue;
            }
        };

        let river_fd = guard.connection_fd();

        let (emacs_ready, river_ready) = {
            let mut polls = [
                nix::poll::PollFd::new(wm.emacs_fd.as_fd(), nix::poll::PollFlags::POLLIN),
                nix::poll::PollFd::new(river_fd, nix::poll::PollFlags::POLLIN),
            ];

            let timeout: u16 = 10_000; // TODO nicely named const
            let count = loop {
                match nix::poll::poll(&mut polls, PollTimeout::from(timeout)) {
                    Ok(c) => break c,
                    Err(nix::errno::Errno::EINTR) => {
                        log::debug!("poll interrupted, continuing");
                        continue;
                    }
                    Err(e) => return Err(e).context("while polling emacs/river"),
                }
            };

            if count == 0 {
                continue;
            }

            (
                polls[0]
                    .revents()
                    .expect("emacs_fd::revents() should not return None")
                    .contains(nix::poll::PollFlags::POLLIN),
                polls[1]
                    .revents()
                    .expect("river_fd::revents() should not return None")
                    .contains(nix::poll::PollFlags::POLLIN),
            )
        };

        // handle commands from emacs first (as they might influence what we respond to river)
        if emacs_ready {
            log::debug!("emacs has commands ready");
            let mut buf = [0u8; 8];
            let _ = nix::unistd::read(&wm.emacs_fd, &mut buf);
            wm.handle_emacs_commands()?;
        }

        if river_ready {
            log::debug!("river has events ready");
            guard.read()?;
            event_queue.dispatch_pending(&mut wm)?;
            wm.send(ToEmacs::Next(counter))?;
            counter += 1;
        }
    }
}

enum FrameState {
    Minimized,
    Displayed(river_wm::river_output_v1::RiverOutputV1),
}

struct Frame {
    state: FrameState,
    window: river_wm::river_window_v1::RiverWindowV1,
}

struct Reka {
    pid: i32,

    rx: Receiver<FromEmacs>,
    tx: Sender<ToEmacs>,
    emacs_fd: Arc<EventFd>,

    seat: Option<river_wm::river_seat_v1::RiverSeatV1>,
    frames: Vec<Frame>,
    outputs: Vec<river_wm::river_output_v1::RiverOutputV1>,
    river_wm: Option<RiverWindowManagerV1>,
}

impl Reka {
    fn handle_emacs_commands(&mut self) -> Result<()> {
        while let Ok(command) = self.rx.try_recv() {
            log::debug!("got command: {:?}", command)
        }

        Ok(())
    }

    fn send(&self, cmd: ToEmacs) -> Result<()> {
        self.tx.send(cmd)?;
        signal::kill(Pid::this(), Some(signal::Signal::SIGUSR1))?;
        Ok(())
    }
}

impl Dispatch<wl_registry::WlRegistry, ()> for Reka {
    fn event(
        state: &mut Self,
        proxy: &wl_registry::WlRegistry,
        event: <wl_registry::WlRegistry as wayland_client::Proxy>::Event,
        _data: &(),
        _conn: &Connection,
        qhandle: &wayland_client::QueueHandle<Self>,
    ) {
        if let wl_registry::Event::Global {
            name, interface, ..
        } = event
        {
            if interface == "river_window_manager_v1" {
                let wm = proxy.bind::<RiverWindowManagerV1, _, _>(name, 3, qhandle, ());
                state.river_wm = Some(wm);
                log::debug!("registering reka window manager ...");
            }
        }
    }
}

impl Dispatch<RiverWindowManagerV1, ()> for Reka {
    fn event(
        state: &mut Self,
        proxy: &RiverWindowManagerV1,
        event: <RiverWindowManagerV1 as wayland_client::Proxy>::Event,
        _data: &(),
        _conn: &Connection,
        _qhandle: &wayland_client::QueueHandle<Self>,
    ) {
        match event {
            // manage sequence: window dimensions, fullscreen state, keyboard focus, decorations, capabilities ...
            river_wm::river_window_manager_v1::Event::ManageStart => {
                log::debug!("manage sequence started");

                // reconcile frames with outputs (each output gets one full-screen frame)
                for (idx, frame) in state.frames.iter_mut().enumerate() {
                    if state.outputs.len() > idx {
                        // frame should be displayed
                        match &frame.state {
                            // everything is correct already
                            FrameState::Displayed(output) if output == &state.outputs[idx] => {}

                            // need to assign new output
                            FrameState::Minimized | FrameState::Displayed(_) => {
                                let output = &state.outputs[idx];
                                frame.state = FrameState::Displayed(output.clone());
                                frame.window.fullscreen(output);
                            }
                        }
                    }
                }

                if let Some(seat) = &state.seat
                    && !state.frames.is_empty()
                {
                    seat.focus_window(&state.frames[0].window);
                }

                proxy.manage_finish();
            }

            // render sequence: positions, z-order, borders, visibility (?), clipping (?)
            river_wm::river_window_manager_v1::Event::RenderStart => {
                proxy.render_finish();
            }

            river_wm::river_window_manager_v1::Event::Output { id } => {
                log::debug!("RiverWindowManagerV1::Event::Output received: id={:?}", id);
                state.outputs.push(id);
            }

            river_wm::river_window_manager_v1::Event::Seat { id } => {
                log::debug!("RiverWindowManagerV1::Event::Seat received: id={:?}", id);
                if state.seat.is_none() {
                    state.seat = Some(id);
                } else {
                    log::error!(
                        "seat is already taken, reka does not support multi-seats at the moment"
                    );
                }
            }

            river_wm::river_window_manager_v1::Event::Unavailable => {
                log::info!("RiverWindowManagerV1::Event::Unavailable received");
            }
            river_wm::river_window_manager_v1::Event::Finished => {
                log::info!("RiverWindowManagerV1::Event::Finished received");
            }
            river_wm::river_window_manager_v1::Event::SessionLocked => {
                log::info!("RiverWindowManagerV1::Event::SessionLocked received");
            }
            river_wm::river_window_manager_v1::Event::SessionUnlocked => {
                log::info!("RiverWindowManagerV1::Event::SessionUnlocked received");
            }
            river_wm::river_window_manager_v1::Event::Window { id } => {
                log::debug!("RiverWindowManagerV1::Event::Window received: id={:?}", id);
            }
        }
    }

    wayland_client::event_created_child!(Reka, RiverWindowManagerV1, [
        river_wm::river_window_manager_v1::EVT_WINDOW_OPCODE => (river_wm::river_window_v1::RiverWindowV1, ()),
        river_wm::river_window_manager_v1::EVT_OUTPUT_OPCODE => (river_wm::river_output_v1::RiverOutputV1, ()),
        river_wm::river_window_manager_v1::EVT_SEAT_OPCODE => (river_wm::river_seat_v1::RiverSeatV1, ()),
    ]);
}

impl Dispatch<river_wm::river_output_v1::RiverOutputV1, ()> for Reka {
    fn event(
        _state: &mut Self,
        _proxy: &river_wm::river_output_v1::RiverOutputV1,
        event: <river_wm::river_output_v1::RiverOutputV1 as wayland_client::Proxy>::Event,
        _data: &(),
        _conn: &Connection,
        _qhandle: &wayland_client::QueueHandle<Self>,
    ) {
        log::debug!("RiverOutputV1 event received: {:?}", event);
    }
}

impl Dispatch<river_wm::river_seat_v1::RiverSeatV1, ()> for Reka {
    fn event(
        _state: &mut Self,
        _proxy: &river_wm::river_seat_v1::RiverSeatV1,
        event: <river_wm::river_seat_v1::RiverSeatV1 as wayland_client::Proxy>::Event,
        _data: &(),
        _conn: &Connection,
        _qhandle: &wayland_client::QueueHandle<Self>,
    ) {
        log::debug!("RiverSeatV1 event received: {:?}", event);
    }
}

impl Dispatch<river_wm::river_window_v1::RiverWindowV1, ()> for Reka {
    fn event(
        state: &mut Self,
        proxy: &river_wm::river_window_v1::RiverWindowV1,
        event: <river_wm::river_window_v1::RiverWindowV1 as wayland_client::Proxy>::Event,
        _data: &(),
        _conn: &Connection,
        _qhandle: &wayland_client::QueueHandle<Self>,
    ) {
        match event {
            river_wm::river_window_v1::Event::UnreliablePid { unreliable_pid }
                if unreliable_pid == state.pid =>
            {
                log::info!("discovered new Emacs frame ...");
                state.frames.push(Frame {
                    state: FrameState::Minimized,
                    window: proxy.clone(),
                });
            }
            _ => log::debug!("RiverWindowV1 event received: {:?}", event),
        }
    }
}
