use std::{
    cell::RefCell,
    collections::HashSet,
    os::fd::AsFd,
    sync::{
        Arc, RwLock,
        mpsc::{Receiver, Sender, channel},
    },
};

use anyhow::Context;
use emacs::{Env, IntoLisp, Result, Value, Vector, defun, use_symbols};
use nix::{
    poll::PollTimeout,
    sys::{
        eventfd::{EfdFlags, EventFd},
        signal,
    },
    unistd::Pid,
};
use wayland_client::{Connection, Dispatch, protocol::wl_registry};

use crate::river_wm::{
    river_window_manager_v1::RiverWindowManagerV1,
    river_window_v1::{Edges, RiverWindowV1},
};

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

use_symbols!(
    kill_buffer
    reka_get_window => "reka--get-window"
    reka_create_buffer => "reka--create-buffer"
    reka_list_buffers => "reka--list-buffers"
);

#[emacs::module(name = "libreka", defun_prefix = "reka")]
fn init(_: &Env) -> Result<()> {
    Ok(())
}

#[derive(Debug)]
enum FromEmacs {}

#[derive(Debug)]
enum ToEmacs {
    Next(u64),
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
    windows: Arc<RwLock<Vec<Window>>>,
}

fn contains_by<T, F: Fn(&T) -> bool>(v: &Vec<T>, f: F) -> bool {
    for e in v {
        if f(e) {
            return true;
        }
    }

    return false;
}

#[defun] // TODO: private?
fn reconcile_window_buffers<'e>(env: &'e Env, handle: &Handle) -> Result<Value<'e>> {
    let mut windows = handle.windows.write().expect("windows rwlock poisoned");
    let buffers = env.call(reka_list_buffers, [])?.into_rust::<Vector<'e>>()?;
    let mut seen = HashSet::<RiverWindowV1>::new();

    log::debug!("found {} reka buffers", buffers.len()); // TODO remove
    for buffer in buffers.into_iter() {
        let window_ptr = env.call(reka_get_window, [buffer])?;
        if !window_ptr.is_not_nil() {
            // invalid buffer?
            log::warn!("found reka buffer without any window object, destroying!");
            env.call(kill_buffer, [buffer])?;
            continue;
        }

        let window_cell: &RefCell<RiverWindowV1> = window_ptr.into_rust()?;
        let window = window_cell.borrow();
        if !contains_by(&windows, |w: &Window| window.eq(&w.window)) {
            log::info!("found orphaned reka buffer, destroying!");
            env.call(kill_buffer, [buffer])?;
            continue;
        }

        seen.insert(window.clone());
    }

    log::debug!("saw {} valid reka buffers", seen.len());

    // create missing buffers and mark windows as active
    for window in windows.iter_mut() {
        match &window.state {
            WindowState::Active if seen.contains(&window.window) => {}
            WindowState::Killed => {}

            WindowState::Active => {
                // buffer is gone, but maybe the hook didn't run? clean up
                window.state = WindowState::Killed;
            }

            WindowState::Starting => {
                log::debug!("creating new buffer for window {:?}", window);
                let window_value = RefCell::new(window.window.clone()).into_lisp(env)?;
                env.call(reka_create_buffer, [window_value])?;
                window.state = WindowState::Active;
            }
        }
    }

    ().into_lisp(env)
}

#[defun]
fn read_command<'e>(env: &'e Env, handle: &Handle) -> Result<Value<'e>> {
    if let Ok(value) = handle.rx.try_recv() {
        return value.into_lisp(env);
    }

    ().into_lisp(env)
}

#[defun]
fn close_window<'e>(
    env: &'e Env,
    handle: &Handle,
    window: &RefCell<RiverWindowV1>,
) -> Result<Value<'e>> {
    let mut windows = handle.windows.write().unwrap();

    for w in windows.iter_mut() {
        if window.borrow().eq(&w.window) {
            w.state = WindowState::Killed;
            break;
        }
    }

    ().into_lisp(env)
}

#[defun(user_ptr)]
fn start_wm(env: &Env) -> Result<Handle> {
    let (tx, rx) = channel::<FromEmacs>();
    let (tx_e, rx_e) = channel::<ToEmacs>();

    let emacs_fd = Arc::new(EventFd::from_value_and_flags(0, EfdFlags::EFD_NONBLOCK)?);
    let emacs_fd_wmside = emacs_fd.clone();

    let windows = Arc::new(RwLock::new(Vec::new()));
    let windows_wmside = windows.clone();

    std::thread::spawn(move || {
        let result = wm_loop(rx, tx_e, emacs_fd_wmside, windows_wmside);
        if let Err(e) = result {
            log::error!("reka window manager thread crashed: {:?}", e);
        }
    });

    env.message("launched reka window manager! have fun ...")?;
    Ok(Handle {
        tx,
        rx: rx_e,
        fd: emacs_fd,
        windows,
    })
}

#[derive(Clone, Debug, PartialEq)]
struct WindowParameters {
    window: RiverWindowV1,
    focused: bool,
    x: i32,
    y: i32,
    h: i32,
    w: i32,
}

#[defun(user_ptr)]
fn make_window_parameters<'e>(
    _env: &'e Env,
    window: &RiverWindowV1,
    focused: Value<'e>,
    x: i32,
    y: i32,
    w: i32,
    h: i32,
) -> Result<WindowParameters> {
    let params = WindowParameters {
        window: window.clone(),
        focused: focused.is_not_nil(),
        x,
        y,
        w,
        h,
    };

    log::debug!("created window params: {:?}", params);
    Ok(params)
}

#[defun]
fn update_window_parameters<'e>(
    env: &'e Env,
    handle: &Handle,
    per_frame: Vector<'e>,
) -> Result<Value<'e>> {
    for frame in per_frame.into_iter() {
        let params = frame.cdr::<Vector<'e>>()?;

        'params: for p in params.into_iter() {
            // TODO: what a mess!
            let wp: &RefCell<WindowParameters> = p.into_rust()?;
            let wp_ref = wp.borrow();
            let mut windows = handle.windows.write().unwrap();

            for w in windows.iter_mut() {
                if w.window.eq(&wp_ref.window) {
                    w.params = Some(wp_ref.clone());
                    continue 'params;
                }
            }
        }
    }

    ().into_lisp(env)
}

fn wm_loop(
    rx: Receiver<FromEmacs>,
    tx: Sender<ToEmacs>,
    emacs_fd: Arc<EventFd>,
    windows: Arc<RwLock<Vec<Window>>>,
) -> Result<()> {
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
        iteration: 0,
        rx,
        tx,
        emacs_fd,
        seat: None,
        frames: vec![],
        outputs: vec![],
        river_wm: None,
        windows,
    };
    let _registry = display.get_registry(&qh, ());

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
        }
    }
}

struct Output {
    output: river_wm::river_output_v1::RiverOutputV1,
    x: i32,
    y: i32,
    width: i32,
    height: i32,
}

enum FrameState {
    Minimized,
    Displayed(river_wm::river_output_v1::RiverOutputV1),
}

struct Frame {
    name: Option<String>,
    state: FrameState,
    node: river_wm::river_node_v1::RiverNodeV1,
    window: RiverWindowV1,
}

#[derive(Debug, PartialEq)]
enum WindowState {
    Starting,
    Active,
    Killed,
}

#[derive(Debug)]
struct Window {
    window: RiverWindowV1,
    node: river_wm::river_node_v1::RiverNodeV1,
    title: Option<String>,
    pid: Option<i32>,
    state: WindowState,
    params: Option<WindowParameters>,
}

struct Seat {
    seat: river_wm::river_seat_v1::RiverSeatV1,
    focus: Option<RiverWindowV1>,
}

struct Reka {
    pid: i32,
    iteration: u64,

    // Emacs-related state
    rx: Receiver<FromEmacs>,
    tx: Sender<ToEmacs>,
    emacs_fd: Arc<EventFd>,

    // river-related state
    seat: Option<Seat>,
    frames: Vec<Frame>,
    outputs: Vec<Output>,
    river_wm: Option<RiverWindowManagerV1>,
    windows: Arc<RwLock<Vec<Window>>>,
}

impl Reka {
    fn handle_emacs_commands(&mut self) -> Result<()> {
        // TODO: maybe we don't need it at all?
        Ok(())
    }

    fn send(&self, cmd: ToEmacs) -> Result<()> {
        self.tx.send(cmd)?;
        signal::kill(Pid::this(), Some(signal::Signal::SIGUSR1))?;
        Ok(())
    }

    // reconcile_frames ensures that each output gets one maximized Emacs frame.
    fn reconcile_frames(&mut self) {
        for (idx, frame) in self.frames.iter_mut().enumerate() {
            if self.outputs.len() > idx {
                let output = &self.outputs[idx];

                // frame should be displayed
                match &frame.state {
                    // everything is correct already
                    FrameState::Displayed(o) if o == &output.output => {}

                    // need to assign new output
                    FrameState::Minimized | FrameState::Displayed(_) => {
                        frame.state = FrameState::Displayed(output.output.clone());
                    }
                }

                // always propose dimensions to keep frame sized to output
                frame.window.propose_dimensions(output.width, output.height);
                frame.window.inform_maximized();
                frame.window.set_tiled(Edges::all());
            } else {
                // frame should be hidden
                if let FrameState::Displayed(_) = frame.state {
                    frame.state = FrameState::Minimized;
                }
            }
        }
    }

    // reconcile_focus updates the seat's focus, attempting to select something
    // useful if focus is lost
    fn reconcile_focus(&mut self) {
        // reconcile focus
        if let Some(seat) = &mut self.seat {
            if seat.focus.is_none() {
                // just select any active frame
                for frame in &self.frames {
                    if let FrameState::Displayed(_) = frame.state {
                        seat.focus = Some(frame.window.clone());
                    }
                }
            }

            if let Some(focus) = &seat.focus {
                seat.seat.focus_window(focus);
            }
        }
    }

    // reconcile_windows closes killed windows and removes them from the list
    fn reconcile_windows(&mut self) {
        let windows = self.windows.read().unwrap();
        for w in windows.iter() {
            match &w.state {
                WindowState::Killed => {
                    log::info!("requesting window closure");
                    w.window.close();
                }
                WindowState::Starting => {
                    log::info!("ignoring starting window");
                }
                WindowState::Active => {
                    log::info!("found active window: {:?}", w);
                    if let Some(params) = &w.params {
                        log::info!("proposing dimensions etc");
                        w.window.set_tiled(Edges::all());
                        w.window.propose_dimensions(params.w, params.h);
                    }
                }
            }
        }

        // TODO: force kill at some point
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
        qhandle: &wayland_client::QueueHandle<Self>,
    ) {
        match event {
            // manage sequence: window dimensions, fullscreen state, keyboard
            // focus, decorations, capabilities ...
            river_wm::river_window_manager_v1::Event::ManageStart => {
                log::debug!("manage sequence started");

                state.reconcile_frames();
                state.reconcile_focus();
                state.reconcile_windows();

                state.iteration += 1;
                if let Err(e) = state.send(ToEmacs::Next(state.iteration)) {
                    log::error!(
                        "failed to notify Emacs (iteration {}): {}",
                        state.iteration,
                        e
                    );
                }
                proxy.manage_finish();
            }

            // render sequence: positions, z-order, borders, visibility (?), clipping (?)
            river_wm::river_window_manager_v1::Event::RenderStart => {
                // reconcile frame display state
                for frame in state.frames.iter() {
                    match &frame.state {
                        FrameState::Minimized => frame.window.hide(),
                        FrameState::Displayed(output_proxy) => {
                            frame.window.show();
                            frame.node.place_bottom();

                            // position frame at its output's origin
                            if let Some(output) =
                                state.outputs.iter().find(|o| &o.output == output_proxy)
                            {
                                frame.node.set_position(output.x, output.y);
                            }
                        }
                    }
                }

                let windows = state.windows.read().unwrap();
                for window in windows.iter() {
                    if let WindowState::Active = window.state {
                        log::info!("display time!!");
                        if let Some(params) = &window.params {
                            log::info!("showing!!");
                            window.window.show();
                            window.node.set_position(params.x, params.y);
                            window.node.place_top();
                        } else {
                            log::info!("hiding!!");
                            window.window.hide();
                        }
                    }
                }

                proxy.render_finish();
            }

            river_wm::river_window_manager_v1::Event::Output { id } => {
                log::debug!("RiverWindowManagerV1::Event::Output received: id={:?}", id);
                state.outputs.push(Output {
                    output: id,
                    x: 0,
                    y: 0,
                    width: 0,
                    height: 0,
                });
            }

            river_wm::river_window_manager_v1::Event::Seat { id } => {
                log::debug!("RiverWindowManagerV1::Event::Seat received: id={:?}", id);
                if state.seat.is_none() {
                    state.seat = Some(Seat {
                        seat: id,
                        focus: None,
                    });
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
                let node = id.get_node(qhandle, ());
                state.windows.write().unwrap().push(Window {
                    window: id,
                    node,
                    title: None,
                    pid: None,
                    state: WindowState::Starting,
                    params: None,
                });
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
        state: &mut Self,
        proxy: &river_wm::river_output_v1::RiverOutputV1,
        event: <river_wm::river_output_v1::RiverOutputV1 as wayland_client::Proxy>::Event,
        _data: &(),
        _conn: &Connection,
        _qhandle: &wayland_client::QueueHandle<Self>,
    ) {
        log::debug!("RiverOutputV1 event received: {:?}", event);

        match event {
            river_wm::river_output_v1::Event::Removed => {
                log::debug!("output disconnected, removing");
                for (idx, output) in state.outputs.iter().enumerate() {
                    if &output.output == proxy {
                        state.outputs.remove(idx);
                        break;
                    }
                }
            }
            river_wm::river_output_v1::Event::Position { x, y } => {
                log::debug!("output position: x={}, y={}", x, y);
                for output in state.outputs.iter_mut() {
                    if &output.output == proxy {
                        output.x = x;
                        output.y = y;
                        break;
                    }
                }
            }
            river_wm::river_output_v1::Event::Dimensions { width, height } => {
                log::debug!("output dimensions: {}x{}", width, height);
                for output in state.outputs.iter_mut() {
                    if &output.output == proxy {
                        output.width = width;
                        output.height = height;
                        break;
                    }
                }
            }
            _ => {}
        }
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

impl Dispatch<RiverWindowV1, ()> for Reka {
    fn event(
        state: &mut Self,
        proxy: &RiverWindowV1,
        event: <RiverWindowV1 as wayland_client::Proxy>::Event,
        _data: &(),
        _conn: &Connection,
        _qh: &wayland_client::QueueHandle<Self>,
    ) {
        log::debug!("RiverWindowV1 event received: {:?}", event);

        match event {
            river_wm::river_window_v1::Event::UnreliablePid { unreliable_pid } => {
                let mut windows = state.windows.write().unwrap();
                if let Some(pos) = windows.iter().position(|w| &w.window == proxy) {
                    windows[pos].pid = Some(unreliable_pid);

                    if unreliable_pid == state.pid {
                        log::info!("discovered new Emacs frame ...");
                        let window = windows.remove(pos);
                        state.frames.push(Frame {
                            name: None,
                            state: FrameState::Minimized,
                            node: window.node,
                            window: window.window,
                        });
                    } else {
                        log::info!("discovered new window (PID: {}) ...", unreliable_pid);
                    }
                }
            }
            river_wm::river_window_v1::Event::Title { title } => {
                let mut windows = state.windows.write().unwrap();
                for w in windows.iter_mut() {
                    if w.window.eq(proxy) {
                        w.title = title;
                        return;
                    }
                }

                if let Some(s) = &title
                    && s.starts_with("reka-frame-")
                {
                    for f in state.frames.iter_mut() {
                        if f.window.eq(proxy) {
                            f.name = title;
                            return;
                        }
                    }
                }

                log::warn!("received title for unknown window, orphan frame?");
            }
            river_wm::river_window_v1::Event::Closed => {
                let mut windows = state.windows.write().unwrap();
                for (i, w) in windows.iter().enumerate() {
                    if w.window.eq(proxy) {
                        windows.swap_remove(i);
                        return;
                    }
                }
            },
            _ => {}
        }
    }
}

impl Dispatch<river_wm::river_node_v1::RiverNodeV1, ()> for Reka {
    fn event(
        _state: &mut Self,
        _proxy: &river_wm::river_node_v1::RiverNodeV1,
        event: <river_wm::river_node_v1::RiverNodeV1 as wayland_client::Proxy>::Event,
        _data: &(),
        _conn: &Connection,
        _qhandle: &wayland_client::QueueHandle<Self>,
    ) {
        log::debug!("RiverNodeV1 event received: {:?}", event);
    }
}
