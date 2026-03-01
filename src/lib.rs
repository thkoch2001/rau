// SPDX-License-Identifier: GPL-3.0-or-later

use std::{
    cell::RefCell,
    collections::HashMap,
    os::fd::AsFd,
    sync::Arc,
    sync::mpsc::{Receiver, Sender, channel},
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
use xkbcommon::xkb;

use crate::river::{
    river_output_v1::RiverOutputV1,
    river_seat_v1::Modifiers,
    river_window_manager_v1::RiverWindowManagerV1,
    river_window_v1::{Edges, RiverWindowV1},
    river_xkb_binding_v1::RiverXkbBindingV1,
    river_xkb_bindings_v1::RiverXkbBindingsV1,
};

mod river {
    pub extern crate wayland_client;
    pub use wayland_client::protocol::wl_surface;

    mod interfaces {
        pub(super) mod wm {
            pub use wayland_client::protocol::__interfaces::*;
            wayland_scanner::generate_interfaces!("./protocol/river-window-management-v1.xml");
        }

        pub(super) mod xkb {
            use super::wm::*;
            wayland_scanner::generate_interfaces!("./protocol/river-xkb-bindings-v1.xml");
        }
    }

    use self::interfaces::wm::*;
    use self::interfaces::xkb::*;
    wayland_scanner::generate_client_code!("./protocol/river-window-management-v1.xml");
    wayland_scanner::generate_client_code!("./protocol/river-xkb-bindings-v1.xml");
}

emacs::plugin_is_GPL_compatible!();

use_symbols!(
    cons
    list
    key_event
    new_window
    window_closed
    focused
    title_change
    frame_request
    reka_get_window => "reka--get-window"
    reka_create_buffer => "reka--create-buffer"
    reka_list_buffers => "reka--list-buffers"
);

#[emacs::module(name = "libreka", defun_prefix = "reka")]
fn init(_: &Env) -> Result<()> {
    Ok(())
}

#[derive(Debug)]
enum FromEmacs {
    RegisterPrefix(XKBPrefix),
    FocusWindow(RiverWindowV1),
    FocusFrame,
    CloseWindow(RiverWindowV1),
    BufferCreated(RiverWindowV1),
    UpdateParameters(Vec<WindowParameters>),
}

#[derive(Debug)]
enum ToEmacs {
    KeyEvent(u32),
    NewWindow(RiverWindowV1),
    WindowClosed(RiverWindowV1),
    Focused(RiverWindowV1),
    TitleChange(RiverWindowV1, String),
    RequestFrame,
}

impl<'e> IntoLisp<'e> for ToEmacs {
    fn into_lisp(self, env: &'e Env) -> Result<Value<'e>> {
        match self {
            ToEmacs::KeyEvent(event) => env.call(cons, (key_event, event)),
            ToEmacs::NewWindow(win) => env.call(cons, (new_window, RefCell::new(win))),
            ToEmacs::WindowClosed(win) => env.call(cons, (window_closed, RefCell::new(win))),
            ToEmacs::Focused(win) => env.call(cons, (focused, RefCell::new(win))),
            ToEmacs::TitleChange(win, title) => {
                env.call(list, (title_change, RefCell::new(win), title))
            }
            ToEmacs::RequestFrame => frame_request.into_lisp(env),
        }
    }
}

struct Handle {
    fd: Arc<EventFd>,
    tx: Sender<FromEmacs>,
    rx: Receiver<ToEmacs>,
}

impl Handle {
    fn send(&self, msg: FromEmacs) -> Result<()> {
        self.tx.send(msg)?;
        self.fd.write(1)?;
        Ok(())
    }
}

#[defun]
fn close_window<'e>(env: &'e Env, handle: &Handle, window: &RiverWindowV1) -> Result<Value<'e>> {
    handle.send(FromEmacs::CloseWindow(window.clone()))?;
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
        fd: emacs_fd,
        tx,
        rx: rx_e,
    })
}

#[derive(Clone, Debug, PartialEq)]
struct WindowParameters {
    window: RiverWindowV1,
    frame_name: String,
    x: i32,
    y: i32,
    h: i32,
    w: i32,
}

#[defun(user_ptr)]
fn make_window_parameters<'e>(
    _env: &'e Env,
    window: &RiverWindowV1,
    frame_name: String,
    x: i32,
    y: i32,
    w: i32,
    h: i32,
) -> Result<WindowParameters> {
    let params = WindowParameters {
        window: window.clone(),
        frame_name,
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
    let mut new_params: Vec<WindowParameters> = Vec::new();
    for elem in per_frame.into_iter() {
        let params: Vector<'e> = elem.into_rust()?;
        for p in params.into_iter() {
            let wp: &RefCell<WindowParameters> = p.into_rust()?;
            new_params.push(wp.borrow().clone());
        }
    }

    handle.send(FromEmacs::UpdateParameters(new_params))?;
    ().into_lisp(env)
}

#[defun]
fn set_focus_request<'e>(env: &'e Env, handle: &Handle, window: Value<'e>) -> Result<Value<'e>> {
    if window.is_not_nil() {
        let cell: &RefCell<RiverWindowV1> = window.into_rust()?;
        handle.send(FromEmacs::FocusWindow(cell.borrow().clone()))?;
    } else {
        handle.send(FromEmacs::FocusFrame)?;
    }

    ().into_lisp(env)
}

#[defun]
fn notify_buffer_created<'e>(
    env: &'e Env,
    handle: &Handle,
    w: &RiverWindowV1,
) -> Result<Value<'e>> {
    handle.send(FromEmacs::BufferCreated(w.clone()))?;
    ().into_lisp(env)
}

#[defun]
fn window_equal<'e>(env: &'e Env, a: &RiverWindowV1, b: &RiverWindowV1) -> Result<Value<'e>> {
    a.eq(b).into_lisp(env)
}

#[defun]
fn register_xkb_prefix<'e>(
    env: &'e Env,
    handle: &Handle,
    event: u32,
    key: Value<'e>,
    modifiers_bits: u32,
) -> Result<Value<'e>> {
    // Emacs hands us either an int (Unicode codepoint for the key), or a string
    // with a "key name". The later is stuff like "XF86AudioRaiseVolume",
    // "Return", etc. which we map back to a keysym with XKB here.
    let keysym = if let Ok(codepoint) = key.into_rust::<i64>() {
        xkb::utf32_to_keysym(codepoint as u32)
    } else {
        let name: String = key.into_rust()?;
        xkb::keysym_from_name(&name, xkb::KEYSYM_CASE_INSENSITIVE)
    };

    if keysym == xkb::keysyms::KEY_NoSymbol.into() {
        return Err(anyhow::anyhow!("could not resolve XKB keysym for key").into());
    }

    let modifiers = Modifiers::from_bits(modifiers_bits).context("unknown modifier bits")?;
    let prefix = XKBPrefix {
        event,
        keysym: u32::from(keysym),
        modifiers,
    };
    handle.send(FromEmacs::RegisterPrefix(prefix))?;

    ().into_lisp(env)
}

#[defun]
fn get_next_command<'e>(env: &'e Env, handle: &Handle) -> Result<Value<'e>> {
    if let Ok(cmd) = handle.rx.try_recv() {
        log::debug!("emacs received command {:?}", cmd);
        return cmd.into_lisp(env);
    }

    ().into_lisp(env)
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
        pending_focus: None,
        frames: vec![],
        outputs: vec![],
        windows: vec![],
        river_wm: None,
        xkb_bindings: None,
        prefixes: Default::default(),
        active_frame: None,
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

        if emacs_ready {
            log::debug!("emacs signalled available data");
            let mut buf = [0u8; 8];
            let _ = nix::unistd::read(&wm.emacs_fd, &mut buf);

            let mut needs_manage = false;
            while let Ok(message) = wm.rx.try_recv() {
                log::debug!("message from emacs: {:?}", message);
                match message {
                    FromEmacs::RegisterPrefix(prefix) => {
                        needs_manage = true;
                        if !wm.prefixes.contains_key(&prefix) {
                            wm.prefixes.insert(prefix, BindingState::Requested);
                        }
                    }
                    FromEmacs::FocusWindow(window) => {
                        needs_manage = true;
                        wm.pending_focus = Some(window);
                    }
                    FromEmacs::FocusFrame => {
                        needs_manage = true;
                        wm.pending_focus = wm.active_frame.clone();
                    }
                    FromEmacs::CloseWindow(window) => {
                        for w in wm.windows.iter_mut() {
                            if window.eq(&w.window) {
                                w.state = WindowState::Killed;
                                needs_manage = true;
                                break;
                            }
                        }
                    }
                    FromEmacs::BufferCreated(window) => {
                        for w in wm.windows.iter_mut() {
                            if window.eq(&w.window) {
                                needs_manage = true;
                                w.state = WindowState::Active;
                            }
                        }
                    }
                    FromEmacs::UpdateParameters(new_params) => {
                        for w in wm.windows.iter_mut() {
                            let result =
                                new_params.iter().find(|p| p.window.eq(&w.window)).cloned();
                            if w.params != result {
                                w.params = result;
                                needs_manage = true;
                            }
                        }
                    }
                }
            }

            // All commands from Emacs need a manage sequence.
            if let Some(river_wm) = &wm.river_wm
                && needs_manage
            {
                river_wm.manage_dirty();
            }

            let register_prefixes = wm
                .prefixes
                .iter()
                .filter_map(|(k, v)| match v {
                    BindingState::Requested => Some(*k),
                    _ => None,
                })
                .collect::<Vec<_>>();

            if !register_prefixes.is_empty() && wm.xkb_bindings.is_some() && wm.seat.is_some() {
                let xkb = wm.xkb_bindings.as_ref().unwrap();
                let seat = wm.seat.as_ref().unwrap().seat.clone();
                let mut bindings = vec![];

                for p in &register_prefixes {
                    let b = xkb.get_xkb_binding(&seat, p.keysym, p.modifiers, &qh, ());
                    bindings.push(b);
                }

                for (p, b) in register_prefixes.into_iter().zip(bindings.into_iter()) {
                    wm.prefixes.insert(p, BindingState::Registered(b));
                }
            }
        }

        if river_ready {
            log::debug!("river has events ready");
            guard.read()?;
            event_queue.dispatch_pending(&mut wm)?;
        }
    }
}

struct Output {
    output: RiverOutputV1,
    x: i32,
    y: i32,
    width: i32,
    height: i32,
}

struct Frame {
    name: Option<String>,
    displayed_on: Option<RiverOutputV1>,
    node: river::river_node_v1::RiverNodeV1,
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
    node: river::river_node_v1::RiverNodeV1,
    title: Option<String>,
    pid: Option<i32>,
    state: WindowState,
    params: Option<WindowParameters>,
    actual_width_height: Option<(i32, i32)>,
}

struct Seat {
    seat: river::river_seat_v1::RiverSeatV1,
    focus: Option<RiverWindowV1>,
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
struct XKBPrefix {
    event: u32,
    keysym: u32,
    modifiers: Modifiers,
}

enum BindingState {
    Requested,
    Registered(RiverXkbBindingV1),
    Enabled(RiverXkbBindingV1),
}

struct Reka {
    pid: i32,

    // Emacs-related state
    rx: Receiver<FromEmacs>,
    tx: Sender<ToEmacs>,
    emacs_fd: Arc<EventFd>,

    // river-related state
    river_wm: Option<RiverWindowManagerV1>,
    xkb_bindings: Option<RiverXkbBindingsV1>,

    seat: Option<Seat>,
    pending_focus: Option<RiverWindowV1>,
    active_frame: Option<RiverWindowV1>,

    frames: Vec<Frame>,
    outputs: Vec<Output>,
    windows: Vec<Window>,
    prefixes: HashMap<XKBPrefix, BindingState>,
}

impl Reka {
    fn send(&self, cmd: ToEmacs) -> Result<()> {
        self.tx.send(cmd)?;
        signal::kill(Pid::this(), Some(signal::Signal::SIGUSR1))?;
        Ok(())
    }

    fn frame_by_output(&self, output: &RiverOutputV1) -> Option<&Frame> {
        self.frames
            .iter()
            .find(|f| f.displayed_on.is_some() && f.displayed_on.as_ref().unwrap().eq(output))
    }

    fn frame_by_name(&self, name: &str) -> Option<&Frame> {
        self.frames
            .iter()
            .find(|f| f.name.is_some() && f.name.as_ref().unwrap().eq(name))
    }

    fn window_by_proxy(&self, proxy: &RiverWindowV1) -> Option<&Window> {
        self.windows.iter().find(|w| proxy.eq(&w.window))
    }

    // reconcile_frames ensures that each output gets one maximized Emacs frame.
    fn reconcile_frames(&mut self) {
        'outputs: for output in &self.outputs {
            if let Some(f) = self.frame_by_output(&output.output) {
                f.window.propose_dimensions(output.width, output.height);
                continue;
            }

            // try to find an existing frame that is minimised and reuse it
            'frames: for f in &mut self.frames {
                if f.displayed_on.is_some() {
                    continue 'frames;
                }

                f.displayed_on = Some(output.output.clone());

                // always propose dimensions to keep frame sized to output
                f.window.propose_dimensions(output.width, output.height);
                f.window.inform_maximized();
                f.window.set_tiled(Edges::all());
                continue 'outputs;
            }

            // no frame found -> ask emacs for a new one
            self.send(ToEmacs::RequestFrame).unwrap();
        }
    }

    // reconcile_focus updates the seat's focus based on pending_focus (set by
    // events)
    fn reconcile_focus(&mut self) {
        let seat = match &mut self.seat {
            Some(s) => s,
            None => {
                log::warn!("no seat present, something might have gone wrong");
                return;
            }
        };

        // There might be no focus if the session is new, or a window was closed
        if seat.focus.is_none() && self.pending_focus.is_none() {
            log::debug!("recovering focus");
            if self.active_frame.is_some() {
                self.pending_focus = self.active_frame.clone()
            } else {
                // pick any displayed frame, we don't know which one is active ...
                self.pending_focus = self
                    .frames
                    .iter()
                    .find(|f| f.displayed_on.is_some())
                    .map(|f| f.window.clone());
            }
        }

        if let Some(window) = self.pending_focus.take() {
            if seat.focus.as_ref().map_or(true, |f| !f.eq(&window)) {
                seat.focus = Some(window.clone());
                seat.seat.focus_window(&window);

                // TODO: the frame/window split is getting annoying ...
                let is_frame = self.frames.iter().any(|f| f.window.eq(&window));
                if is_frame {
                    self.active_frame = Some(window);
                    return;
                }

                self.send(ToEmacs::Focused(window.clone()))
                    .expect("sending failed");

                if let Some(frame_window) = self
                    .window_by_proxy(&window)
                    .and_then(|w| w.params.as_ref())
                    .and_then(|p| self.frame_by_name(&p.frame_name))
                    .map(|f| f.window.clone())
                {
                    self.active_frame = Some(frame_window);
                }
            }
        }
    }

    // reconcile_windows closes killed windows and removes them from the list
    fn reconcile_windows(&mut self) {
        for w in self.windows.iter() {
            match &w.state {
                WindowState::Killed => {
                    log::info!("requesting window closure");
                    w.window.close();
                }
                WindowState::Starting => {
                    log::info!("ignoring starting window");
                }
                WindowState::Active => {
                    if let Some(params) = &w.params {
                        w.window.set_tiled(Edges::all());
                        w.window.propose_dimensions(params.w, params.h);
                    }
                }
            }
        }

        // TODO: force kill at some point
    }

    fn reconcile_bindings(&mut self) {
        let mut to_enable = vec![];
        {
            for (_, v) in self.prefixes.iter_mut() {
                if let BindingState::Registered(b) = v {
                    to_enable.push(b.clone());
                    *v = BindingState::Enabled(b.clone());
                }
            }
        };
        log::info!("enabling {} bindings", to_enable.len());
        for binding in to_enable.into_iter() {
            binding.enable();
        }
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
            } else if interface == "river_xkb_bindings_v1" {
                let xkb = proxy.bind::<RiverXkbBindingsV1, _, _>(name, 2, qhandle, ());
                state.xkb_bindings = Some(xkb);
                log::debug!("registering river xkb bindings ...");
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
            river::river_window_manager_v1::Event::ManageStart => {
                log::debug!("manage sequence started");

                state.reconcile_frames();
                state.reconcile_focus();
                state.reconcile_windows();
                state.reconcile_bindings();

                proxy.manage_finish();
            }

            // render sequence: positions, z-order, borders, visibility (?), clipping
            river::river_window_manager_v1::Event::RenderStart => {
                // reconcile frame display state
                for frame in state.frames.iter() {
                    match frame.displayed_on.as_ref() {
                        None => frame.window.hide(),
                        Some(output_proxy) => {
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

                for window in state.windows.iter() {
                    if let WindowState::Active = window.state {
                        if let Some(params) = &window.params {
                            // look up frame-relative output coordinates, but
                            // hide the window if no output is found (this
                            // occurs after outputs are disconnected but frames
                            // on them have remaining alive window parameters)
                            let output = state
                                .frame_by_name(&params.frame_name)
                                .and_then(|f| f.displayed_on.clone())
                                .and_then(|op| state.outputs.iter().find(|o| o.output == op));

                            if let Some(output) = output {
                                window.window.show();
                                window
                                    .node
                                    .set_position(params.x + output.x, params.y + output.y);
                                window.node.place_top();

                                // clip to actual content size, to get rid of unwanted decorations
                                let (clip_w, clip_h) =
                                    window.actual_width_height.unwrap_or((params.w, params.h));
                                window.window.set_clip_box(0, 0, clip_w, clip_h);
                            } else {
                                window.window.hide();
                            }
                        } else {
                            window.window.hide();
                        }
                    }
                }

                proxy.render_finish();
            }

            river::river_window_manager_v1::Event::Output { id } => {
                log::debug!("RiverWindowManagerV1::Event::Output received: id={:?}", id);
                state.outputs.push(Output {
                    output: id,
                    x: 0,
                    y: 0,
                    width: 0,
                    height: 0,
                });
            }

            river::river_window_manager_v1::Event::Seat { id } => {
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

            river::river_window_manager_v1::Event::Unavailable => {
                log::info!("RiverWindowManagerV1::Event::Unavailable received");
            }
            river::river_window_manager_v1::Event::Finished => {
                log::info!("RiverWindowManagerV1::Event::Finished received");
            }
            river::river_window_manager_v1::Event::SessionLocked => {
                log::info!("RiverWindowManagerV1::Event::SessionLocked received");
            }
            river::river_window_manager_v1::Event::SessionUnlocked => {
                log::info!("RiverWindowManagerV1::Event::SessionUnlocked received");
            }
            river::river_window_manager_v1::Event::Window { id } => {
                log::debug!("RiverWindowManagerV1::Event::Window received: id={:?}", id);
                let node = id.get_node(qhandle, ());
                state.windows.push(Window {
                    window: id,
                    node,
                    title: None,
                    pid: None,
                    state: WindowState::Starting,
                    params: None,
                    actual_width_height: None,
                });
            }
        }
    }

    wayland_client::event_created_child!(Reka, RiverWindowManagerV1, [
        river::river_window_manager_v1::EVT_WINDOW_OPCODE => (river::river_window_v1::RiverWindowV1, ()),
        river::river_window_manager_v1::EVT_OUTPUT_OPCODE => (river::river_output_v1::RiverOutputV1, ()),
        river::river_window_manager_v1::EVT_SEAT_OPCODE => (river::river_seat_v1::RiverSeatV1, ()),
    ]);
}

impl Dispatch<RiverOutputV1, ()> for Reka {
    fn event(
        state: &mut Self,
        proxy: &RiverOutputV1,
        event: <RiverOutputV1 as wayland_client::Proxy>::Event,
        _data: &(),
        _conn: &Connection,
        _qhandle: &wayland_client::QueueHandle<Self>,
    ) {
        log::debug!("RiverOutputV1 event received: {:?}", event);

        match event {
            river::river_output_v1::Event::Removed => {
                log::debug!("output disconnected, removing");
                for frame in &mut state.frames {
                    if let Some(o) = frame.displayed_on.as_ref()
                        && o.eq(proxy)
                    {
                        frame.displayed_on = None;
                    }
                }

                for (idx, output) in state.outputs.iter().enumerate() {
                    if &output.output == proxy {
                        state.outputs.remove(idx);
                        break;
                    }
                }
            }
            river::river_output_v1::Event::Position { x, y } => {
                log::debug!("output position: x={}, y={}", x, y);
                for output in state.outputs.iter_mut() {
                    if &output.output == proxy {
                        output.x = x;
                        output.y = y;
                        break;
                    }
                }
            }
            river::river_output_v1::Event::Dimensions { width, height } => {
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

impl Dispatch<river::river_seat_v1::RiverSeatV1, ()> for Reka {
    fn event(
        state: &mut Self,
        _proxy: &river::river_seat_v1::RiverSeatV1,
        event: <river::river_seat_v1::RiverSeatV1 as wayland_client::Proxy>::Event,
        _data: &(),
        _conn: &Connection,
        _qhandle: &wayland_client::QueueHandle<Self>,
    ) {
        log::debug!("RiverSeatV1 event received: {:?}", event);

        if let river::river_seat_v1::Event::WindowInteraction { window } = event {
            log::debug!("window interaction, setting pending focus: {:?}", window);
            state.pending_focus = Some(window);
        }
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
            river::river_window_v1::Event::UnreliablePid { unreliable_pid } => {
                if let Some(pos) = state.windows.iter().position(|w| &w.window == proxy) {
                    state.windows[pos].pid = Some(unreliable_pid);

                    if unreliable_pid == state.pid {
                        log::info!("discovered new Emacs frame ...");
                        let window = state.windows.remove(pos);
                        state.frames.push(Frame {
                            name: None,
                            displayed_on: None,
                            node: window.node,
                            window: window.window,
                        });
                    } else {
                        log::info!("discovered new window (PID: {}) ...", unreliable_pid);
                        if let Err(err) = state.send(ToEmacs::NewWindow(proxy.clone())) {
                            log::error!("failed to send new window command to Emacs: {}", err);
                        }
                    }
                }
            }
            river::river_window_v1::Event::Title { title } => {
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

                if let Some(title) = title.as_ref() {
                    state
                        .send(ToEmacs::TitleChange(proxy.clone(), title.clone()))
                        .unwrap();
                }

                for w in state.windows.iter_mut() {
                    if w.window.eq(proxy) {
                        w.title = title;
                        return;
                    }
                }

                log::warn!("received title for unknown window, orphan frame?");
            }
            river::river_window_v1::Event::Dimensions { width, height } => {
                for w in state.windows.iter_mut() {
                    if w.window.eq(proxy) {
                        w.actual_width_height = Some((width, height));
                        return;
                    }
                }
            }
            river::river_window_v1::Event::Closed => {
                state
                    .send(ToEmacs::WindowClosed(proxy.clone()))
                    .expect("could not signal emacs"); // TODO: fix the error handling for all of these
                if let Some(seat) = &mut state.seat {
                    if let Some(focus) = &seat.focus {
                        if focus.eq(proxy) {
                            // manage sequence immediately after will recover focus
                            seat.focus = None;
                        }
                    }
                }

                {
                    for (i, w) in state.windows.iter().enumerate() {
                        if w.window.eq(proxy) {
                            state.windows.swap_remove(i);
                            return;
                        }
                    }
                }

                for (i, f) in state.frames.iter().enumerate() {
                    if f.window.eq(proxy) {
                        state.frames.swap_remove(i);
                        return;
                    }
                }
            }
            _ => {}
        }
    }
}

impl Dispatch<river::river_node_v1::RiverNodeV1, ()> for Reka {
    fn event(
        _state: &mut Self,
        _proxy: &river::river_node_v1::RiverNodeV1,
        event: <river::river_node_v1::RiverNodeV1 as wayland_client::Proxy>::Event,
        _data: &(),
        _conn: &Connection,
        _qhandle: &wayland_client::QueueHandle<Self>,
    ) {
        log::debug!("RiverNodeV1 event received: {:?}", event);
    }
}

impl Dispatch<river::river_xkb_bindings_v1::RiverXkbBindingsV1, ()> for Reka {
    fn event(
        _state: &mut Self,
        _proxy: &river::river_xkb_bindings_v1::RiverXkbBindingsV1,
        event: <river::river_xkb_bindings_v1::RiverXkbBindingsV1 as wayland_client::Proxy>::Event,
        _data: &(),
        _conn: &Connection,
        _qhandle: &wayland_client::QueueHandle<Self>,
    ) {
        log::debug!("RiverXkbBindingsV1 event received: {:?}", event);
    }
}

impl Dispatch<river::river_xkb_binding_v1::RiverXkbBindingV1, ()> for Reka {
    fn event(
        state: &mut Self,
        proxy: &river::river_xkb_binding_v1::RiverXkbBindingV1,
        event: <river::river_xkb_binding_v1::RiverXkbBindingV1 as wayland_client::Proxy>::Event,
        _data: &(),
        _conn: &Connection,
        _qhandle: &wayland_client::QueueHandle<Self>,
    ) {
        log::debug!("RiverXkbBindingV1 event received: {:?}", event);
        if matches!(event, river::river_xkb_binding_v1::Event::Pressed) {
            for (k, v) in state.prefixes.iter() {
                if let BindingState::Enabled(this) = v
                    && this.eq(proxy)
                {
                    if let Err(err) = state.send(ToEmacs::KeyEvent(k.event)) {
                        log::error!("failed to send key event to Emacs: {}", err);
                    }
                    state.pending_focus = state
                        .active_frame
                        .clone()
                        .or_else(|| state.frames.first().map(|f| f.window.clone()));
                    break;
                }
            }
        }
    }
}

impl Dispatch<river::river_xkb_bindings_seat_v1::RiverXkbBindingsSeatV1, ()> for Reka {
    fn event(
        _state: &mut Self,
        _proxy: &river::river_xkb_bindings_seat_v1::RiverXkbBindingsSeatV1,
        event: <river::river_xkb_bindings_seat_v1::RiverXkbBindingsSeatV1 as wayland_client::Proxy>::Event,
        _data: &(),
        _conn: &Connection,
        _qhandle: &wayland_client::QueueHandle<Self>,
    ) {
        log::debug!("RiverXkbBindingsSeatV1 event received: {:?}", event);
    }
}
