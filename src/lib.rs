// SPDX-License-Identifier: GPL-3.0-or-later

use std::cell::RefCell;
use std::collections::HashMap;
use std::fs::File;
use std::io::Write;
use std::os::fd::{AsFd, FromRawFd};
use std::sync::Arc;
use std::sync::mpsc::{Receiver, Sender, channel};

use anyhow::{Context, anyhow};
use emacs::{Env, FromLisp, IntoLisp, Result, Value, Vector, defun, use_symbols};
use nix::poll::PollTimeout;
use nix::sys::eventfd::{EfdFlags, EventFd};
use wayland_client::{Connection, Dispatch, protocol::wl_registry};
use xkbcommon::xkb;

use crate::river::{
    river_layer_shell_output_v1::RiverLayerShellOutputV1,
    river_layer_shell_seat_v1::RiverLayerShellSeatV1,
    river_layer_shell_v1::RiverLayerShellV1,
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

        pub(super) mod layer_shell {
            use super::wm::*;
            wayland_scanner::generate_interfaces!("./protocol/river-layer-shell-v1.xml");
        }
    }

    use self::interfaces::layer_shell::*;
    use self::interfaces::wm::*;
    use self::interfaces::xkb::*;
    wayland_scanner::generate_client_code!("./protocol/river-window-management-v1.xml");
    wayland_scanner::generate_client_code!("./protocol/river-xkb-bindings-v1.xml");
    wayland_scanner::generate_client_code!("./protocol/river-layer-shell-v1.xml");
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
    app_id_change
    frame_request
    discard_frame
    toggle_fullscreen
    reka_get_window => "reka--get-window"
    reka_create_buffer => "reka--create-buffer"
    reka_list_buffers => "reka--list-buffers"
);

macro_rules! dispatch_log_only {
    ($proxy_ty:ty) => {
        impl Dispatch<$proxy_ty, ()> for Reka {
            fn event(
                _: &mut Self,
                _: &$proxy_ty,
                event: <$proxy_ty as wayland_client::Proxy>::Event,
                _: &(),
                _: &Connection,
                _: &wayland_client::QueueHandle<Self>,
            ) {
                log::debug!("{} event: {:?}", stringify!($proxy_ty), event);
            }
        }
    };
}

#[emacs::module(name = "libreka", defun_prefix = "reka")]
fn init(_: &Env) -> Result<()> {
    Ok(())
}

#[derive(Debug)]
enum Command {
    /// Always forward the binding to Emacs
    Forward,

    /// Toggle fullscreen for the selected window
    ToggleFullscreen,
}

impl<'e> FromLisp<'e> for Command {
    fn from_lisp(value: Value<'e>) -> Result<Self> {
        if value.is_not_nil() {
            if toggle_fullscreen.eq(&value) {
                return Ok(Command::ToggleFullscreen);
            }

            return Err(anyhow!("unknown builtin command"));
        }

        return Ok(Command::Forward);
    }
}

#[derive(Debug)]
enum FromEmacs {
    RegisterPrefix(XKBPrefix, Command),
    FocusWindow(RiverWindowV1),
    FocusFrame,
    CloseWindow(RiverWindowV1),
    BufferCreated(RiverWindowV1),
    UpdateParameters(HashMap<RiverWindowV1, WindowParameters>),
}

#[derive(Debug)]
enum ToEmacs {
    KeyEvent(u32),
    NewWindow(RiverWindowV1),
    WindowClosed(RiverWindowV1),
    Focused(RiverWindowV1),
    TitleChange(RiverWindowV1, String),
    AppIDChange(RiverWindowV1, String),
    RequestFrame,
    DiscardFrame(String),
}

impl<'e> IntoLisp<'e> for ToEmacs {
    fn into_lisp(self, env: &'e Env) -> Result<Value<'e>> {
        match self {
            ToEmacs::KeyEvent(event) => env.call(cons, (key_event, event)),
            ToEmacs::NewWindow(win) => env.call(cons, (new_window, RefCell::new(win))),
            ToEmacs::WindowClosed(win) => env.call(cons, (window_closed, RefCell::new(win))),
            ToEmacs::Focused(win) => env.call(cons, (focused, RefCell::new(win))),
            ToEmacs::AppIDChange(win, app_id) => {
                env.call(list, (app_id_change, RefCell::new(win), app_id))
            }
            ToEmacs::TitleChange(win, title) => {
                env.call(list, (title_change, RefCell::new(win), title))
            }
            ToEmacs::RequestFrame => frame_request.into_lisp(env),
            ToEmacs::DiscardFrame(frame_name) => env.call(cons, (discard_frame, frame_name)),
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
fn start_wm(env: &Env, pipe: Value<'_>) -> Result<Handle> {
    let channel_fd_i32 = env.open_channel(pipe)?;
    let channel_file = unsafe {
        // SAFETY: I hope Emacs works correctly!
        File::from_raw_fd(channel_fd_i32)
    };

    let (tx, rx) = channel::<FromEmacs>();
    let (tx_e, rx_e) = channel::<ToEmacs>();

    let emacs_fd = Arc::new(EventFd::from_value_and_flags(0, EfdFlags::EFD_NONBLOCK)?);
    let emacs_fd_wmside = emacs_fd.clone();

    std::thread::spawn(move || {
        let result = wm_loop(rx, tx_e, channel_file, emacs_fd_wmside);
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

    Ok(params)
}

#[defun]
fn update_window_parameters<'e>(
    env: &'e Env,
    handle: &Handle,
    per_frame: Vector<'e>,
) -> Result<Value<'e>> {
    let mut new_params = HashMap::new();
    for elem in per_frame.into_iter() {
        let params: Vector<'e> = elem.into_rust()?;
        for p in params.into_iter() {
            let wp: &RefCell<WindowParameters> = p.into_rust()?;
            let params = wp.borrow().clone();
            new_params.insert(params.window.clone(), params);
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
    command: Command,
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
        return Err(anyhow!("could not resolve XKB keysym for key").into());
    }

    let modifiers = Modifiers::from_bits(modifiers_bits).context("unknown modifier bits")?;
    let prefix = XKBPrefix {
        event,
        keysym: u32::from(keysym),
        modifiers,
    };
    handle.send(FromEmacs::RegisterPrefix(prefix, command))?;

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

fn wm_loop(
    rx: Receiver<FromEmacs>,
    tx: Sender<ToEmacs>,
    to_emacs: File,
    emacs_fd: Arc<EventFd>,
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
        rx,
        tx,
        to_emacs,
        from_emacs: emacs_fd,
        seat: None,
        focus: Focus {
            dirty: false,
            state: FocusState::Lost,
        },
        frames: HashMap::new(),
        outputs: HashMap::new(),
        windows: HashMap::new(),
        river_wm: None,
        xkb_bindings: None,
        layer_shell: None,
        prefixes: Default::default(),
        pending_frames: 0,
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
                nix::poll::PollFd::new(wm.from_emacs.as_fd(), nix::poll::PollFlags::POLLIN),
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
            wm.handle_emacs(&qh);
        }

        if river_ready {
            log::debug!("river has events ready");
            guard.read()?;
            event_queue.dispatch_pending(&mut wm)?;
        }
    }
}

enum FullscreenState {
    None,
    Requested {
        new: RiverWindowV1,
        previous: Option<RiverWindowV1>,
    },
    Fullscreen(RiverWindowV1),
    Exiting(RiverWindowV1),
}

struct Output {
    proxy: RiverOutputV1,
    ls_output: RiverLayerShellOutputV1,
    x: i32,
    y: i32,
    width: i32,
    height: i32,
    fullscreen: FullscreenState,
}

impl Drop for Output {
    fn drop(&mut self) {
        self.proxy.destroy();
        self.ls_output.destroy();
    }
}

struct Frame {
    name: Option<String>,
    displayed_on: Option<RiverOutputV1>,
    node: river::river_node_v1::RiverNodeV1,
    proxy: RiverWindowV1,
}

impl Drop for Frame {
    fn drop(&mut self) {
        // do NOT destroy displayed_on, output drop handles this
        self.proxy.destroy();
        self.node.destroy();
    }
}

#[derive(Debug, PartialEq)]
enum WindowState {
    Starting,
    Active,
    Killed,
}

#[derive(Debug)]
struct Window {
    proxy: RiverWindowV1,
    node: river::river_node_v1::RiverNodeV1,
    state: WindowState,
    params: Option<WindowParameters>,
    actual_width_height: Option<(i32, i32)>,
}

impl Drop for Window {
    fn drop(&mut self) {
        self.proxy.destroy();
        self.node.destroy();
    }
}

struct Seat {
    proxy: river::river_seat_v1::RiverSeatV1,
    #[allow(dead_code)] // TODO(tazjin): do I *need* to store this?
    ls_seat: RiverLayerShellSeatV1,
}

impl Drop for Seat {
    fn drop(&mut self) {
        self.proxy.destroy();
        self.ls_seat.destroy();
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
struct XKBPrefix {
    event: u32,
    keysym: u32,
    modifiers: Modifiers,
}

#[derive(Debug)]
enum BindingState {
    Requested,
    Registered(RiverXkbBindingV1),
    Enabled(RiverXkbBindingV1),
}

impl Drop for BindingState {
    fn drop(&mut self) {
        if let BindingState::Enabled(binding) = self {
            binding.destroy();
        }
    }
}

#[derive(Debug)]
struct Binding {
    #[allow(dead_code)]
    command: Command,
    state: BindingState,
}

enum FocusState {
    Lost,
    Frame(RiverWindowV1),
    Window {
        window: RiverWindowV1,
        on_frame: RiverWindowV1,
    },
}

struct Focus {
    dirty: bool,
    state: FocusState,
}

impl Focus {
    fn mark_dirty(&mut self) {
        self.dirty = true;
    }

    fn is_dirty(&mut self) -> bool {
        std::mem::replace(&mut self.dirty, false)
    }

    fn lost_focus(&self) -> bool {
        matches!(self.state, FocusState::Lost)
    }

    fn current_focus_and_frame(&self) -> Option<(&RiverWindowV1, &RiverWindowV1)> {
        match &self.state {
            FocusState::Lost => None,
            FocusState::Frame(frame) => Some((frame, frame)),
            FocusState::Window { window, on_frame } => Some((window, on_frame)),
        }
    }

    /// invalidate a window, meaning it can not be focused anymore (e.g. was closed)
    fn invalidate(&mut self, target: &RiverWindowV1) {
        match &self.state {
            FocusState::Frame(w) | FocusState::Window { on_frame: w, .. } if w.eq(target) => {
                self.state = FocusState::Lost;
            }
            FocusState::Window { window, on_frame } if window.eq(target) => {
                self.state = FocusState::Frame(on_frame.clone());
            }
            _ => {}
        }
    }

    fn focus_window(&mut self, window: RiverWindowV1, on_frame: RiverWindowV1) {
        self.state = FocusState::Window { window, on_frame };
    }

    fn focus_frame(&mut self, frame: RiverWindowV1) {
        self.state = FocusState::Frame(frame);
    }

    fn switch_to_frame(&mut self) {
        if let FocusState::Window { on_frame, .. } = &self.state {
            self.focus_frame(on_frame.clone());
        }
    }
}

struct Reka {
    pid: i32,

    // Emacs-related state
    rx: Receiver<FromEmacs>,
    tx: Sender<ToEmacs>,
    to_emacs: File,
    from_emacs: Arc<EventFd>,
    pending_frames: usize,

    // river-related state
    river_wm: Option<RiverWindowManagerV1>,
    xkb_bindings: Option<RiverXkbBindingsV1>,
    layer_shell: Option<RiverLayerShellV1>,

    seat: Option<Seat>,
    focus: Focus,

    frames: HashMap<RiverWindowV1, Frame>,
    outputs: HashMap<RiverOutputV1, Output>,
    windows: HashMap<RiverWindowV1, Window>,
    prefixes: HashMap<XKBPrefix, Binding>,
}

impl Drop for Reka {
    fn drop(&mut self) {
        // Destroy everything else before tearing down the WM objects.
        self.prefixes.clear();
        self.windows.clear();
        self.frames.clear();
        self.outputs.clear();
        self.seat.take();

        if let Some(bindings) = self.xkb_bindings.take() {
            bindings.destroy();
        }

        if let Some(layer_shell) = self.layer_shell.take() {
            layer_shell.destroy();
        }

        if let Some(wm) = self.river_wm.take() {
            wm.destroy();
        }
    }
}

impl Reka {
    fn send(&mut self, cmd: ToEmacs) {
        if let Err(err) = self.tx.send(cmd) {
            log::error!("could not send message to Emacs: {}", err);
        }
        if let Err(err) = self.to_emacs.write_all(&[1]) {
            log::error!("could not notify Emacs: {}", err);
        }
    }

    fn frame_by_output(&self, output: &RiverOutputV1) -> Option<&Frame> {
        self.frames
            .iter()
            .find(|(_, f)| f.displayed_on.as_ref() == Some(output))
            .map(|x| x.1)
    }

    fn frame_by_name(&self, name: &str) -> Option<&Frame> {
        self.frames
            .iter()
            .find(|(_, f)| f.name.as_deref() == Some(name))
            .map(|x| x.1)
    }

    fn displaying_frame(&self, window: &RiverWindowV1) -> Option<&Frame> {
        self.windows
            .get(window)
            .and_then(|w| w.params.as_ref())
            .and_then(|p| self.frame_by_name(&p.frame_name))
    }

    fn binding_by_proxy(&self, proxy: &RiverXkbBindingV1) -> Option<(&XKBPrefix, &Binding)> {
        self.prefixes.iter().find(|(_, v)| {
            if let BindingState::Enabled(this) = &v.state {
                return this.eq(proxy);
            } else {
                false
            }
        })
    }

    // reconcile_frames ensures that each output gets one maximized Emacs frame.
    fn reconcile_frames(&mut self) {
        let mut frame_requests: usize = 0;

        'outputs: for (output_proxy, output) in &self.outputs {
            if let Some(f) = self.frame_by_output(output_proxy) {
                f.proxy.propose_dimensions(output.width, output.height);
                continue;
            }

            // try to find an existing frame that is minimised and reuse it
            for f in self.frames.values_mut() {
                if f.displayed_on.is_some() {
                    continue;
                }

                f.displayed_on = Some(output_proxy.clone());

                // always propose dimensions to keep frame sized to output
                f.proxy.propose_dimensions(output.width, output.height);
                f.proxy.inform_maximized();
                f.proxy.set_tiled(Edges::all());
                continue 'outputs;
            }

            // no frame found -> might need a new frame
            frame_requests += 1;
        }

        // Request new frames (if we haven't already fired a request). This
        // accounting is necessary because there might be additional manage
        // sequences before a frame shows up.
        if frame_requests > self.pending_frames {
            for _ in 0..(frame_requests - self.pending_frames) {
                self.send(ToEmacs::RequestFrame);
                self.pending_frames += 1;
            }
        }
    }

    // reconcile_focus updates the seat's focus based on pending_focus (set by
    // events)
    fn reconcile_focus(&mut self) {
        let Some(seat) = &mut self.seat else {
            log::warn!("no seat present, something might have gone wrong");
            return;
        };

        if self.focus.lost_focus() {
            // recover focus by focusing *any* displayed frame
            let Some(frame) = self
                .frames
                .iter()
                .find(|(_, f)| f.displayed_on.is_some())
                .map(|(proxy, _)| proxy.clone())
            else {
                log::error!("no displayed frames -> can not focus anything!");
                return;
            };

            self.focus.focus_frame(frame);
        }

        let is_dirty = self.focus.is_dirty();
        let (target, frame) = self.focus.current_focus_and_frame().unwrap();

        seat.proxy.focus_window(target);

        if is_dirty {
            let output = self.frames[frame]
                .displayed_on
                .as_ref()
                .expect("reka bug: inactive frame focused");
            self.outputs[output].ls_output.set_default();

            // don't notify emacs about focused frames
            if target != frame {
                self.send(ToEmacs::Focused(target.clone()));
            }
        }
    }

    // reconcile_windows closes killed windows and removes them from the list
    fn reconcile_windows(&mut self) {
        for (proxy, w) in self.windows.iter() {
            match &w.state {
                WindowState::Killed => {
                    log::info!("requesting window closure");
                    proxy.close();
                }
                WindowState::Starting => {
                    log::info!("ignoring starting window");
                }
                WindowState::Active => {
                    if let Some(params) = &w.params {
                        proxy.set_tiled(Edges::all());
                        proxy.propose_dimensions(params.w, params.h);
                    }
                }
            }
        }

        // TODO: force kill at some point
    }

    fn reconcile_bindings(&mut self) {
        let mut to_enable = vec![];
        {
            for (prefix, v) in self.prefixes.iter_mut() {
                if let BindingState::Registered(b) = &v.state {
                    to_enable.push(b.clone());
                    v.state = BindingState::Enabled(b.clone());
                    log::info!("enabling new XKB binding (event={})", prefix.event);
                }
            }
        };

        for binding in to_enable.into_iter() {
            binding.enable();
        }
    }

    fn reconcile_fullscreen(&mut self) {
        for output in self.outputs.values_mut() {
            match &output.fullscreen {
                FullscreenState::Requested { new, previous } => {
                    if let Some(previous) = previous {
                        previous.inform_not_fullscreen();
                        previous.exit_fullscreen();
                    }

                    new.inform_fullscreen();
                    new.fullscreen(&output.proxy);
                    output.fullscreen = FullscreenState::Fullscreen(new.clone());
                }
                FullscreenState::Exiting(win) => {
                    win.inform_not_fullscreen();
                    win.exit_fullscreen();
                    output.fullscreen = FullscreenState::None;
                }
                _ => {}
            }
        }
    }

    fn handle_emacs(&mut self, qh: &wayland_client::QueueHandle<Self>) {
        log::debug!("emacs signalled available data");
        let mut buf = [0u8; 8];
        let _ = nix::unistd::read(&self.from_emacs, &mut buf);

        let mut needs_manage = false;
        while let Ok(message) = self.rx.try_recv() {
            log::debug!("message from emacs: {:?}", message);
            match message {
                FromEmacs::RegisterPrefix(prefix, command) => {
                    needs_manage = true;
                    log::debug!("registering new prefix with command {:?}", command);
                    match self.prefixes.entry(prefix) {
                        std::collections::hash_map::Entry::Occupied(mut entry) => {
                            entry.get_mut().command = command;
                        }
                        std::collections::hash_map::Entry::Vacant(entry) => {
                            entry.insert(Binding {
                                command,
                                state: BindingState::Requested,
                            });
                        }
                    }
                }
                FromEmacs::FocusWindow(window) => {
                    needs_manage = true;
                    let Some(frame) = self.displaying_frame(&window) else {
                        log::error!("can not focus window that is not displayed!");
                        continue;
                    };
                    self.focus.focus_window(window, frame.proxy.clone());
                }
                FromEmacs::FocusFrame => {
                    // TODO: explicit selection?
                    needs_manage = true;
                    self.focus.switch_to_frame();
                }
                FromEmacs::CloseWindow(window) => {
                    if let Some(w) = self.windows.get_mut(&window) {
                        w.state = WindowState::Killed;
                        needs_manage = true;
                    }
                }
                FromEmacs::BufferCreated(window) => {
                    if let Some(w) = self.windows.get_mut(&window) {
                        needs_manage = true;
                        w.state = WindowState::Active;
                    }
                }
                FromEmacs::UpdateParameters(mut new_params) => {
                    for (proxy, w) in self.windows.iter_mut() {
                        let result = new_params.remove(proxy);
                        if w.params != result {
                            w.params = result;
                            needs_manage = true;
                        }
                    }
                }
            }
        }

        // All commands from Emacs need a manage sequence.
        if let Some(river_wm) = &self.river_wm
            && needs_manage
        {
            river_wm.manage_dirty();
        }

        let register_prefixes = self
            .prefixes
            .iter()
            .filter_map(|(k, v)| match v.state {
                BindingState::Requested => Some(*k),
                _ => None,
            })
            .collect::<Vec<_>>();

        if !register_prefixes.is_empty() && self.xkb_bindings.is_some() && self.seat.is_some() {
            let xkb = self.xkb_bindings.as_ref().unwrap();
            let seat = self.seat.as_ref().unwrap().proxy.clone();
            let mut bindings = vec![];

            for p in &register_prefixes {
                let b = xkb.get_xkb_binding(&seat, p.keysym, p.modifiers, &qh, ());
                bindings.push(b);
            }

            for (p, b) in register_prefixes.into_iter().zip(bindings.into_iter()) {
                let binding = self
                    .prefixes
                    .get_mut(&p)
                    .expect("binding magically disappeared");
                binding.state = BindingState::Registered(b);
            }
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
                let wm = proxy.bind::<RiverWindowManagerV1, _, _>(name, 4, qhandle, ());
                state.river_wm = Some(wm);
                log::debug!("registering river window manager ...");
            } else if interface == "river_xkb_bindings_v1" {
                let xkb = proxy.bind::<RiverXkbBindingsV1, _, _>(name, 2, qhandle, ());
                state.xkb_bindings = Some(xkb);
                log::debug!("registering river xkb bindings ...");
            } else if interface == "river_layer_shell_v1" {
                let layer_shell = proxy.bind::<RiverLayerShellV1, _, _>(name, 1, qhandle, ());
                state.layer_shell = Some(layer_shell);
                log::debug!("registering river layer shell ... ");
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
                state.reconcile_windows();
                state.reconcile_bindings();
                state.reconcile_fullscreen();
                state.reconcile_focus();

                proxy.manage_finish();
            }

            // render sequence: positions, z-order, borders, visibility (?), clipping
            river::river_window_manager_v1::Event::RenderStart => {
                // reconcile frame display state
                for (frame_proxy, frame) in state.frames.iter() {
                    match frame.displayed_on.as_ref() {
                        None => frame_proxy.hide(),
                        Some(output_proxy) => {
                            frame_proxy.show();
                            frame.node.place_bottom();

                            // position frame at its output's origin
                            if let Some(output) = state.outputs.get(output_proxy) {
                                frame.node.set_position(output.x, output.y);
                            }
                        }
                    }
                }

                for (win_proxy, window) in state.windows.iter() {
                    if let WindowState::Active = window.state {
                        if let Some(params) = &window.params {
                            // look up frame-relative output coordinates, but
                            // hide the window if no output is found (this
                            // occurs after outputs are disconnected but frames
                            // on them have remaining alive window parameters)
                            let output = state
                                .frame_by_name(&params.frame_name)
                                .and_then(|f| f.displayed_on.as_ref())
                                .and_then(|op| state.outputs.get(op));

                            if let Some(output) = output {
                                win_proxy.show();
                                window
                                    .node
                                    .set_position(params.x + output.x, params.y + output.y);
                                window.node.place_top();

                                // clip to actual content size, to get rid of unwanted decorations
                                let (clip_w, clip_h) =
                                    window.actual_width_height.unwrap_or((params.w, params.h));
                                win_proxy.set_clip_box(0, 0, clip_w, clip_h);
                            } else {
                                win_proxy.hide();
                            }
                        } else {
                            win_proxy.hide();
                        }
                    }
                }

                proxy.render_finish();
            }

            river::river_window_manager_v1::Event::Output { id } => {
                log::debug!("RiverWindowManagerV1::Event::Output received: id={:?}", id);
                let ls_output = state
                    .layer_shell
                    .as_ref()
                    .expect("layer shell not registered")
                    .get_output(&id, &qhandle, ());

                state.outputs.insert(
                    id.clone(),
                    Output {
                        proxy: id,
                        ls_output,
                        x: 0,
                        y: 0,
                        width: 0,
                        height: 0,
                        fullscreen: FullscreenState::None,
                    },
                );
            }

            river::river_window_manager_v1::Event::Seat { id } => {
                log::debug!("RiverWindowManagerV1::Event::Seat received: id={:?}", id);
                let ls_seat = state
                    .layer_shell
                    .as_ref()
                    .expect("layer shell not bound")
                    .get_seat(&id, qhandle, ());
                if state.seat.is_none() {
                    state.seat = Some(Seat { proxy: id, ls_seat });
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
                // TODO: do we still need to recover focus here?
            }
            river::river_window_manager_v1::Event::Window { id } => {
                log::debug!("RiverWindowManagerV1::Event::Window received: id={:?}", id);
                // *not* creating the window object here to avoid Drop complications
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
                log::info!("an output was disconnected, removing");
                let Some(output) = state.outputs.remove(proxy) else {
                    log::warn!("unknown output disconnected");
                    return;
                };

                let Some(frame_name) = state
                    .frame_by_output(&output.proxy)
                    .and_then(|f| f.name.clone())
                else {
                    log::warn!("disconnected output had no frame");
                    return;
                };

                state.send(ToEmacs::DiscardFrame(frame_name));
            }
            river::river_output_v1::Event::Position { x, y } => {
                log::debug!("output position: x={}, y={}", x, y);
                if let Some(output) = state.outputs.get_mut(proxy) {
                    output.x = x;
                    output.y = y;
                }
            }
            river::river_output_v1::Event::Dimensions { width, height } => {
                log::debug!("output dimensions: {}x{}", width, height);
                if let Some(output) = state.outputs.get_mut(proxy) {
                    output.width = width;
                    output.height = height;
                }
            }
            _ => {}
        }
    }
}

impl Dispatch<RiverLayerShellOutputV1, ()> for Reka {
    fn event(
        state: &mut Self,
        proxy: &RiverLayerShellOutputV1,
        event: <RiverLayerShellOutputV1 as wayland_client::Proxy>::Event,
        _: &(),
        _: &Connection,
        _: &wayland_client::QueueHandle<Self>,
    ) {
        match event {
            river::river_layer_shell_output_v1::Event::NonExclusiveArea {
                x,
                y,
                width,
                height,
            } => {
                let Some(output_proxy) = state
                    .outputs
                    .iter()
                    .find(|(_, o)| o.ls_output.eq(proxy))
                    .map(|(p, _)| p.clone())
                else {
                    log::warn!("could not find matching output for layer-shell output");
                    return;
                };

                if let Some(output) = state.outputs.get_mut(&output_proxy) {
                    output.x = x;
                    output.y = y;
                    output.width = width;
                    output.height = height;
                }
            }
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
            log::debug!("window interaction, setting focus: {:?}", window);
            if state.frames.contains_key(&window) {
                state.focus.focus_frame(window);
                return;
            }

            let Some(frame) = state.displaying_frame(&window) else {
                log::error!("received window interaction for window without a frame");
                return;
            };

            state.focus.focus_window(window, frame.proxy.clone());
            state.focus.mark_dirty();
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
        qhandle: &wayland_client::QueueHandle<Self>,
    ) {
        log::debug!("RiverWindowV1 event received: {:?}", event);

        match event {
            river::river_window_v1::Event::UnreliablePid { unreliable_pid } => {
                let node = proxy.get_node(qhandle, ());

                if unreliable_pid == state.pid {
                    log::info!("discovered new Emacs frame ...");
                    state.frames.insert(
                        proxy.clone(),
                        Frame {
                            name: None,
                            displayed_on: None,
                            proxy: proxy.clone(),
                            node,
                        },
                    );

                    if state.pending_frames > 0 {
                        state.pending_frames -= 1;
                    } else {
                        // users might (accidentally?) create new frames
                        log::warn!("new frame was not requested by WM");
                    }
                    return;
                }

                log::info!("discovered new window (PID: {}) ...", unreliable_pid);
                state.send(ToEmacs::NewWindow(proxy.clone()));

                state.windows.insert(
                    proxy.clone(),
                    Window {
                        proxy: proxy.clone(),
                        node,
                        state: WindowState::Starting,
                        params: None,
                        actual_width_height: None,
                    },
                );
            }
            river::river_window_v1::Event::AppId {
                app_id: Some(app_id),
            } => {
                if app_id.is_empty() {
                    return;
                }

                state.send(ToEmacs::AppIDChange(proxy.clone(), app_id));
            }
            river::river_window_v1::Event::Title { title: Some(title) } => {
                if title.is_empty() {
                    return;
                }

                if title.starts_with("reka-frame-") {
                    if let Some(f) = state.frames.get_mut(proxy) {
                        f.name = Some(title);
                        return;
                    }
                }

                state.send(ToEmacs::TitleChange(proxy.clone(), title.clone()));

                log::warn!("received title for unknown window, orphan frame?");
            }
            river::river_window_v1::Event::Dimensions { width, height } => {
                if let Some(w) = state.windows.get_mut(proxy) {
                    w.actual_width_height = Some((width, height));
                }
            }
            river::river_window_v1::Event::Closed => {
                state.send(ToEmacs::WindowClosed(proxy.clone()));
                state.focus.invalidate(proxy);

                for output in state.outputs.values_mut() {
                    match &output.fullscreen {
                        FullscreenState::Requested { new: win, .. }
                        | FullscreenState::Fullscreen(win)
                        | FullscreenState::Exiting(win)
                            if proxy.eq(win) =>
                        {
                            output.fullscreen = FullscreenState::None;
                        }
                        _ => {}
                    }
                }

                if state.windows.remove(proxy).is_some() {
                    return;
                }

                if state.frames.remove(proxy).is_none() {
                    log::warn!("unknown window closed; reka bug?");
                }
            }
            river::river_window_v1::Event::FullscreenRequested { output: target } => {
                // one of the denser pieces of logic here ... figure out which output to fullscreen on:
                // 1. the requested output
                // 2. the output the window is already on
                // 3. the active output
                let potential_target = target
                    .or_else(|| {
                        state
                            .windows
                            .get(proxy)
                            .and_then(|w| w.params.as_ref())
                            .and_then(|p| state.frame_by_name(&p.frame_name))
                            .and_then(|f| f.displayed_on.clone())
                    })
                    .or_else(|| {
                        state
                            .focus
                            .current_focus_and_frame()
                            .and_then(|(_, fw)| state.frames.get(fw))
                            .and_then(|f| f.displayed_on.clone())
                    });
                let Some(target) = potential_target else {
                    log::warn!("fullscreen requested, but could not find matching output!");
                    return;
                };

                if let Some(output) = state.outputs.get_mut(&target) {
                    let previous = match &output.fullscreen {
                        FullscreenState::Fullscreen(window) | FullscreenState::Exiting(window) => {
                            Some(window.clone())
                        }
                        _ => None,
                    };

                    log::debug!("requesting fullscreen state");
                    output.fullscreen = FullscreenState::Requested {
                        new: proxy.clone(),
                        previous,
                    };
                }
            }

            river::river_window_v1::Event::ExitFullscreenRequested => {
                for output in state.outputs.values_mut() {
                    match &output.fullscreen {
                        FullscreenState::Requested { new: current, .. }
                        | FullscreenState::Fullscreen(current)
                            if current.eq(proxy) =>
                        {
                            log::debug!("exiting fullscreen as per request");
                            output.fullscreen = FullscreenState::Exiting(current.clone());
                            return;
                        }
                        _ => {}
                    };
                }
            }
            _ => {}
        }
    }
}

impl Dispatch<RiverXkbBindingV1, ()> for Reka {
    fn event(
        state: &mut Self,
        proxy: &RiverXkbBindingV1,
        event: <RiverXkbBindingV1 as wayland_client::Proxy>::Event,
        _data: &(),
        _conn: &Connection,
        _qhandle: &wayland_client::QueueHandle<Self>,
    ) {
        log::debug!("current known bindings: {:?}", state.prefixes);
        log::debug!("RiverXkbBindingV1 event received: {:?}", event);
        if matches!(event, river::river_xkb_binding_v1::Event::Pressed) {
            let Some((prefix, binding)) = state.binding_by_proxy(proxy) else {
                log::warn!("received keypress event for unknown binding, reka bug?");
                return;
            };

            match binding.command {
                Command::Forward => {
                    state.send(ToEmacs::KeyEvent(prefix.event));
                    state.focus.switch_to_frame();
                }
                Command::ToggleFullscreen => {
                    let Some((focus, frame)) = state.focus.current_focus_and_frame() else {
                        log::warn!("fullscreen requested, but nothing is focused. reka bug?");
                        return;
                    };

                    if focus == frame {
                        // can't make the frame even more fullscreen! maybe the
                        // user meant something else? let emacs handle it ..
                        state.send(ToEmacs::KeyEvent(prefix.event));
                        return;
                    }

                    let Some(output) = &state.frames[frame].displayed_on else {
                        log::warn!("selected frame for fullscreen is not displayed. reka bug?");
                        return;
                    };

                    let new_state = match &state.outputs[output].fullscreen {
                        FullscreenState::None => FullscreenState::Requested {
                            new: focus.clone(),
                            previous: None,
                        },
                        FullscreenState::Fullscreen(current) => {
                            FullscreenState::Exiting(current.clone())
                        }
                        _ => {
                            log::warn!("invalid output state for fullscreen toggle");
                            return;
                        }
                    };

                    state.outputs.get_mut(output).unwrap().fullscreen = new_state;
                }
            }
        }
    }
}

dispatch_log_only!(river::river_node_v1::RiverNodeV1);
dispatch_log_only!(river::river_xkb_bindings_v1::RiverXkbBindingsV1);
dispatch_log_only!(river::river_xkb_bindings_seat_v1::RiverXkbBindingsSeatV1);
dispatch_log_only!(RiverLayerShellV1);
dispatch_log_only!(RiverLayerShellSeatV1); // TODO: focus_none? hmm
