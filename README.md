reka
====

reka lets emacs' logic just flow into [river](https://codeberg.org/river/river).
It is a window manager inside of Emacs for the Wayland world.

![screenshot of reka](https://tazj.in/blobs/reka_0228.png)

## Background

I spent many years using [EXWM](https://github.com/ch11ng/exwm/), it was great
but quite wonky. At some point Wayland became unavoidable (I really need
per-monitor fractional scaling!) and I switched to
[niri](https://github.com/YaLTeR/niri), which is one of the highest quality
pieces of software in the world.

At some point my friend [ezemtsov](https://codeberg.org/ezemtsov/) wrote
[EWM](https://codeberg.org/ezemtsov/ewm), which is a full Emacs compositor
running as a dynamic module inside of Emacs. It's a great project, but I wanted
to see if we can avoid the complexity of a full compositor implementation with
the [river WM
protocol](https://isaacfreund.com/docs/wayland/river-window-management-v1/), and
classic *brain-coding*.

What else ... the name? Oh, it's obvious to Russian speakers.

## Status

reka kind of works and I have successfully used it several times for most of the
day. It definitely has bugs though, it doesn't support layer shells yet, it
doesn't do any input management (use
[channel](https://codeberg.org/Sivecano/channel) for that) and so on. If you
just want working Wayland window management in Emacs, look in the direction of
EWM instead.

## Using this

I have not added any Nix files, session files or whatever else for launching
this conveniently yet. That is on purpose! This is not a convenient project
(yet), and you should be a sufficiently committed person to try and use it.

In short, build the Rust project (you need `pkg-config` and `libxkbcommon`), and
make yourself a script that launches Emacs approximately like this:

```
emacs --directory $reka_src/target/release --directory $reka_src/lisp ...
```

and then pass that script to river's `-c` flag on launch. In your Emacs config,
make sure to `(require 'reka)` and `(reka-enable)`.

## Contributing

I do not accept any contributions to reka at this time, but you can ping me on
IRC (tazjin in #river on libera) with issues.

If I keep developing this (no guarantees, I might just use EWM instead!) then it
will move into [TVL](https://tvl.fyi) and follow the standard contribution flow
there.
