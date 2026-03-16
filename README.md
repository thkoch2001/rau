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

reka works, and I have been using it day-to-day for a few days at this point.

It definitely has bugs and rough edges. It also doesn't do any input or output
management (use [channel](https://codeberg.org/Sivecano/channel) and
[kanshi](https://gitlab.freedesktop.org/emersion/kanshi) for that). If you just
want working Wayland window management in Emacs, look in the direction of EWM
instead.

## Using this

I have not added anything for launching this conveniently yet. That is on
purpose! This is not a convenient project (yet), and you should be a
sufficiently committed person to try and use it.

First, get the source either from TVL directly, or from the mirror:

```
# clone directly from TVL:
git clone https://code.tvl.fyi/depot.git:/tools/emacs-pkgs/reka.git

# or clone from mos.ru mirror:
git clone https://codeberg.org/tazjin/reka.git
```

After that, build the Rust project (you need `pkg-config` and `libxkbcommon`),
and make yourself a script that launches Emacs approximately like this:

```
emacs --directory $reka_src/target/release --directory $reka_src/lisp ...
```

and then pass that script to river's `-c` flag on launch. In your Emacs config,
make sure to `(require 'reka)` and `(reka-enable)`.

## Contributing

Reka follows the standard TVL [contribution guidelines](https://code.tvl.fyi/tree/docs/CONTRIBUTING.md).
