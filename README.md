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

Most things work, and I use reka as my daily driver. The main feature that is
still missing is floating windows. There are likely still some bugs and rough
edges outside of my particular configuration, though, so be ready for that.

There is **no** simulation key support, this will need a [new
protocol](https://codeberg.org/river/river/pulls/1412) on the river side.

Reka does not expose any input or output management to Emacs Lisp right now, and
I'm not sure that it will. River has external tools
([channel](https://codeberg.org/Sivecano/channel) for input management, and
[kanshi](https://gitlab.freedesktop.org/emersion/kanshi) for output management)
that are sufficient for me.

If you want something more fully featured, look in the direction of EWM instead!

## Using this

You should be a sufficiently committed person to use this. There is no
hand-holding!

First, get the source either from TVL directly, or from the mirror:

```
# clone directly from TVL:
git clone https://code.tvl.fyi/depot.git:/tools/emacs-pkgs/reka.git

# or clone from Codeberg mirror:
git clone https://codeberg.org/tazjin/reka.git
```

After that, build the Rust project (you need `pkg-config` and `libxkbcommon`),
and make yourself a script that launches Emacs approximately like this:

```
emacs --directory $reka_src/target/release --directory $reka_src/lisp ...
```

and then pass that script to river's `-c` flag on launch. In your Emacs config,
make sure to `(require 'reka)` and then `(reka-enable)`.

## Contributing

The upstream code location is in [the TVL
monorepo](https://code.tvl.fyi/about/tools/emacs-pkgs/reka). Issues can be
reported to the TVL bug tracker for those that have an account here, or on the
[Codeberg mirror](https://codeberg.org/tazjin/reka).

Reka follows the standard TVL [contribution
guidelines](https://code.tvl.fyi/tree/docs/CONTRIBUTING.md). Patches must be
written by real humans using fleshy brains.
