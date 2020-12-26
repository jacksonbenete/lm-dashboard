# lm-dashboard
Lisp Machine Inspired Dashboard

This is a package for learning how to use Emacs Widgets.

It aims to offer a Dashboard-like experience that resembles
some Lisp Machine initial pages such as of Symbolics Genera.

It is way less powerful than emacs-dashboard at the moment, but aims
to integrate more functions and compatibilities in future.

The package is about 0.5 seconds slower than emacs-dashboard.

The package is working but need to be refactored both for increasing
speed and provides a cleaner code.

## Dashboard
This is how it's working at the moment.

You can press `r` or `m` to jump sections just like on emacs-dashboard.

You can cycle between widgets using `TAB`.
`C-n` and `C-p` works as well as arrows.

Pressing `g` will refresh the dashboard,
and `q` will quit, but not kill the window.
If you ever kill the dashboard and want to call it again you can
`M-x lm-dashboard-refresh`.

![fullscreen][1]

The dashboard will automatically resize when the frame resizes,
if the screen is too small it will show only one column of files.

![halfscreen][2]

## Instalation

Clone the package to your `.emacs.d/` and require the package.

```lisp
(add-to-list 'load-path "~/.emacs.d/lm-dashboard")
(require 'lm-dashboard)
(lm-dashboard-startup-hook)
```

[1]: ./screenshots/lm-dashboard-full-screen.png
[2]: ./screenshots/lm-dashboard-half-screen.png
