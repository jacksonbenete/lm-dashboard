# lm-dashboard

This is a package for learning how to use Emacs Widgets.

It aims to offer a Dashboard-like experience that resembles
some Lisp Machine initial pages such as of Symbolics Genera.

This package is not as powerful as
[emacs-dashboard](https://github.com/emacs-dashboard/emacs-dashboard),
the sole purpose is for learning, but since I don't use emacs-dashboard
full potential it works like a charm for me.

The package is about 0.5 seconds slower than emacs-dashboard.

The package is working but need to be refactored both for increasing
speed and provide a cleaner code.

It is also not currently resembling *Symbolics Genera* inital page, both to
avoid writing "Symbolics Genera 8.3" which is trademarked,
and because the package is not complete yet.


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

When the filename/path is too big, you can still see the full name
by selecting the item or passing the mouse over.


## Instalation

Clone the package to your `.emacs.d/` and require the package.

```lisp
(add-to-list 'load-path "~/.emacs.d/lm-dashboard")
(require 'lm-dashboard)
(lm-dashboard-startup-hook)
```


## TODO
- Some way more modular and easy to use functions are already implemented,
those are `lm-dashboard-create-widgets-list` and `lm-dashboard-insert-widgets`
but it still uses an old version of `lm-dashboard-list-widgets` for now.

- Redistribute sections and write footers for it to looks like Genera
initial page.

![Lisp Machine Initial Page 1][3]
![Lisp Machine Initial Page 2][4]


## Acknowledgment

The Lisp logo on screenshots is part of a great work of art called
[The Elusive Language](https://github.com/phoe/the-elusive-language)
by [Michał \"phoe\" Herda](https://phoe.github.io/). It's licensed under BY-SA.


[1]: ./screenshots/lm-dashboard-full-screen.png
[2]: ./screenshots/lm-dashboard-half-screen.png
[3]: ./screenshots/genera.gif
[4]: ./screenshots/open-genera.png

