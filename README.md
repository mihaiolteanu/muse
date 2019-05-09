# muse - a last.fm like mouseless music player with lyrics

## Overview

Use the last.fm artist database together with youtube links to
implement a music player that acts similary to last.fm but that can be
interacted with without using the mouse. The main idea is a local
database with content parsed from last.fm and a localhost web server
that can be called to play songs from youtube using and interacting
with mpv behind the scenes. The interaction with the web server can be
done directly from a web browser (which kinda defeats the purpose) or
with a window manager like [stumpwm](https://stumpwm.github.io/), for
example, or by using
[dmenu](https://wiki.archlinux.org/index.php/dmenu). As an extra
feature, songs can be searched and played based on their lyrics, again
without resorting to a web browser or a mouse.

## Release status

This project is not officially released yet. Some features are already
implemented but some improvements and features are definitely needed
or desired. Still, I feel I need to up my Common Lisp game before I
tackle anything more serious than this. See the discussion and details
in the linked article.

## Instalation
Make sure ql:quickload can find this repo by cloning it in this location

```shell
git clone https://github.com/mihaiolteanu/muse ~/quicklisp/local-projects/muse
```

Start a REPL and load it

```shell
(ql:quickload :muse)
```

Or from the command line,

```shell
sbcl --quit --eval "(ql:quickload :muse)"
```

## Usage
For stumpwm, the commands to control the player are already there. Copy the `muse-stump` folder to stumpwm's `modules` folder.

```shell
cp ~/quicklisp/local-projects/muse/muse-stump ~/.stumpwm.d/modules
```

After that, the muse module can be loaded in your ``init.lisp` file with

```common-lisp
(load-module "muse-stump")
```

The following commands can then be keybinded to whatever you like. For
exemple,

```common-lisp
(defvar *muse-map* (make-sparse-keymap))
(define-key *root-map* (kbd "m") '*muse-map*)
(define-key *muse-map* (kbd "v") "continue-with-video")
(define-key *muse-map* (kbd "w") "what-is-playing")
(define-key *muse-map* (kbd "l") "song-lyrics")
(define-key *muse-map* (kbd "p") "play-artist")
(define-key *muse-map* (kbd "s") "toggle-shuffle")
(define-key *top-map* (kbd "XF86AudioPlay") "play")
(define-key *top-map* (kbd "XF86AudioPrev") "previous-song")
(define-key *top-map* (kbd "XF86AudioNext") "next-song")
(define-key *top-map* (kbd "XF86AudioStop") "stop-player")
```

## Tests

Test for interacting with the mpv music player are available. These
actually load youtube urls and start playing videos thus testing the
actual player functionality. These take longer and are thus specified
seperately. The no-dependency-tests are all the tests minus these mpv
tests.

### Run all  tests
```shell
sbcl --quit --eval "(asdf:test-system :muse)"`
```

### Run mpv tests
```shell
sbcl --quit --eval "(ql:quickload :muse/tests)" --eval "(fiveam:run! :mpv-tests)"`
```

### Run tests with no outside dependencies
```shell
sbcl --quit --eval "(ql:quickload :muse/tests)" --eval "(fiveam:run! :no-dependency-tests)"
```
`
