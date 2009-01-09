# Emacs Starter Kit

From Phil Hagelberg's emacs-starter-kit.

## Current or planned customizations by Geoffrey Grosenbach (topfunky)

* Intended for use with Emacs built for Mac OS X from git://git.sv.gnu.org/emacs.git

### Planned

* Implement Mac-friendly keyboard shortcuts. Liberal usage of the Apple command key to make shortcuts even shorter.
* Add other plugins that I use, such as Textile, Haml, Sass, etc.
* Add TextMate-style snippets with yasnippet

# Description

This should provide a saner set of defaults than you get normally with
Emacs. It's intended for beginners, but it should provide a reasonable
working environment for anyone using Emacs.

## Installation

1. Install Emacs
   Use your package manager if you have one.
   Otherwise, Mac users should get it [from Apple](http://www.apple.com/downloads/macosx/unix_open_source/carbonemacspackage.html).
   Windows users can get it [from GNU](http://ftp.gnu.org/gnu/emacs/windows/emacs-22.3-bin-i386.zip).
2. Move this directory to ~/.emacs.d
3. Launch Emacs!

If you are missing some autoloads after an update (should manifest
itself as "void function: foobar" errors) try deleting your
loaddefs.el file and restarting Emacs.

If you want to keep your regular ~/.emacs.d in place and just launch a
single instance using the starter kit, try the following invocation:

  $ emacs -q -l ~/src/emacs-starter-kit/init.el

## ELPA

Libraries from ELPA (http://tromey.com/elpa) are preferred when
available since dependencies are handled automatically, and the burden
to update them is removed from the user.

## Contributing

If you know your way around Emacs, please try out the starter kit as a
replacement for your regular dotfiles for a while. If there's anything
you just can't live without, add it or let me know so I can add it.

Also: see the file TODO.
