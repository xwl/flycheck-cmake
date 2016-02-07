flycheck-cmake
==============

Define below two checkers:

- c/c++-cmake
- c/c++-cmake-remote

By analysing cmake compile_commands.json, it defines a precise
flycheck checker (c/c++-cmake) for each cmake project, with exact
compile flags, include paths, etc.

If you are like me, editing in Mac OS X, but compiling inside a
virtual machine, then the other remote cmake checker
(c/c++-cmake-remote) defined here will be very helpful.  What it
does is simply running flycheck over ssh.

## Usage

See [cmake-compile-commands](https://github.com/xwl/cmake-compile-commands)
