# breakout

A clone of Breakout. This particular clone was originally written by Jamie Zawinski for the TI-Explorer Lisp Machine, and ported in this version to "modern" Common-Lisp using SDL bindings.

![breakout](https://user-images.githubusercontent.com/19293817/83981989-f5709600-a922-11ea-9b9a-97dab199d4df.gif)
<sup>_The gif above is only for previewing purposes and it's not as smooth and satisfying as when you run the program ;-)_</sup>

The original sources were taken from the [CMU AI Repository](http://www-cgi.cs.cmu.edu/afs/cs/project/ai-repository/ai/lang/lisp/code/impdep/explorer/).

I've used SBCL, so I have no clue if/how this will work on other Lisps. To run this you'll need quicklisp and lispbuilder-sdl.

If you don't have quicklisp you can find instructions here:<br/>
https://golems.github.io/motion-grammar-kit/install.html

or here:<br/>
https://lispcookbook.github.io/cl-cookbook/getting-started.html

Then once into the REPL:

```
* (load "~/quicklisp/setup.lisp")

T
* (ql:quickload :lispbuilder-sdl)
To load "lispbuilder-sdl":
  Load 1 ASDF system:
    lispbuilder-sdl
; Loading "lispbuilder-sdl"
......
(:LISPBUILDER-SDL)
* (load "breakout")

T
* (breakout)
```
