[![Build Status](https://travis-ci.org/david-christiansen/racket-presentation-gui.svg?branch=master)](https://travis-ci.org/david-christiansen/racket-presentation-gui)

Presentations
=============

This is a collection of Racket GUI widgets for creating _presentation-based interfaces_, in which there is a link between information on a screen and an underlying semantic object. GUI commands can act on presented objects, and highlights are provided to show when two objects are equal.

For a demo, check this code out and then run:
```
$ racket repl.rkt
```
In the resulting window, evaluate:
```
> #((1 2 3) 1 2 (3 (4 5)))
```
Mouse over different parts of the result, and see equal things light up. Right-click the whole result, and select "Inspect value". Scrub the mouse around the resulting window, and note how correspondences between the various objects are highlighted.
