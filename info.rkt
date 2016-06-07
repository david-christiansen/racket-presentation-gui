#lang info
(define collection "presentations")
(define deps '("base"
               "rackunit-lib"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/presentations.scrbl" ())))
(define pkg-desc "Presentation-based GUI widgets")
(define version "0.0")
(define pkg-authors '("David Christiansen"))
