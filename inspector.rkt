#lang racket

(require racket/gui framework)
(require pict pict/shadow)
(require "private/presentation.rkt"
         "private/presentation/canvas.rkt"
         "private/presentation/text.rkt")

(provide gui-inspect)

(define inspection-modality 'value)

(define (exact-floor n) (inexact->exact (floor n)))
(define (exact-ceiling n) (inexact->exact (ceiling n)))

(define (present-pair pair)
  (define box-size 30)
  (define car-box
    (new rectangle% [width box-size] [height box-size] [color "gray"] [hl-color "red"]))
  (define cdr-box
    (new rectangle% [width box-size] [height box-size] [color "gray"] [hl-color "red"]))
  (define pair-img
    (new compound-img%
         [imgs `((0 0 ,car-box)
                 (,(add1 box-size) 0 ,cdr-box))]))
  (define car-contents (inspector-present (car pair)))
  (define cdr-contents (inspector-present (cdr pair)))
  (define whole-img
    (new compound-img% [imgs `((0 0 ,pair-img)
                               (,(exact-floor (/ box-size 2)) ,(exact-floor (+ box-size 40)) ,car-contents)
                               (,(exact-floor (+ 40 (send car-contents get-width)))
                                ,(exact-floor (+ box-size 40))
                                ,cdr-contents))]))
  (new presentation-img%
         [object pair]
         [modality 'value]
         [img whole-img]))

(define (inspector-present obj)
  (match obj
    [(? pair? pair)
     (present-pair pair)]
    [other
     (let* ([str (inset (text (format "~v" obj)) 5)]
            [boxed (cc-superimpose (filled-rectangle (pict-width str) (pict-height str) #:color "white" #:border-color "black" #:draw-border? #t)
                                   str)])
       (new compound-img%
            [imgs `((0 0 ,(new presentation-img%
                               [object other]
                               [modality 'value]
                               [img (new pict-img%
                                         [pict boxed]
                                         [hl-pict (shadow boxed 20 #:shadow-color "yellow")])])))]))]))

(define (gui-inspect obj)
  (define frame (new frame% [width 400] [height 400] [label "Inspector"]))
  (define canvas (new presentation-canvas% [parent frame] [style '(hscroll vscroll)]))
  (send canvas add-img (inspector-present obj) 20 20)
  (send frame show #t))

(module+ main
  #;(gui-inspect "hello")
  (gui-inspect '(1 2 "hello" 4))
  #;(gui-inspect (cons 'a 'b))
  )

