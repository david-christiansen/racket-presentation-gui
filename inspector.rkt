#lang racket

(require racket/gui framework)
(require pict pict/shadow)
(require "private/presentation.rkt"
         "private/presentation/pict.rkt"
         "private/presentation/text.rkt")

(provide gui-inspect value/p (rename-out [present-value present-to-pict]))

(define (exact-floor n) (inexact->exact (floor n)))
(define (exact-ceiling n) (inexact->exact (ceiling n)))

(define (cell-box)
  (cc-superimpose
   (filled-rectangle 20 20 #:draw-border? #t #:color "white")
   (filled-ellipse 5 5 #:color "black")))

(define value/p (make-presentation-type 'value/p))

(define/contract (present-value c v)
  (-> (is-a?/c pict-presenter<%>) any/c pict?)
  (cond
    [(pair? v)
     (define car-pict (present-value c (car v)))
     (define cdr-pict (present-value c (cdr v)))
     (define box-1 (cell-box))
     (define box-2 (cell-box))
     (define cons-cell-pict
       (vc-append 30
                  (hc-append box-1 box-2)
                  (ht-append 20
                             car-pict
                             cdr-pict)))
     (define with-car-arrow
       (pin-arrow-line 5 cons-cell-pict box-1 cc-find car-pict ct-find
                       #:end-angle (* pi 1.5)))
     (define with-cdr-arrow
       (pin-arrow-line 5 with-car-arrow box-2 cc-find cdr-pict ct-find
                       #:end-angle (* pi 1.5)))
     (send c make-presentation v value/p
           with-cdr-arrow
           hl)]
    [(vector? v)
     (define start-pict (text "#(" null 20))
     (define end-pict (text ")" null 20))
     (define sub-picts
       (for/list ([cell (in-vector v)])
         (cons (cell-box) (present-value c cell))))
     (define contents (apply hc-append (map car sub-picts)))
     (define no-arrows (vc-append 60
                                  (hc-append start-pict contents end-pict)
                                  (apply ht-append 20 (map cdr sub-picts))))
     (send c make-presentation v value/p
           (for/fold ([picture no-arrows])
                     ([elem (in-list sub-picts)])
             (pin-arrow-line 5 picture (car elem) cc-find (cdr elem) ct-find
                             #:start-angle (* pi 1.5)
                             #:end-angle (* pi 1.5)))
           hl)]
    [else (send c make-presentation v value/p
                (let ([t (inset (text (format "~v" v) null 20) 2)])
                  (cc-superimpose
                   (filled-rectangle (pict-width t) (pict-height t) #:color "white")
                   t))
                hl)]))

(define (hl p)
  (colorize p "red"))

(define (gui-inspect obj)
  (define frame (new frame% [width 400] [height 400] [label "Inspector"]))
  (define canvas (new presentation-pict-canvas% [parent frame] [style '(hscroll vscroll)]))
  (send canvas add-pict (present-value canvas obj) 20 20)
  (send frame show #t))

(module+ main
  #;(gui-inspect "hello")
  (gui-inspect '(1 2 "hello" (1 4)))
  #;(gui-inspect (cons 'a 'b))
  )

