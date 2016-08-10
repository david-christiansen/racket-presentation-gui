#lang racket

(require racket/gui framework)
(require pict pict/shadow pict/convert)
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


;;; A presentation cache is needed to prevent sub-picts from being
;;; redrawn too often. A cache inside the library is not sufficient,
;;; because when a presentation is regenerated, it recursively
;;; re-generates the sub-picts. The library can't track them by
;;; equality of presented object, because one object might have
;;; multiple independent presentations, and it can't track them by
;;; pointer identity, because they are regenerated. Structural
;;; equality is a no-go due to the presence of higher-order data. So
;;; presenters must pay attention and not regenerate sub-picts.
;;;
;;; Sigh.
(define (make-cache)
 (define presented (make-hasheq))
 ;; Arrange for the cache to be obliterated upon mutations.
 (send (current-presentation-context) register-presenter
       (make-object
        (class* object% (presenter<%>)
          (super-new)
          (define/public (highlight t v) (void))
          (define/public (no-highlighting) (void))
          (define/public (mutation)
            (hash-clear! presented)))))
 presented)

(define/contract (present-value c v [cache (make-cache)])
  (-> (is-a?/c pict-presenter<%>) any/c pict-convertible?)
  (hash-ref! cache v
             (cond
               [(pair? v)
                (send c make-presentation v value/p
                      (lambda (pair)
                        (define car-pict (present-value c (car pair) cache))
                        (define cdr-pict (present-value c (cdr pair) cache))
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
                        with-cdr-arrow)
                      hl)]
               [(vector? v)
                (send c make-presentation v value/p
                      (lambda (vec)
                        (define start-pict (text "#(" null 20))
                        (define end-pict (text ")" null 20))
                        (define sub-picts
                          (for/list ([cell (in-vector vec)])
                            (cons (cell-box) (present-value c cell cache))))
                        (define contents (apply hc-append (map car sub-picts)))
                        (define no-arrows (vc-append 60
                                                     (hc-append start-pict contents end-pict)
                                                     (apply ht-append 20 (map cdr sub-picts))))
                        (for/fold ([picture no-arrows])
                                  ([elem (in-list sub-picts)])
                          (pin-arrow-line 5 picture (car elem) cc-find (cdr elem) ct-find
                                          #:start-angle (* pi 1.5)
                                          #:end-angle (* pi 1.5))))
                      hl)]
               [(box? v)
                (send c make-presentation v value/p
                      (lambda (val)
                        (define box-pict (cell-box))
                        (define val-pict (present-value c (unbox val) cache))
                        (define picture (vc-append 20 box-pict val-pict))
                        (pin-arrow-line 5 picture box-pict cc-find val-pict ct-find
                                        #:start-angle (* pi 1.5)
                                        #:end-angle (* pi 1.5)))
                      hl)]
               [else (send c make-presentation v value/p
                           (thunk* (let ([t (inset (text (format "~v" v) null 20) 2)])
                                     (cc-superimpose
                                      (filled-rectangle (pict-width t) (pict-height t) #:color "white")
                                      t)))
                           hl)])))

(define (hl p)
  (colorize p "red"))

(define (gui-inspect obj)
  (define frame (new frame% [width 400] [height 400] [label "Inspector"]))
  (define canvas (new presentation-pict-canvas% [parent frame] [style '(hscroll vscroll)]))
  (send canvas add-pict (present-value canvas obj) 20 20)
  (send frame show #t))

(define data (list 1 2 "hello" (box 4)))

(module+ main
  #;(gui-inspect "hello")
  (gui-inspect data)
  #;(gui-inspect (cons 'a 'b))
  )

