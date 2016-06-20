#lang racket

(require racket/gui)
(require "../presentation.rkt" "pict.rkt")

(provide presentation-pict-snip%)

(define presentation-pict-snip%
  (class* (pict-presenter-mixin snip%)
    (presenter<%> pict-presenter<%>)
    (super-new)

    (inherit handle-mouse-event)

    (define (set-box!/ok b val)
      (if (box? b)
          (set-box! b val)
          (void)))

    ;; Snip methods
    (send this set-flags (cons 'handles-all-mouse-events
                               (cons 'handles-events
                                     (send this get-flags))))

    (define/override (get-extent dc x y
                                 [w #f]
                                 [h #f]
                                 [descent #f]
                                 [space #f]
                                 [lspace #f]
                                 [rspace #f])
      (set-box!/ok w (+ 2 (send this get-draw-width)))
      (set-box!/ok h (+ 2 (send this get-draw-height)))
      (set-box!/ok descent 1.0)
      (set-box!/ok space 1.0)
      (set-box!/ok lspace 0)
      (set-box!/ok rspace 0))

    (define/override (draw dc x y left top right bottom dx dy draw-caret)
      (define smoothing (send dc get-smoothing))
      (define pen (send dc get-pen))
      (send this draw-picts dc x y)
      (send dc set-smoothing smoothing)
      (send dc set-pen pen))

    (define/override (on-event dc x y editor-x editor-y ev)
      ;; Here we must pretend that we are running all mouse events
      ;; relative to global rather than local coordinates, because
      ;; when the presentation-picts register themselves, they only
      ;; know about the DC in which they are found.
      (handle-mouse-event ev 0 0))))


