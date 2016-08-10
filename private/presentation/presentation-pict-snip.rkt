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

    (define/override (show-popup-menu menu x y)
      (define admin (send this get-admin))
      (when admin
        (send admin popup-menu menu this x y)))

    (define/override (mutation)
      (super mutation)
      (send (send this get-admin) resized this #t))

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
      (set-box!/ok w (send this get-draw-width))
      (set-box!/ok h (send this get-draw-height))
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
      (define ev-x (send ev get-x))
      (define ev-y (send ev get-y))
      (when (and ev-x (<= 0 (- ev-x x) (send this get-draw-width))
                 ev-y (<= 0 (- ev-y y) (send this get-draw-height)))
        (handle-mouse-event ev x y)))

    (define/override (adjust-cursor dc x y ed-x ed-y ev)
      (make-object cursor% 'arrow))))


