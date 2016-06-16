#lang racket

(require racket/gui/base racket/class racket/contract racket/match racket/set)
(require pict pict/shadow)
(require (only-in pict/private/pict cons-picture*))

(require "../presentation.rkt")

(provide presentation-pict-canvas%)

(struct pos (x y pict) #:transparent)

(define (exact-round n) (inexact->exact (round n)))
(define (exact-ceiling n) (inexact->exact (ceiling n)))

(define pict-occlusion-cache (make-hasheq))
(define (occlusion-dc pict)
  (let ([make-bitmap
         (thunk
          (let* ([new-bitmap (make-object bitmap%
                                          (exact-ceiling (pict-width pict))
                                          (exact-ceiling (pict-height pict))
                                          #f #t
                                          (get-display-backing-scale))]
                 [new-dc (new bitmap-dc% [bitmap new-bitmap])])
            (send new-dc erase)
            (draw-pict pict new-dc 0 0)
            new-dc))])
    (hash-ref! pict-occlusion-cache pict make-bitmap)))

(define (pict-occludes? pict x y)
  (set! x (exact-round x))
  (set! y (exact-round y))
  (and (<= 0 x (pict-width pict))
       (<= 0 y (pict-height pict))
       (let ([dc (occlusion-dc pict)]
             [pixel-color (make-object color%)])
         (send dc get-pixel x y pixel-color)
         (> (send pixel-color alpha) 0.00001))))


(define presentation-pict-canvas%
  (class* canvas%
    (presenter<%>)
    (init [style null])
    (init-field [presentation-context #f])
    (super-new [style (cons 'no-autoclear style)]
               [paint-callback
                (lambda (canvas dc)
                  (send canvas suspend-flush)
                  (send dc clear)
                  (set! presentations null)
                  (for ([img+location picts])
                    (match-let ([(pos x y p) img+location])
                      (draw-pict p dc x y)))
                  (send canvas resume-flush))])
    (unless presentation-context
      (set! presentation-context (current-presentation-context)))
    (send presentation-context register-presenter this)

    (define mouse-x #f)
    (define mouse-y #f)
    ;; a procedure to recognize active presentations
    (define active? (lambda (x) #f))

    (define picts '())

    (define/public (add-pict pict x y)
      (set! picts (cons (pos x y pict) picts))
      (queue-callback (thunk (send this refresh))))

    (define/public (remove-all-picts)
      (set! picts null)
      (queue-callback (thunk (send this refresh))))

    (define/override (on-superwindow-show shown?)
      (send this refresh-now))

    (define/public (highlight pres-type value)
      (set! active? (lambda (x) (presented-object-equal? pres-type value x)))
      (queue-callback (thunk (send this refresh))))

    (define/public (no-highlighting)
      (set! active? (lambda (x) #f))
      (queue-callback (thunk (send this refresh))))

    (define/public (new-context-state st)
      (void))

    (define (transform-rectangle dc x y w h)
      (define mx (send dc get-initial-matrix))
      (define-values (sx sy) (send dc get-scale))
      (define-values (dx dy) (send dc get-origin))
      (define r (send dc get-rotation))
      (define p (new dc-path%))
      (send p rectangle x y w h)
      (send p rotate r)
      (send p scale sx sy)
      (send p translate dx dy)
      (send p transform mx)
      (send p get-bounding-box))

    (struct pict-presentation (x y pict value type)
      #:property prop:presentation
      (list (lambda (x) (pict-presentation-value x))
            (lambda (x) (pict-presentation-type x))))

    (define presentations null)
    (define (register-presentation object modality pict x y)
      (set! presentations
            (cons
             (pict-presentation x y pict object modality)
             presentations)))

    (define (most-specific p1 p2)
      ;; TODO - find a better measure. Right now, smallest = most specific...
      (define pict-1 (pict-presentation-pict p1))
      (define pict-2 (pict-presentation-pict p2))
      (if (< (* (pict-width pict-1)
                (pict-height pict-1))
             (* (pict-width pict-2)
                (pict-height pict-2)))
          p1
          p2))

    (define/public (make-presentation object type pict hl)
      (define drawer (make-pict-drawer pict))
      (define hl-drawer (make-pict-drawer (hl pict)))
      (define (draw-fn dc dx dy)
        (define-values (bx by bw bh) (transform-rectangle dc dx dy (pict-width pict) (pict-height pict)))
        (register-presentation object type pict dx dy)
        (if (active? object)
            (hl-drawer dc dx dy)
            (drawer dc dx dy)))
      (make-pict `(prog ,draw-fn ,(pict-height pict))
                 (pict-width pict) (pict-height pict)
                 (pict-ascent pict) (pict-descent pict)
                 (pict-children pict)
                 (pict-panbox pict)
                 (pict-last pict)))

    (define (find-presentations x y)
      (define ((presentation-covers? x y) p)
        (and (<= (pict-presentation-x p) x (+ (pict-presentation-x p)
                                              (pict-width (pict-presentation-pict p))))
             (<= (pict-presentation-y p) y (+ (pict-presentation-y p)
                                              (pict-height (pict-presentation-pict p))))))
      (define (presentation-area pres)
        (define pict (pict-presentation-pict pres))
        (* (pict-width pict) (pict-height pict)))
      (sort (filter (presentation-covers? x y) presentations) < #:key presentation-area))

    (define (find-current-presentation x y)
      (define accepting (send presentation-context currently-accepting))
      (define (presentation-interesting? p)
        (or (not accepting)
            (presentation-has-type? p accepting)))
      (define res
        (let* ([ps (find-presentations mouse-x mouse-y)])
          (if (pair? ps)
              (let loop ([best (car ps)] [rest (cdr ps)])
                (cond
                  [(and (presentation-interesting? best)
                        (pict-occludes? (pict-presentation-pict best)
                                        (- x (pict-presentation-x best))
                                        (- y (pict-presentation-y best))))

                   best]
                  [(pair? rest)
                   (loop (car rest) (cdr rest))]
                  [else           ; no interesting presentations found
                   #f]))
              #f)))
      res)

    (define/override (on-event ev)
      ;; Update mouse coordinates
      (cond [(or (send ev moving?) (send ev entering?) (send ev button-down?))
             (set! mouse-x (send ev get-x))
             (set! mouse-y (send ev get-y))]
            [(send ev leaving?)
             (set! mouse-x #f)
             (set! mouse-y #f)])
      ;; If the mouse coordinates are valid then update the presentation context
      (cond
        [(or (send ev moving?) (send ev entering?))
         (queue-callback
          (thunk (let ([p (find-current-presentation mouse-x mouse-y)])
                   (if p
                       (send presentation-context make-active p)
                       (send presentation-context nothing-active))
                   (queue-callback (thunk (send this refresh))))))]
        [(and (send presentation-context currently-accepting) (send ev button-down?))
         (let ([p (find-current-presentation mouse-x mouse-y)])
           (when p
             (send presentation-context accepted p)))]
        [(send ev button-down? 'right)
         (let ([p (find-current-presentation mouse-x mouse-y)]
               [menu (new popup-menu%)])
           (when p
             (define cmds (send presentation-context commands-for p))
             (when (not (null? cmds))
               (for ([cmd cmds])
                 (new menu-item%
                      [label (car cmd)]
                      [parent menu]
                      [callback
                       (lambda args (queue-callback (cadr cmd)))]))
               (send this popup-menu menu (send ev get-x) (send ev get-y)))))]))))



