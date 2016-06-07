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
    (super-new [style (cons 'no-autoclear style)])
    (unless presentation-context
      (set! presentation-context (current-presentation-context)))
    (send presentation-context register-presenter this)

    ;; Drawing buffer (for flicker-prevention)
    (define buffer #f)
    (define buffer-dc #f)
    (define (resize-buffer x y)
      (set! buffer (make-screen-bitmap x y))
      (set! buffer-dc (send buffer make-dc)))
    (resize-buffer (send this get-width) (send this get-height))

    (define/override (on-size x y)
      (resize-buffer x y)
      (queue-callback (thunk (send this refresh))))

    (define mouse-x #f)
    (define mouse-y #f)
    (define active (seteq)) ;; The active _domain objects_ that are presented

    (define picts '())

    (define/public (add-pict pict x y)
      (set! picts (cons (pos x y pict) picts)))

    (define/override (on-paint)
      (define a (current-milliseconds))
      (send buffer-dc clear)
      (set! presentations null)
      (for ([img+location picts])
        (match-let ([(pos x y p) img+location])
          (draw-pict p buffer-dc x y )))
      (send (send this get-dc) draw-bitmap buffer 0 0)
      (define b (current-milliseconds))
      (void))

    (define/public (activate obj)
      (unless (and (= (set-count active) 1)
                   (set-member? active obj))
        (set! active (seteq obj))
        (queue-callback (thunk (send this refresh)))))

    (define/public (deactivate)
      (unless (set-empty? active)
        (set! active (seteq))
        (queue-callback (thunk (send this refresh)))))

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

    (define pict-presentation%
      (class* object% (presentation<%>)
        (init-field x y pict object modality)
        (super-new)
        (define/public (get-presented-object) object)
        (define/public (get-modality) modality)))
    (define (presentation-x p) (get-field x p))
    (define (presentation-y p) (get-field y p))
    (define (presentation-pict p) (get-field pict p))
    (define (presentation-object p) (get-field object p))
    (define (presentation-modality p) (get-field modality p))
    (define (presentation x y pict object modality)
      (make-object pict-presentation% x y pict object modality))

    (define presentations null)
    (define (register-presentation object modality pict x y)
      (set! presentations
            (cons
             (presentation x y pict object modality)
             presentations)))

    (define (most-specific p1 p2)
      ;; TODO - find a better measure. Right now, smallest = most specific...
      (define pict-1 (presentation-pict p1))
      (define pict-2 (presentation-pict p2))
      (if (< (* (pict-width pict-1)
                (pict-height pict-1))
             (* (pict-width pict-2)
                (pict-height pict-2)))
          p1
          p2))

    (define/public (make-presentation object modality pict hl)
      (define drawer (make-pict-drawer pict))
      (define hl-drawer (make-pict-drawer (hl pict)))
      (define (draw-fn dc dx dy)
        (define-values (bx by bw bh) (transform-rectangle dc dx dy (pict-width pict) (pict-height pict)))
        (register-presentation object modality pict dx dy)
        (if (set-member? active object)
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
        (and (<= (presentation-x p) x (+ (presentation-x p)
                                         (pict-width (presentation-pict p))))
             (<= (presentation-y p) y (+ (presentation-y p)
                                         (pict-height (presentation-pict p))))))
      (define (presentation-area pres)
        (define pict (presentation-pict pres))
        (* (pict-width pict) (pict-height pict)))
      (sort (filter (presentation-covers? x y) presentations) < #:key presentation-area))

    (define (find-current-presentation x y)
      (define st (send presentation-context get-state))
      (define (presentation-interesting? p)
        (match st
          [`(accepting ,pred ,callback)
           (pred (presentation-object p) (presentation-modality p))]
          [_ #t]))
      (define res
        (let* ([ps (find-presentations mouse-x mouse-y)])
          (if (pair? ps)
              (let loop ([best (car ps)] [rest (cdr ps)])
                (cond
                  [(and (presentation-interesting? best)
                         (pict-occludes? (presentation-pict best)
                                         (- x (presentation-x best))
                                         (- y (presentation-y best))))

                   best]
                  [(pair? rest)
                   (loop (car rest) (cdr rest))]
                  [else ; no interesting presentations found
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
                       (send presentation-context make-active (presentation-object p))
                       (send presentation-context nothing-active))
                   (queue-callback (thunk (send this refresh))))))]
        [(and (send presentation-context accepting?) (send ev button-down?))
         (let ([p (find-current-presentation mouse-x mouse-y)])
           (when p
             (send presentation-context accepted (presentation-object p))))]
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



