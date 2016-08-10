#lang racket

(require racket/gui/base racket/class racket/contract racket/match racket/set)
(require pict pict/shadow pict/convert)
(require (only-in pict/private/pict cons-picture*))

(require "../presentation.rkt")

(provide presentation-pict-canvas% pict-presenter<%> pict-presenter-mixin)

(struct pos (x y pict) #:transparent)

(define (exact-round n) (inexact->exact (round n)))
(define (exact-ceiling n) (inexact->exact (ceiling n)))

(define pict-occlusion-cache (make-weak-hasheq))
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
            (send new-dc set-smoothing 'aligned)
            (draw-pict pict new-dc 0 0)
            new-dc))])
    (hash-ref! pict-occlusion-cache pict make-bitmap)))

(define (pict-occludes? pict x y)
  (define x* (exact-round x))
  (define y* (exact-round y))
  (and (<= 0 x* (pict-width pict))
       (<= 0 y* (pict-height pict))
       (let ([dc (occlusion-dc pict)]
             [pixel-color (make-object color%)])
         (send dc get-pixel x* y* pixel-color)
         (> (send pixel-color alpha) 0.00001))))

(define pict-presenter<%>
  (interface (presenter<%>)
    [draw-picts (->m (is-a?/c dc<%>) number? number? void?)]
    [make-presentation (->i ([me any/c]
                             [object (p-t) (presentation-type/c p-t)]
                             [p-t presentation-type?]
                             [pict (p-t) (-> (presentation-type/c p-t) pict-convertible?)]
                             [hl (-> pict? pict-convertible?)])
                            [result pict-convertible?])]
    [add-pict (->m pict-convertible? number? number? void?)]
    [remove-all-picts (->m void?)]
    [find-current-presentation (->m number? number? (or/c presentation? #f))]
    [handle-mouse-event (->m (is-a?/c mouse-event%) number? number? void?)]
    [after-draw (->m void?)]
    [get-draw-width (->m number?)]
    [get-draw-height (->m number?)]
    [show-popup-menu (->m (is-a?/c popup-menu%)
                          real? real?
                          void?)]))

(define pict-presenter-mixin
  (mixin () (presenter<%> pict-presenter<%>)
    (super-new)
    (init-field [presentation-context #f])

    (unless presentation-context
      (set! presentation-context (current-presentation-context)))
    (send presentation-context register-presenter this)

    (define picts '())

    (define/public (add-pict pict x y)
      (set! picts (cons (pos x y pict) picts)))

    (define/public (remove-all-picts)
      (set! picts null)
      (send this after-draw))

    (define/public (get-draw-width)
      (apply max (for/list ([p picts])
                   (match-define (pos x y pict) p)
                   (+ x (pict-width pict)))))

    (define/public (get-draw-height)
      (apply max (for/list ([p picts])
                   (match-define (pos x y pict) p)
                   (+ y (pict-height pict)))))

    ;; a procedure to recognize active presentations
    (define active? (lambda (x) #f))

    (define/public (after-draw)
      (void))

    (define/public (highlight pres-type value)
      (set! active? (lambda (x) (presented-object-equal? pres-type value x)))
      (send this after-draw))

    (define/public (no-highlighting)
      (set! active? (lambda (x) #f))
      (send this after-draw))

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

    (struct pict-presentation (x y obj->pict value type)
      #:transparent
      #:property prop:presentation
      (list (lambda (x) (pict-presentation-value x))
            (lambda (x) (pict-presentation-type x))))

    (define presentations null)

    (define (register-presentation object modality obj->pict x y)
      (set! presentations
            (cons
             (pict-presentation x y obj->pict object modality)
             presentations)))

    (define (pict-presentation-pict pp)
      (match-define (pict-presentation _ _ obj->pict val _) pp)
      (obj->pict val))

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

    (define pict-cache (make-weak-hasheq))

    (define/public (mutation)
      (hash-clear! pict-cache)
      (send this after-draw))

    (struct presented-pict
      (object type object->pict hl)
      #:property prop:pict-convertible
      (lambda (me)
        (match-define (presented-pict object type object->pict hl) me)
        (hash-ref! pict-cache me
                   (thunk
                    (define pict (pict-convert (object->pict object)))
                    (define drawer (make-pict-drawer pict))
                    (define hl-drawer (make-pict-drawer (hl pict)))
                    (define (draw-fn dc dx dy)
                      (define-values (bx by bw bh)
                        (transform-rectangle dc dx dy (pict-width pict) (pict-height pict)))
                      (register-presentation object type object->pict dx dy)
                      (if (active? object)
                          (hl-drawer dc dx dy)
                          (drawer dc dx dy)))
                    (make-pict `(prog ,draw-fn ,(pict-height pict))
                               (pict-width pict) (pict-height pict)
                               (pict-ascent pict) (pict-descent pict)
                               (pict-children pict)
                               (pict-panbox pict)
                               (pict-last pict))))))

    (define/public (make-presentation object type obj->pict hl)
      (presented-pict object type obj->pict hl))


    (define (find-presentations x y)
      (define ((presentation-covers? x y) p)
        (and (<= (pict-presentation-x p)
                 x
                 (+ (pict-presentation-x p)
                    (pict-width (pict-presentation-pict p))))
             (<= (pict-presentation-y p)
                 y
                 (+ (pict-presentation-y p)
                    (pict-height (pict-presentation-pict p))))))
      (define (presentation-area pres)
        (define pict (pict-presentation-pict pres))
        (* (pict-width pict) (pict-height pict)))
      (sort (filter (presentation-covers? x y) presentations)
            <
            #:key presentation-area))

    (define/public (find-current-presentation x y)
      (define accepting (send presentation-context currently-accepting))
      (define (presentation-interesting? p)
        (or (not accepting)
            (presentation-has-type? p accepting)))
      (define res
        (let* ([ps (find-presentations x y)])
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

    (define/public (draw-picts dc dx dy)
      (define old-transformation (send dc get-transformation))
      (define old-smoothing (send dc get-smoothing))
      (send dc translate dx dy)
      (send dc set-smoothing 'aligned)
      (set! presentations null)
      (for ([img+location picts])
        (match-let ([(pos x y p) img+location])
          (draw-pict p dc x y)))
      (send dc set-smoothing old-smoothing)
      (send dc set-transformation old-transformation))

    (define/public (show-popup-menu menu x y)
      (void))

    (define/public (handle-mouse-event ev dx dy)
      (define mouse-x #f)
      (define mouse-y #f)
      ;; Update mouse coordinates
      (cond [(or (send ev moving?) (send ev entering?) (send ev button-down?))
             (set! mouse-x (- (send ev get-x) dx))
             (set! mouse-y (- (send ev get-y) dy))])
      ;; If the mouse coordinates are valid then update the presentation context
      (when (and mouse-x mouse-y)
        (cond
          [(or (send ev moving?) (send ev entering?))
           (queue-callback
            (thunk (let ([p (find-current-presentation mouse-x mouse-y)])
                     (if p
                         (send presentation-context make-active p)
                         (send presentation-context nothing-active))
                     (send this after-draw))))]
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
                 (send this show-popup-menu menu mouse-x mouse-y))))])))))

(define presentation-pict-canvas%
  (class* (pict-presenter-mixin canvas%)
    (presenter<%>)
    (init [style null])
    (super-new [style (cons 'no-autoclear style)]
               [paint-callback
                (lambda (canvas dc)
                  (send canvas suspend-flush)
                  (send dc clear)
                  (send this draw-picts dc 0 0)
                  (send canvas resume-flush))])

    (inherit find-current-presentation
             handle-mouse-event)

    (define/override (on-superwindow-show shown?)
      (send this refresh-now))

    (define/override (on-event ev)
      (handle-mouse-event ev 0 0))

    (define/override (after-draw)
      (queue-callback (thunk (send this refresh))))

    (define/override (show-popup-menu menu x y)
      (send this popup-menu menu x y))))



