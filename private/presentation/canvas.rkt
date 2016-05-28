#lang racket/base

(require racket/gui/base racket/class racket/match racket/set)
(require pict)

(require "../presentation.rkt")

(provide img<%> circle% rectangle% pict-img% presentation-img%
         presentation-canvas%)

(define img<%>
  (interface ()
    [draw (->m (is-a?/c dc<%>) real? real? void?)]
    [draw-hl (->m (is-a?/c dc<%>) real? real? void?)]
    [get-width (->m real?)]
    [get-height (->m real?)]
    [occludes? (->m real? real? boolean?)]))

(define circle%
  (class* object% (img<%>)
    (init-field radius [color "black"] [hl-color "red"])
    (super-new)

    (define/public (draw dc x y)
      (send dc set-brush color 'opaque)
      (send dc draw-ellipse x y (* radius 2) (* radius 2)))

    (define/public (draw-hl dc x y)
      (send dc set-brush hl-color 'opaque)
      (send dc draw-ellipse (- x 2) (- y 2) (+ (* radius 2) 4) (+ (* radius 2) 4)))

    (define/public (get-width)
      (* radius 2))

    (define/public (get-height)
      (* radius 2))

    (define/public (occludes? x y)
      (let ([center-x radius]
            [center-y radius])
        (< (sqrt (+ (expt (- x center-x) 2)
                    (expt (- y center-y) 2)))
           radius)))))

(define rectangle%
  (class* object% (img<%>)
    (init-field width height [color "black"] [hl-color "red"])
    (super-new)

    (define/public (draw dc x y)
      (send dc set-brush color 'opaque)
      (send dc draw-rectangle x y width height))

    (define/public (draw-hl dc x y)
      (send dc set-brush hl-color 'opaque)
      (send dc draw-rectangle (- x 2) (- y 2) (+ width 4) (+ height 4)))

    (define/public (get-width)
      width)

    (define/public (get-height)
      height)

    (define/public (occludes? x y)
      (and (>= x 0) (<= x width)
           (>= y 0) (<= y height)))))

(define pict-img%
  (class* object% (img<%>)
    (init-field pict hl-pict)
    (super-new)
    (define/public (draw dc x y)
      (draw-pict pict dc x y))
    (define/public (draw-hl dc x y)
      (draw-pict hl-pict dc x y))
    (define/public (get-width)
      (pict-width pict))
    (define/public (get-height)
      (pict-height pict))

    (define crazy-color-1 (make-object color% 35 35 99 1.0))
    (define crazy-color-2 (make-object color% 35 200 200 1.0))
    (define occlusion-bitmap-dc-1
      (let* ([bitmap-1 (make-screen-bitmap (pict-width pict) (pict-height pict))]
             [dc-1 (new bitmap-dc% [bitmap bitmap-1])])
        (send* dc-1
          (set-background crazy-color-1)
          (clear))
        (draw-pict pict dc-1 0 0)
        dc-1))

    (define occlusion-bitmap-dc-2
      (let* ([bitmap-2 (make-screen-bitmap (pict-width pict) (pict-height pict))]
             [dc-2 (new bitmap-dc% [bitmap bitmap-2])])
        (send* dc-2
          (set-background crazy-color-2)
          (clear))
        (draw-pict pict dc-2 0 0)
        dc-2))


    (define/public (occludes? x y)
      (let* ([color-1 (make-object color%)]
             [color-2 (make-object color%)]
             [ok1 (send occlusion-bitmap-dc-1 get-pixel x y color-1)]
             [ok2 (send occlusion-bitmap-dc-2 get-pixel x y color-2)])
        (and ok1 ok2
             (not (= (send color-1 red) (send crazy-color-1 red)))
             (not (= (send color-1 green) (send crazy-color-1 green)))
             (not (= (send color-1 blue) (send crazy-color-1 blue)))
             (not (= (send color-2 red) (send crazy-color-2 red)))
             (not (= (send color-2 green) (send crazy-color-2 green)))
             (not (= (send color-2 blue) (send crazy-color-2 blue))))))))



(define presentation-img%
  (class* object% (img<%> presentation<%>)
    (init-field object modality img)
    (super-new)
    (define/public (draw dc x y) (send img draw dc x y))
    (define/public (draw-hl dc x y) (send img draw-hl dc x y))
    (define/public (get-width) (send img get-width))
    (define/public (get-height) (send img get-height))
    (define/public (occludes? x y) (send img occludes? x y))

    (define/public (get-presented-object) object)
    (define/public (get-modality) modality)))



(define presentation-canvas%
  (class* canvas%
    (presenter<%>)

    (init-field [presentation-context #f])
    (super-new [style '(no-autoclear)])
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
      (send this refresh))

    (define mouse-x #f)
    (define mouse-y #f)
    (define active (mutable-seteq))

    ;; A list of presented objects.  Each presented object is a list
    ;; of a presentation type, the presented object, and its bounding
    ;; box.
    (define presented '())

    (define imgs '())

    (define/public (add-img img x y)
      (set! imgs (cons (list img x y) imgs)))

    (define/override (on-paint)
      (send buffer-dc clear)
      (for ([img+location imgs])
        (match-let ([(list i x y) img+location])
          (if (set-member? active i)
              (send i draw-hl buffer-dc x y)
              (send i draw buffer-dc x y))))
      (send (send this get-dc) draw-bitmap buffer 0 0)
      (void))

    (define/public (activate obj)
      (let ([obj-presentations
             (for*/seteq ([img+location imgs]
                          [i (list (car img+location))]
                          #:when (is-a? i presentation<%>)
                          #:when (eq? obj (send i get-presented-object)))
                         i)])
        (when (not (set=? obj-presentations active))
          (set! active obj-presentations)
          (send this refresh))))

    (define/public (deactivate)
      (set! active (mutable-seteq))
      (send this refresh))

    (define/public (new-context-state st)
      (void))

    (define (current-object)
      (define context presentation-context)
      (define state (send context get-state))

      ;; TODO: move to context?
      (define (object-acceptable? obj [m #f])
        (match state
          [(list 'accepting pred _)
           (pred obj m)]
          [#f #t]))

      (define (most-specific p1 p2)
        (if (> (* (send p1 get-width) (send p1 get-height))
               (* (send p2 get-width) (send p2 get-height)))
            p2
            p1))

      (define candidates
        (filter (lambda (x) x)
                (for/list ([img+location imgs])
                  (match-let ([(list i x y) img+location])
                    (if (and mouse-x mouse-y
                             (is-a? i presentation<%>)
                             (send i occludes? (- mouse-x x) (- mouse-y y))
                             (object-acceptable? (send i get-presented-object)
                                                 (send i get-modality)))
                        (cons i (send i get-presented-object))
                        #f)))))

      ;; Pick the most specific activation candidate, if possible
      (if (null? candidates)
          #f
          (let loop ([best (car candidates)]
                     [remaining (cdr candidates)])
            (if (null? remaining)
                best
                (loop (most-specific (car best) (caar remaining))
                      (cdr remaining))))))

    (define/override (on-event ev)
      ;; Update mouse coordinates
      (cond [(or (send ev moving?) (send ev entering?) (send ev button-down?))
             (set! mouse-x (send ev get-x))
             (set! mouse-y (send ev get-y))]
            [(send ev leaving?)
             (set! mouse-x #f)
             (set! mouse-y #f)])
      ;; Handle event logic
      (cond [(or (send ev moving?) (send ev entering?))
             (let ([obj (current-object)])
               (if obj
                   (send presentation-context make-active (cdr obj))
                   (send presentation-context nothing-active)))]
            [(and (send presentation-context accepting?) (send ev button-down?))
             (let ([obj (current-object)])
               (send presentation-context accepted (cdr obj)))]
            [(send ev button-down? 'right)
             (let ([obj (current-object)]
                   [menu (new popup-menu%)])
               (when obj
                 (define cmds (send presentation-context commands-for
                                    (car obj)))
                 (when (not (null? cmds))
                   (for ([cmd cmds])
                     (new menu-item%
                          [label (car cmd)]
                          [parent menu]
                          [callback
                           (lambda args (queue-callback (cadr cmd)))]))
                   (send this popup-menu menu (send ev get-x) (send ev get-y)))))]))))
