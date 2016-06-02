#lang racket/base

(require racket/class racket/contract racket/match racket/set)
(require racket/gui/base)
(require "../presentation.rkt")

(provide presentation-string<%>
         (contract-out
          [pstring (-> string? (is-a?/c presentation-string<%>))]
          [pstring-append
           (->* () #:rest (listof (is-a?/c presentation-string<%>))
                (is-a?/c presentation-string<%>))]
          [pstring-annotate
           (-> any/c any/c (is-a?/c presentation-string<%>)
               (is-a?/c presentation-string<%>))])
         presentation-text%)

(define presentation-string<%>
  (interface ()
    [get-string (->m string?)]
    [get-length (->m exact-nonnegative-integer?)]
    [get-presentations (->m (listof (list/c exact-nonnegative-integer?
                                            exact-nonnegative-integer?
                                            any/c
                                            any/c)))]))

(define presentation-string%
  (class* object%
    (presentation-string<%>)
    (init-field string)
    (super-new)
    (define len (string-length string))
    (define/public (get-string) string)
    (define/public (get-length) len)
    (define/public (get-presentations) '())))

(define presentation-string-append%
  (class* object%
    (presentation-string<%>)
    (init-field strings)
    (super-new)
    (define len
      (apply + (for/list ([str strings])
                 (send str get-length))))
    (define/public (get-string)
      (apply string-append
             (for/list ([str strings])
               (send str get-string))))
    (define/public (get-length) len)
    (define/public (get-presentations)
      (define length 0)
      (apply append
             (for*/list ([str (in-sequences strings)])
               (define result
                 (for/list ([pres (send str get-presentations)])
                   (match-define (list offset len object modality) pres)
                   (list (+ offset length) len object modality)))
               (set! length (+ length (string-length (send str get-string))))
               result)))))

(define presentation-of-string%
  (class* object%
    (presentation-string<%>)
    (super-new)
    (init-field string object modality)
    (define len (send string get-length))
    (define/public (get-string)
      (send string get-string))
    (define/public (get-length)
      len)
    (define/public (get-presentations)
      (cons (list 0 len object modality) (send string get-presentations)))))

(define (pstring str)
  (new presentation-string% [string str]))
(define (pstring-append . strs)
  (new presentation-string-append% [strings strs]))
(define (pstring-annotate object modality str)
  (new presentation-of-string% [string str] [object object] [modality modality]))

(define presentation-text%
  (class* text%
    (presenter<%>)
    (init-field [presentation-context #f])
    (super-new)
    (unless presentation-context
      (set! presentation-context (current-presentation-context)))
    (send presentation-context register-presenter this)

    (define active-presentations (seteq))

    ;; TODO: less-dumb data structure
    ;; For now, a list of lists containing offset, length, object, modality
    (define presented-objects '())

    (define/augment (can-insert? start len)
      (and (not (send presentation-context accepting?))
           (inner #t can-insert? start len)))

    (define/augment (can-delete? start len)
      (and (not (send presentation-context accepting?))
           (inner #t can-delete? start len)))

    ;; Maintain the presented-objects map
    (define/augment (on-delete start len)
      (define end (+ start len))
      (set! presented-objects
            (for/list ([presented presented-objects])
              (match-define (list obj-start obj-len object modality) presented)
              (define obj-end (+ obj-start obj-len))
              (cond
                [(< start obj-start)
                 (if (<= end obj-start)
                     (list (- obj-start len) obj-len object modality)
                     (list start (- obj-len (- end obj-start)) object modality))]
                [(= start obj-start)
                 (list start (max (- obj-len len) 0) object modality)]
                [(> start obj-start)
                 (cond [(>= start obj-end) ;; after end, no worries
                        presented]
                       [(< end obj-end) ;; we removed from the middle
                        (list obj-start (- obj-len len) object modality)]
                       [else ;; overlap with end
                        (list obj-start (- start obj-start) object modality)])]))))
    (define/augment (on-insert start len)
      (define end (+ start len))
      (set! presented-objects
            (for/list ([presented presented-objects])
              (match-define (list obj-start obj-len object modality) presented)
              (define obj-end (+ obj-start obj-len))
              (cond [(<= start obj-start)
                     (list (+ obj-start len) obj-len object modality)]
                    [(>= start obj-end)
                     presented]
                    [else
                     (list obj-start (+ obj-len len) object modality)]))))

    (define/public (insert-presenting pstring [start #f])
      (unless start
        (set! start (let ([b (box 0)])
                      (send this get-position b)
                      (unbox b))))
      (define str (send pstring get-string))
      (define pres (send pstring get-presentations))
      (send this insert str start)
      (set! presented-objects
            (append (for/list ([p pres])
                      (match-define (list obj-start obj-len object modality) p)
                      (list (+ obj-start start) obj-len object modality))
                    presented-objects)))


    (define/override (on-paint before?
                               dc
                               left
                               top
                               right
                               bottom
                               dx
                               dy
                               draw-caret)
      (super on-paint before? dc left top right bottom dx dy draw-caret)
      (unless before?
        (define old-brush (send dc get-brush))
        (define old-pen (send dc get-pen))
        (send dc set-brush "white" 'transparent)
        (send dc set-pen (make-object color% 200 30 0 0.3) 5 'solid)
        (for ([p active-presentations])
          (match-define (list start len object modality) p)
          (define relevant-lines
            (in-range (send this position-line start)
                      (add1 (send this position-line (+ start len)))))
          (for ([line relevant-lines])
            (define line-start (send this line-start-position line))
            (define line-end (send this line-end-position line))
            (define x-begin (box 0.0))
            (define y-begin (box 0.0))
            (define x-end (box 0.0))
            (define y-end (box 0.0))
            (define hl-start-pos (max line-start start))
            (define hl-end-pos (min line-end (+ start len)))
            (when (> hl-end-pos hl-start-pos) ;; needed to deal with newline at end of presentation
              (send this position-location hl-start-pos x-begin y-begin #t #f)
              (send this position-location hl-end-pos x-end y-end #f #t)
              (send dc draw-rectangle
                    (+ (unbox x-begin) dx) (+ (unbox y-begin) dy)
                    (- (unbox x-end) (unbox x-begin)) (- (unbox y-end) (unbox y-begin))))))
        (send dc set-brush old-brush)
        (send dc set-pen old-pen)))

    (define/public (activate obj)
      (set! active-presentations
            (for/seteq ([p presented-objects]
                        #:when (eq? (caddr p) obj))
              p))
      (send this invalidate-bitmap-cache))

    (define/public (deactivate)
      (set! active-presentations (seteq))
      (send this invalidate-bitmap-cache))

    (define/public (new-context-state st)
      (match st
        [(list 'accepting _ _)
         (send this set-cursor (make-object cursor% 'arrow) #t)]
        [_
         (send this set-cursor #f #f)]))

    (define (object-at x y)
      (define (smallest a b)
        (if (< (cadr a) (cadr b)) a b))
      (let* ([pos (send this find-position x y)]
             [candidates
              (for/list ([p presented-objects]
                         #:when (and (>= pos (car p))
                                     (< pos (+ (car p) (cadr p)))))
                p)])
        (if (null? candidates)
            #f
            (let loop ([best (car candidates)]
                       [remaining (cdr candidates)])
              (if (null? remaining)
                  best
                  (loop (smallest best (car remaining))
                        (cdr remaining)))))))

    (define/override (on-default-event ev)
      (super on-default-event ev)
      (define-values (x y)
        (send this dc-location-to-editor-location (send ev get-x) (send ev get-y)))
      (cond [(or (send ev moving?) (send ev entering?))
             (let ([obj (object-at x y)])
               (if obj
                   (send presentation-context make-active (caddr obj))
                   (send presentation-context nothing-active)))]
            [(and (send presentation-context accepting?) (send ev button-down?))
             (let ([obj (object-at x y)])
               (when obj
                 (send presentation-context accepted (caddr obj))))]
            [(send ev button-down? 'right)
             (let ([obj (object-at x y)]
                   [menu (new popup-menu%)])
               (when obj
                 (define cmds (send presentation-context commands-for
                                    (make-object simple-presentation%
                                                 (caddr obj)
                                                 (cadddr obj))))
                 (when (not (null? cmds))
                   (for ([cmd cmds])
                     (new menu-item%
                          [label (car cmd)]
                          [parent menu]
                          [callback
                           (lambda args (queue-callback (cadr cmd)))]))
                   (send (send this get-admin)
                         popup-menu menu (send ev get-x) (send ev get-y)))))]))))
