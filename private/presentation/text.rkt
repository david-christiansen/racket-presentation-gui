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
           (->i ([value (type) (presentation-type/c type)]
                 [type presentation-type?]
                 [str (is-a?/c presentation-string<%>)])
                ()
                [result (is-a?/c presentation-string<%>)])])
         presentation-text%)

(struct textual-presentation (offset len value type)
  #:property prop:presentation
  (list (lambda (x) (textual-presentation-value x))
        (lambda (x) (textual-presentation-type x))))

(define presentation-string<%>
  (interface ()
    [get-string (->m string?)]
    [get-length (->m exact-nonnegative-integer?)]
    [get-presentations (->m (listof textual-presentation?))]))

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
                   (match-define (textual-presentation offset len object presentation-type)
                     pres)
                   (textual-presentation (+ offset length) len object presentation-type)))
               (set! length (+ length (string-length (send str get-string))))
               result)))))

(define presentation-of-string%
  (class* object%
    (presentation-string<%>)
    (super-new)
    (init-field string object presentation-type)
    (define len (send string get-length))
    (define/public (get-string)
      (send string get-string))
    (define/public (get-length)
      len)
    (define/public (get-presentations)
      (cons (textual-presentation 0 len object presentation-type) (send string get-presentations)))))

(define (pstring str)
  (new presentation-string% [string str]))
(define (pstring-append . strs)
  (new presentation-string-append% [strings strs]))
(define (pstring-annotate object presentation-type str)
  (new presentation-of-string% [string str] [object object] [presentation-type presentation-type]))


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
    ;; For now, a list of lists containing offset, length, object, presentation-type
    (define presented-objects '())

    (define/augment (can-insert? start len)
      (and (not (send presentation-context currently-accepting))
           (inner #t can-insert? start len)))

    (define/augment (can-delete? start len)
      (and (not (send presentation-context currently-accepting))
           (inner #t can-delete? start len)))

    ;; Maintain the presented-objects map
    (define/augment (on-delete start len)
      (define end (+ start len))
      (set! presented-objects
            (for/list ([presented presented-objects])
              (match-define (textual-presentation obj-start obj-len object presentation-type) presented)
              (define obj-end (+ obj-start obj-len))
              (cond
                [(< start obj-start)
                 (if (<= end obj-start)
                     (textual-presentation (- obj-start len)
                                           obj-len
                                           object
                                           presentation-type)
                     (textual-presentation start
                                           (- obj-len (- end obj-start))
                                           object
                                           presentation-type))]
                [(= start obj-start)
                 (textual-presentation start
                                       (max (- obj-len len) 0)
                                       object
                                       presentation-type)]
                [(> start obj-start)
                 (cond [(>= start obj-end) ;; after end, no worries
                        presented]
                       [(< end obj-end) ;; we removed from the middle
                        (textual-presentation obj-start
                                              (- obj-len len)
                                              object
                                              presentation-type)]
                       [else ;; overlap with end
                        (list obj-start
                              (- start obj-start)
                              object
                              presentation-type)])]))))
    (define/augment (on-insert start len)
      (define end (+ start len))
      (set! presented-objects
            (for/list ([presented presented-objects])
              (match-define (textual-presentation obj-start obj-len object presentation-type)
                presented)
              (define obj-end (+ obj-start obj-len))
              (cond [(<= start obj-start)
                     (textual-presentation (+ obj-start len) obj-len object presentation-type)]
                    [(>= start obj-end)
                     presented]
                    [else
                     (textual-presentation obj-start (+ obj-len len) object presentation-type)]))))

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
                      (match-define (textual-presentation obj-start obj-len object presentation-type)
                        p)
                      (textual-presentation (+ obj-start start) obj-len object presentation-type))
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
          (match-define (textual-presentation start len object presentation-type) p)
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

    (define/public (highlight type value)
      (set! active-presentations
            (for/seteq ([p presented-objects]
                        #:when (presented-object-equal? type
                                                        (presentation-value p)
                                                        value))
                       p))
      (send this invalidate-bitmap-cache))

    (define/public (no-highlighting)
      (set! active-presentations (seteq))
      (send this invalidate-bitmap-cache))

    (define (presentation-at x y)
      (define (smallest a b)
        (if (< (textual-presentation-len a)
               (textual-presentation-len b))
            a
            b))
      (let* ([accepting (send presentation-context currently-accepting)]
             [pos (send this find-position x y)]
             [candidates
              (for/list ([p presented-objects]
                         #:when (and (or (not accepting)
                                         (presentation-has-type? p accepting))
                                     (>= pos (textual-presentation-offset p))
                                     (< pos (+ (textual-presentation-offset p)
                                               (textual-presentation-len p)))))
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
             (let ([pres (presentation-at x y)])
               (if pres
                   (send presentation-context make-active pres)
                   (send presentation-context nothing-active)))]
            [(and (send presentation-context currently-accepting) (send ev button-down?))
             (let ([pres (presentation-at x y)])
               (when pres
                 (send presentation-context accepted pres)))]
            [(send ev button-down? 'right)
             (let ([pres (presentation-at x y)]
                   [menu (new popup-menu%)])
               (when pres
                 (define cmds (send presentation-context commands-for
                                    (simple-presentation
                                     (caddr pres)
                                     (cadddr pres))))
                 (when (not (null? cmds))
                   (for ([cmd cmds])
                     (new menu-item%
                          [label (car cmd)]
                          [parent menu]
                          [callback
                           (lambda args (queue-callback (cadr cmd)))]))
                   (send (send this get-admin)
                         popup-menu menu (send ev get-x) (send ev get-y)))))]))))


(module+ main
  (require racket racket/gui)

  (struct fnord (illuminatus) #:transparent)
  (define fnord-1 (fnord "here"))
  (define fnord-2 (fnord "and here"))

  (define fnord/p (make-presentation-type 'fnord/p))
  (define non-fnord/p (make-presentation-type 'non-fnord/p))

  (define text
    (pstring-append (pstring "It seems that there are ")
                    (pstring-annotate fnord-1 fnord/p (pstring "things we don't know"))
                    (pstring " about ")
                    (pstring-annotate fnord-2 non-fnord/p (pstring "our"))
                    (pstring " ")
                    (pstring-annotate fnord-2 fnord/p (pstring "linguistic environment"))
                    (pstring " and why ")
                    (pstring-annotate fnord-1 fnord/p (pstring "it makes us anxious"))))

  (define f (new frame% [label "Find them!"] [width 800] [height 500]))
  (define b (new button%
                 [label "Specificity, please"]
                 [parent f]
                 [callback (lambda (b e)
                             (send (current-presentation-context) accept
                                   fnord/p displayln))]))
  (define t (new presentation-text% [auto-wrap #t]))
  (define c (new editor-canvas% [parent f] [editor t]))

  (send t insert-presenting text)

  (send f show #t))
