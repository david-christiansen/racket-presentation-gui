#lang racket

(require racket/gui framework)
(require pict)
(require "private/presentation.rkt"
         "private/presentation/text.rkt")

(require "inspector.rkt")

(define presentation-repl%
  (class* presentation-text%
    (presenter<%>)

    (init-field eval-callback
                [prompt-string ">"])
    (super-new)

    (inherit get-text
             insert
             insert-presenting
             last-position
             set-position)

    (define previous-input-beginning-position 0)
    (define input-beginning-position 0)
    (define locked? #t)

    (define/public (set-prompt new-prompt-string)
      (set! prompt-string new-prompt-string))

    (define/augment (can-insert? start len)
      (and (>= start input-beginning-position)
           (not locked?)))

    (define/override (on-char c)
      (if (and (eq? (send c get-key-code)
                    #\return)
               (not locked?))
          (begin
            (set-position (last-position))
            (super on-char c)
            (set! locked? #t)
            (set! previous-input-beginning-position input-beginning-position)
            (let ([input (get-text input-beginning-position
                                   (- (last-position) 1))])
              (queue-callback
               (thunk (output (eval-callback input))
                      (insert-prompt)))))
          (super on-char c)))

    (define (insert-prompt)
      (queue-callback
       (thunk (set! locked? #f)
              (set-position (last-position))
              (unless (or (= (last-position) 0)
                          (string=? (get-text (- (last-position) 1) (last-position))
                                    "\n"))
                (insert "\n"))
              (insert prompt-string)
              (insert " ")
              (set! input-beginning-position (last-position)))))

    (define/public (output str)
      (queue-callback
       (thunk
        (let ((was-locked? locked?)
              (insertion-base (last-position)))
          (set! locked? #f)
          (if (is-a? str presentation-string<%>)
              (insert-presenting str)
              (insert str))
          (set! locked? was-locked?)))))

    (queue-callback insert-prompt)))

(define (intersperse sep lst)
  (cond
    [(null? lst) null]
    [(and (pair? lst) (null? (cdr lst)))
     lst]
    [else (cons (car lst) (cons sep (intersperse sep (cdr lst))))]))

(define (present-exn exn)
  (let ([msg (exn-message exn)]
        [trace (continuation-mark-set->context (exn-continuation-marks exn))])
    (pstring-annotate
     exn 'exn
     (apply pstring-append (pstring msg)
            (pstring "\n")
            (for/list ([frame trace])
              (apply pstring-append
                     (append (if (car frame)
                                 (list (pstring (symbol->string (car frame))))
                                 null)
                             (if (and (car frame) (cdr frame))
                                 (list (pstring ": "))
                                 null)
                             (if (cdr frame)
                                 (list (pstring-annotate (cdr frame) 'srcloc
                                                         (pstring (srcloc->string (cdr frame)))))
                                 null)
                             (list (pstring "\n")))))))))

(define (pretty-present object)
  (define width-preference 80)
  (define (width str)
    (apply max (for/list ([line (string-split str "\n")])
                 (string-length line))))
  (define (pretty-present-sequence xs start-col)
    (define presented-xs
      (for/list ([x xs])
        (real-pretty-present x
                             (+ start-col 2))))
    (define break? (> (+ start-col
                         (length xs)
                         (apply + (for/list ([p presented-xs])
                                    (send p get-length))))
                      width-preference))
    (define contents
      (if break?
          (intersperse (pstring (string-append "\n"
                                               (build-string start-col
                                                             (thunk* #\space))))
                       presented-xs)
          (intersperse (pstring " ") presented-xs)))
    (apply pstring-append contents))

  (define (real-pretty-present obj start-col)
    (match obj
      [(? null? x)
       (pstring-annotate x value/p (pstring "'()"))]
      [(list xs ...)
       (define contents (pretty-present-sequence xs (+ start-col 2)))
       (define start (pstring "'("))
       (define end (pstring ")"))
       (pstring-annotate obj value/p (pstring-append start contents end))]
      [(vector xs ...)
       (define contents (pretty-present-sequence xs (+ start-col 2)))
       (define start (pstring "#("))
       (define end (pstring ")"))
       (pstring-annotate obj value/p (pstring-append start contents end))]
      [other (pstring-annotate obj value/p (pstring (format "~v" other)))]))
  (real-pretty-present object 0))

(module+ main
  (send (current-presentation-context) register-command-translator
        value/p
        (lambda (val)
          (list (list "Inspect value" (thunk (gui-inspect val))))))

  (define (rep str)
    (with-handlers ([exn? present-exn])
      (define result (eval (with-input-from-string str (thunk (read)))
                           (make-base-namespace)))
      (pretty-present result)))

  (define frame (new frame% [label "REPL"] [width 800] [height 600]))
  (define repl (new presentation-repl%
                    [eval-callback rep]))
  (define editor-canvas (new editor-canvas%
                             [parent frame]
                             [editor repl]))
  (send frame show #t))
