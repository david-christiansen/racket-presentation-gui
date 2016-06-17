#lang racket

(require racket/gui)
(require "../presentation.rkt"
         "text.rkt")

(provide presentation-repl%)

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
