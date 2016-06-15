#lang racket
(require "../presentation.rkt")
(require (for-syntax syntax/parse))

(provide define-command command-lambda command/p
         (struct-out command-argument)
         command-name command-arguments)

(struct command-argument (name type) #:transparent)
(struct command (name arguments implementation)
  #:transparent
  #:property prop:procedure (struct-field-index implementation)
  #:methods gen:custom-write
  [(define (write-proc cmd port mode)
     (fprintf port "#<command:~a>" (command-name cmd)))])

(begin-for-syntax
  (define-syntax-class command-argument
    (pattern (name:id type:expr))))

(define-syntax (define-command stx)
  (syntax-parse stx
    [(_ (cmd:id arguments:command-argument ...)
        body:expr ...)
     #'(define cmd
         (command 'cmd
                  (list (command-argument 'arguments.name arguments.type) ...)
                  (contract
                   (-> (presentation-type/c arguments.type)
                       ...
                       any/c)
                   (lambda (arguments.name ...)
                     body ...)
                   'command
                   'other
                   'cmd
                   #f)))]
    [(_ cmd:id body:expr)
     #'(define-command (cmd) body:expr)]))

(define-syntax (command-lambda stx)
  (syntax-parse stx
    [(_ (arguments:command-argument ...)
        body:expr ...)
     (with-syntax ([name (gensym 'anonymous-command)])
       #'(command 'name
                  (list (command-argument 'arguments.name arguments.type) ...)
                  (contract
                   (-> (presentation-type/c arguments.type)
                       ...
                       any/c)
                   (lambda (arguments.name ...)
                     body ...)
                   'command
                   'other
                   'name
                   #f)))]))

(define command/p
  (make-presentation-type
   'command/p
   #:equiv? (lambda (x y)
              (and (command? x)
                   (command? y)
                   (eq? x y)))
   #:empty-set seteq))

(define not-supplied
  ((lambda ()
     (struct not-supplied ()
       #:methods gen:custom-write
       [(define (write-proc x port mode)
          (fprintf port "#<not-supplied>"))])
     (not-supplied))))

(define (supplied? x)
  (not (eq? x not-supplied)))

(struct command-application-argument (value type) #:transparent)

(struct command-application (command arguments) #:transparent)

(define (make-command-application cmd)
  (command-application
   cmd
   (for/list ([a (command-arguments cmd)])
     (command-application-argument not-supplied
                                   (command-argument-type a)))))
