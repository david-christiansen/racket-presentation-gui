#lang racket/base

(require racket/gui/base)
(require racket/class racket/contract racket/match racket/set)

(provide presentation<%> simple-presentation% presenter<%>
         presentation-context<%>
         (contract-out [current-presentation-context
                        (parameter/c (is-a?/c presentation-context<%>))]))

(define presentation<%>
  (interface ()
    [get-presented-object (->m any/c)]
    [get-modality (->m any/c)]))

(define simple-presentation%
  (class* object% (presentation<%>)
    (init-field object modality)
    (super-new)
    (define/public (get-presented-object) object)
    (define/public (get-modality) modality)))

(define presenter<%>
  (interface ()
    ;; Presentations of this object should be shown in an active state
    [activate (->m any/c void?)]
    ;; Deactivae all
    [deactivate (->m void?)]
    ;; Listen for state changes
    [new-context-state (->m any/c void?)]))

(define presentation-context<%>
  (interface ()
    [get-state (->m (or/c #f
                          (list/c 'accepting (-> any/c any/c any/c) (-> any/c void?))))]
    [accepting? (->m boolean?)]
    [accept (->m (-> any/c any/c any/c) (-> any/c void?) void?)]
    [make-active (->m any/c void?)]
    [nothing-active (->m void?)]
    [accepted (->m any/c void?)]
    [register-command-translator (->m (-> any/c
                                          any/c
                                          (listof (list/c string? (-> any/c void?))))
                                      void?)]
    [commands-for (->m (is-a?/c presentation<%>)
                       (listof (list/c string? (-> any/c void?))))]))

;;; A presentation context manages the global application presentation
;;; state, including:
;;;
;;; 1. Whether the application is currently accepting a presentation,
;;;    and if so, what kinds.
;;;
;;; 2. The collection of presented objects
(define presentation-context%
  (class* object% (presentation-context<%>)
    (super-new)
    ;; The state can be:
    ;; - #f, the default state
    ;; - `(accepting ,pred? ,action)
    (define state #f)

    (define/public (get-state)
      state)

    (define (set-state new-state)
      (set! state new-state)
      (for ([p (in-set presenters)])
        (send p new-context-state new-state)))

    (define/public (accepting?)
      (match state
        [(list-rest 'accepting _) #t]
        [_ #f]))

    (define/public (accept pred action)
      (set-state `(accepting ,pred ,action)))

    (define presenters (weak-seteq))
    (define/public (register-presenter presenter)
      (set-add! presenters presenter))

    ;; Instruct all registered presenters to show obj as active
    (define/public (make-active obj)
      (for ([p (in-set presenters)])
        (send p activate obj)))

    (define/public (nothing-active)
      (for ([p (in-set presenters)])
        (send p deactivate)))

    (define/public (accepted obj)
      (match state
        [(list 'accepting pred act)
         (act obj)
         (set-state #f)]
        [_ (raise-argument-error 'accepted "Not accepting" obj)]))

    ;; Presentations can be implictly considered as commands, which
    ;; will be presented in a menu.  A presentation-to-command
    ;; translator is a procedure that, when given a presentation
    ;; object and modality, returns a possibly-empty list of command
    ;; name and thunk lists.
    (define command-translators '())
    (define/public (register-command-translator proc)
      (set! command-translators (append command-translators (list proc))))
    (define/public (commands-for presentation)
      (for*/list ([translator command-translators]
                  [command (translator (send presentation get-presented-object)
                                       (send presentation get-modality))])
        command))))

(define current-presentation-context
  (make-parameter (new presentation-context%)))
