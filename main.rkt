#lang racket

(require racket/gui framework)
(require pict)
(require "private/presentation.rkt"
         "private/presentation/text.rkt"
         "private/presentation/pict.rkt"
         "private/presentation/presentation-pict-snip.rkt"
         "private/presentation/repl.rkt")

(module+ test
  (require rackunit))

(provide presentation<%> presenter<%>
         presentation-context<%> current-presentation-context
         presentation-text% presentation-string<%>
         pstring pstring-append pstring-annotate
         presentation-pict-canvas%
         presentation-pict-snip%
         presentation-repl%
         pict-presenter<%>
         make-presentation-type
         presentation-type?)

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco doc <<name>>
;;
;; For your convenience, we have included a LICENSE.txt file, which links to
;; the GNU Lesser General Public License.
;; If you would prefer to use a different license, replace LICENSE.txt with the
;; desired license.
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html

;; Code here

(define green (make-object color% 0 255 0 1))
(define red (make-object color% 255 0 0 1))


(module+ test
  ;; Tests to be run with raco test
  )


(module+ main
  ;; The semantic domain
  (struct icthyoid (text) #:transparent)
  (define my-fish (icthyoid "the fish"))
  (define your-fish (icthyoid "not the fish"))

  (define fish/p (make-presentation-type 'fish/p))
  (define not-fish/p (make-presentation-type 'not-fish/p))

  ;; The GUI
  (define frame (new frame% [label "hey"] [width 800] [height 900]))
  (define split (new vertical-panel% [parent frame]))
  (define toolbar (new horizontal-panel% [parent split] [stretchable-height #f]))
  (define content (new panel:vertical-dragable% [parent split]))

  ;; Canvas construction
  (define canvas
    (new presentation-pict-canvas% [parent content]))

  (define (hl p)
    (cc-superimpose p (inset (cellophane (scale p 1.5) 0.25) -30)))

  (define circle
    (send canvas make-presentation 'circle not-fish/p
          (filled-ellipse 50 50)
          hl))
  (define rect (filled-rectangle 23 90))
  (define fish
    (send canvas make-presentation my-fish fish/p
          (standard-fish 50 30 #:color "orange")
          hl))
  (define another-fish
    (send canvas make-presentation my-fish fish/p
          (standard-fish 50 40 #:color "blue")
          hl))
  (define more-fish
    (send canvas make-presentation your-fish fish/p
          (standard-fish 55 40 #:color "brown")
          hl))
  (send canvas add-pict circle 100 100)
  (send canvas add-pict rect 400 200)
  (send canvas add-pict fish 200 400)
  (send canvas add-pict another-fish 400 400)
  (send canvas add-pict more-fish 300 300)

  (send canvas add-pict (standard-fish 10 10) 20 40)

  ;; Text construction
  (define editor (new presentation-text% [auto-wrap #t]))
  (define editor-canvas (new editor-canvas% [parent content] [editor editor]))

  (define text-fish (pstring-append (pstring "I am a big fan of the ")
                                    (pstring-annotate my-fish fish/p (pstring "Fish!"))
                                    (pstring " because it is great.")))

  (define looong (pstring-append (pstring "hello ") (pstring "hello ") (pstring "hello ")
                                 (pstring "hello ") (pstring "hello ") (pstring "hello ")
                                 (pstring "hello ") (pstring "hello ") (pstring "hello ")
                                 (pstring "hello ") (pstring "hello ") (pstring "hello ")
                                 (pstring "hello ") (pstring "hello ") (pstring "hello ")
                                 (pstring "hello ") (pstring "hello ") (pstring "hello ")
                                 (pstring "hello ") (pstring "hello ") (pstring "hello ")
                                 (pstring "hello ") (pstring "hello ") (pstring "hello ")
                                 (pstring "hello ") (pstring "hello ") (pstring "hello ")
                                 (pstring "hello ") (pstring "hello ") (pstring "hello ")
                                 (pstring "hello ") (pstring "hello ") (pstring "hello ")
                                 (pstring-annotate
                                  your-fish fish/p
                                  (pstring-append (pstring "F")
                                                  (pstring (build-string 300 (thunk* #\I)))
                                                  (pstring "SH")))
                                 (pstring "!!!!!!!!!!!!!!!!!")))
  (send editor insert-presenting text-fish)
  (send editor insert-presenting looong)

  (send (current-presentation-context) register-command-translator fish/p
        (lambda (obj)
          (list (list "Which fish?" (thunk (displayln obj))))))

  ;; Major commands
  (define fish-button (new button%
                           [parent toolbar]
                           [label "Identify fish"]
                           [callback
                            (thunk*
                             (send frame set-status-text "Accepting fish")
                             (send (current-presentation-context)
                                   accept fish/p
                                   (lambda (o)
                                     (send frame set-status-text "")
                                     (displayln o))))]))
  (send frame create-status-line)
  (send frame show #t))
