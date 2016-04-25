#lang racket
(require racket/gui)
(require "ship.rkt")
(require "game.rkt")

(define *game-window* (new frame%
                           [label "Asteroids"]
                           [width 1920]
                           [height 1080]))

(define *player-1*
  (make-object ship%))

(define *game-physics*
  (new game%
    [player *player-1*]))

(define game-canvas%
  (class canvas%
    (init-field [keyboard-handler display])
    (define/override (on-char key-event)
      (keyboard-handler key-event))
    (super-new)))

(define *my-game-canvas*
  (new game-canvas%
       [parent *game-window*]
       [paint-callback (lambda (canvas dc)
                         (send *game-physics* render dc))]
       [keyboard-handler (lambda (key-event)
                           (let ((key-code (send key-event get-key-code)))
                             (if (not (equal? key-code 'release))
                                 (send *player-1* move key-code)
                                 (void))))]))

(define *game-timer* (new timer%
                          [notify-callback (lambda ()
                                             (send *my-game-canvas* refresh))]))
(define (start-game)
  (send *game-window* show #t)
  (send *game-timer* start 16)
  (send *my-game-canvas* focus))