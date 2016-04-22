#lang racket
(require racket/gui)

(define rotating-image%
  (class object%
    (init-field [image (make-bitmap 100 100)]
                [x 0]
                [y 0]
                [angle 0]
                [speed 10])
    (define/public (get-image) image)
    (define/private (move-left)
      (set! angle (+ angle 0.1)))
    (define/private (move-right)
      (set! angle (- angle 0.1)))
    (define/private (move-up)
      (set! y (- y speed)))
    (define/private (move-down)
      (set! y (+ y speed)))
    (define/public (render dc)
      (let ((w (send image get-width))
            (h (send image get-height)))
        (send dc translate (+ x (/ w 2)) (+ y (/ h 2)))
        (send dc rotate angle)
        (send dc draw-bitmap image (- (/ w 2)) (- (/ h 2)))
        (send dc rotate (- angle))
        (send dc translate (- (+ x (/ w 2))) (- (+ y (/ h 2))))))
    (define/public (key-down key-code)
      (cond ((equal? key-code #\w) (move-up))
            ((equal? key-code #\s) (move-down))
            ((equal? key-code #\a) (move-left))
            ((equal? key-code #\d) (move-right))))
    (super-new)))

(define game-canvas%
  (class canvas%
    (init-field [keyboard-handler display])
    (define/override (on-char key-event)
      (keyboard-handler key-event))
    (super-new)))

(define (create-test-image bitmap-target)
  (let ((dc (new bitmap-dc% [bitmap bitmap-target])))
    (send dc set-brush (make-object brush% "white" 'solid))
    (send dc draw-ellipse 10 10 40 40)
    (send dc draw-ellipse 50 10 40 40)
    (send dc set-brush (make-object brush% "red" 'solid))
    (send dc draw-ellipse 25 25 50 50)))
(define *my-rotating-image* (new rotating-image%
                                 [x 100]
                                 [y 50]))
(define *my-window* (new frame%
                         [label "Test"]
                         [width 300]
                         [height 200]))
(define *my-game-canvas*
  (new game-canvas%
       [parent *my-window*]
       [paint-callback (lambda (canvas dc)
                         (send *my-rotating-image* render dc))]
       [keyboard-handler (lambda (key-event)
                           (let ((key-code (send key-event get-key-code)))
                             (if (not (equal? key-code 'release))
                                 (send *my-rotating-image* key-down key-code)
                                 (void))))]))
(define *my-timer* (new timer%
                        [notify-callback (lambda ()
                                           (send *my-game-canvas* refresh))]))
(create-test-image (send *my-rotating-image* get-image))
(send *my-window* show #t)
(send *my-timer* start 17)
(send *my-game-canvas* focus)