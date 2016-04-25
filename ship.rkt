#lang racket
(require racket/gui)
(provide ship%)
(require "bullet.rkt")


(define ship%
  (class object%
    (init-field [xpos 200]
                [ypos 200]
                [dx 0]
                [dy 0]
                [speed 0]
                [angle 0]
                [image (make-bitmap 100 100)])
    

    
    (define/public (get-image)
      image)
    
    (define/private (create-ship-image bitmap-target)
      (let ((dc (new bitmap-dc% [bitmap bitmap-target])))
        (send dc draw-line
              50 0
              0 100)
        (send dc draw-line
              50 0
              100 100)
        (send dc draw-line
              0 100
              50 75)
        (send dc draw-line
              100 100
              50 75)))
    
    (define/private (fire)
      (hash-set! bullet-hash this (make-object bullet% xpos ypos)))
    
    (define/public (move key-code)
      (case key-code
        [(numpad8 #\w up) (unless (>= speed 2)
                            (set! speed (+ speed 0.35)))
                          (set! dy (- dy (* speed (cos angle))))
                          (set! dx (- dx (* speed (sin angle))))]
        [(numpad4 #\a left) (set! angle (+ angle 0.2))]
        [(numpad2 #\d right) (set! angle (- angle 0.2))]
        [(numpad2 #\m space) (fire)]))
    
    (create-ship-image image)
    
    (super-new)))