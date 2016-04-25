#lang racket
(require racket/gui)
(require "game.rkt")
(provide bullet%)

(define bullet%
  (class object%
    (init-field [bullet-speed 4]
                [bullet-xpos 0]
                [bullet-ypos 0]
                [bullet-dx 0]
                [bullet-dy 0]
                [image (make-bitmap 10 10)])
    
    (define/private (create-body-image bitmap-target)
      (let ((dc (new bitmap-dc% [bitmap bitmap-target])))
        (send dc draw-ellipse bullet-xpos bullet-ypos 10 10)))
    
    (create-body-image image)
    
    (define/public (update dc)
      (set! bullet-xpos (+ bullet-xpos bullet-dx))
      (set! bullet-ypos (+ bullet-ypos bullet-dy))
      (send dc draw-bitmap image bullet-xpos bullet-ypos)
      (set! bullet-dx (* bullet-dx 0.8))
      (set! bullet-dy (* bullet-dy 0.8)))

    (append bullet-list this)
    (super-new)))