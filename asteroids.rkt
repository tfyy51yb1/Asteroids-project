#lang racket
(require racket/gui)
(provide asteroid%)

(define asteroid%
  (class object%
    (init-field [image (make-bitmap 100 100)]
                [xpos (random 1920)]
                [ypos (random 1080)]
                [dx (random 4)]
                [dy (random 4)]
                [angle (random 7)])
    
  (define/public (get-image)
    image)
    
  (define/private (create-asteroid-image bitmap-target)
    (let ([dc (new bitmap-dc% [bitmap bitmap-target])])
       (send dc draw-ellipse 0 0 100 100)))
    
    (create-asteroid-image image)
    (super-new)))
  