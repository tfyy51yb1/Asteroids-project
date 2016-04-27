#lang racket
(require racket/gui)
(provide bullet%)
(provide bullet-hash)

;; Very much work in progress. Nothing of worth here...

(define bullet-hash (make-hash))

(define bullet%
  (class object%
    (init-field [health 1]
                [xpos 0]
                [ypos 0]
                [dx 5]
                [dy 5]
                [name ""]
                [angle 0]
                [image (make-bitmap 10 10)])
    
    (define/public (get-image)
      image)
    (define/public (destroy-bullet name)
      (hash-remove! bullet-hash name))
    
    (define/private (create-bullet-image bitmap-target)
      (let ((dc (new bitmap-dc% [bitmap bitmap-target])))
        (send dc draw-ellipse 0 0 10 10)))
    
    (create-bullet-image image)
    
    (super-new)))