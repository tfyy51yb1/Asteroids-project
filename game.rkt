#lang racket
(require racket/gui)
(provide game%)

(define game%
  (class object%
    (init-field obj)

    (field [image (send obj get-image)])
    
    (define/public (render dc)
      (send dc set-canvas-background "black")
      (send obj draw dc))
    
    (super-new)))