#lang racket
(require racket/gui)
(provide (all-defined-out))

(define bullet-list null)

(define game%
  (class object%
    (init-field [player null])
    (define/public (render dc)
      (send player update dc)
      (unless (null? bullet-list)
        (send (car bullet-list) update dc)))
    (super-new)))