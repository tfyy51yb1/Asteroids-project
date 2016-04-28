#lang racket
(require racket/gui)
(provide asteroid%)
(provide asteroids-hash)

;; asteroid% contains the information for creating asteroids
;; at random locations, thus all the use of the random procedure.
;; TODO: Expand on asteroid%'s current capabilities, eg. providing support
;; for asteroids of different sizes and a method for detroying asteroids.

;; Create a hash table, asteroids-hash, in which all the asteroid-objects
;; will be stored. We use hash tables to store stuff throughout the project
;; because they're mutable, and it turns out to be rather convenient.
(define asteroids-hash (make-hash))

(define asteroid%
  (class object%
    (init-field [name ""]
                [health 1]
                [image (make-bitmap 200 200)] ;A bitmap to draw the asteroid in.
                [xpos (random 1920)] ;The asteroid's x-coordinate.
                [ypos (random 1080)] ;The asteroid's y-coordinate.
                [dx (random 4)] ;The asteroid's speed in the x-direction.
                [dy (random 4)] ;The asteroid's speed in the y-direction.
                [angle (random 7)]) ;The angle at which the asteroid is turned.
    ; Not in use as of now but useful if we
    ; want the asteroids to rotate.
    
    ;; Method which provides the bitmap.
    (define/public (get-image)
      image)
    
    (define/public (destroy-asteroid name)
      (hash-remove! asteroids-hash name))
    
    ;; Method for drawing the asteroid in the bitmap.  
    (define/private (create-asteroid-image bitmap-target)
      (let ([dc (new bitmap-dc% [bitmap bitmap-target])])
        (send dc draw-ellipse 0 0 200 200)))
    
    (create-asteroid-image image)
    (super-new)))
