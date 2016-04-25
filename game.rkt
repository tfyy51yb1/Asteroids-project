#lang racket
(require racket/gui)
(require "bullet.rkt")
(provide game%)


(define (screen-wrap obj)
  (let* ([xpos (get-field xpos obj)]
         [ypos (get-field ypos obj)])
    (cond
      [(> xpos 1920) (set! xpos 0)]
      [(< xpos 0) (set! xpos 1920)]
      [(> ypos 1080) (set! ypos 0)]
      [(< ypos 0) (set! ypos 1080)])
    (set-field! xpos obj xpos)
    (set-field! ypos obj ypos)))

(define game%
  (class object%
    (define (update-ship ship dc)
      (let* ([image (send ship get-image)]
             [image-width (send image get-width)]
             [image-height (send image get-height)]
             [xpos (get-field xpos ship)]
             [ypos (get-field ypos ship)]
             [dx (get-field dx ship)]
             [dy (get-field dy ship)]
             [angle (get-field angle ship)]
             [speed (get-field speed ship)])
        (set! xpos (+ xpos dx))
        (set! ypos (+ ypos dy))
        (send dc translate (+ xpos (/ image-width 2))
              (+ ypos (/ image-height 2)))
        (send dc rotate angle)
        (send dc draw-bitmap image (- (/ image-width 2))
              (- (/ image-height 2)))
        (send dc rotate (- angle))
        (send dc translate (- (+ xpos (/ image-width 2)))
              (- (+ ypos (/ image-height 2))))
        
        (set! dx (* dx 0.99))
        (set! dy (* dy 0.99))
        (set-field! xpos ship xpos)
        (set-field! ypos ship ypos)
        (set-field! dx ship dx)
        (set-field! dy ship dy)
        (set-field! angle ship angle)
        (set-field! speed ship speed)
        (screen-wrap ship)))
    
    (define (update-bullets bullet-hash dc)
      (for-each (lambda (bullet)
                  (let*  ([image (send bullet get-image)]
                          [image-width (send image get-width)]
                          [image-height (send image get-height)]
                          [angle (get-field angle bullet)]
                          [xpos  (get-field xpos bullet)]
                          [ypos (get-field ypos bullet)]
                          [dx (get-field dx bullet)]
                          [dy (get-field dy bullet)])
                    (cond
                      [(> xpos 1920) (set! xpos 0) (set! ypos (- 1080 ypos))]
                      [(< xpos 0) (set! xpos 1920) (set! ypos (- 1080 ypos))]
                      [(> ypos 1080) (set! ypos 0) (set! xpos (- 1920 xpos))]
                      [(< ypos 0) (set! ypos 1080) (set! xpos (- 1920 xpos))])
                    (set! xpos (+ xpos dx))
                    (set! ypos (+ ypos dy))
                    (send dc draw-bitmap image xpos ypos)
                    (set-field! xpos bullet xpos)
                    (set-field! ypos bullet ypos)
                    (set-field! dy bullet dy)
                    (set-field! dx bullet dx)
                    (screen-wrap bullet)))
                (hash-values bullet-hash)))
    
    (define (update-asteroids asteroids-hash dc)
      (for-each (lambda (asteroid)
                  (let* ([image (send asteroid get-image)]
                         [image-width (send image get-width)]
                         [image-height (send image get-height)]
                         [angle (get-field angle asteroid)]
                         [xpos  (get-field xpos asteroid)]
                         [ypos (get-field ypos asteroid)]
                         [dx (get-field dx asteroid)]
                         [dy (get-field dy asteroid)])
                    
                    (cond
                      [(> xpos 1920) (set! xpos 0) (set! ypos (- 1080 ypos))]
                      [(< xpos 0) (set! xpos 1920) (set! ypos (- 1080 ypos))]
                      [(> ypos 1080) (set! ypos 0) (set! xpos (- 1920 xpos))]
                      [(< ypos 0) (set! ypos 1080) (set! xpos (- 1920 xpos))])
                    (set! xpos (+ xpos dx))
                    (set! ypos (+ ypos dy))
                    (send dc draw-bitmap image xpos ypos)
                    (set-field! xpos asteroid xpos)
                    (set-field! ypos asteroid ypos)
                    (set-field! dy asteroid dy)
                    (set-field! dx asteroid dx)
                    (screen-wrap asteroid)))
                (hash-values asteroids-hash)))
    
    (define/public (render ship bullet-hash asteroids-hash dc)
      (update-ship ship dc)
      (update-asteroids asteroids-hash dc)
      (update-bullets bullet-hash dc))
    
    (super-new)))