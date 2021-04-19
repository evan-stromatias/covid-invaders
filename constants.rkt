;; Constants:
#lang racket
(require 2htdp/image)

(provide (all-defined-out))

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)
(define INVADER-Y-SPEED 1.5)
(define INVADER-DX 1)

(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)
(define INVADE-RATE-ADJUSTED (/ (sqr INVADE-RATE) 2)) ; From here: https://courses.edx.org/courses/course-v1:UBCx+HtC1x+2T2017/discussion/forum/v1-UBCx-SPD1x-2T2016-general/threads/597f195d3662b7096c000153

(define BACKGROUND (empty-scene WIDTH HEIGHT))

; spike
(define SPIKE1 (overlay/offset (ellipse 6 3 "solid" "darkred") 0 5
                               (rectangle 2 10 "solid" "darkred")))
(define BODY (circle 10 "solid" "darkred"))

(define INVADER
  (overlay/offset (rotate 50 (flip-vertical SPIKE1)) -8 -6
       (overlay/offset (rotate -55 (flip-vertical SPIKE1)) 8 -6
              (overlay/offset (rotate -3 (flip-vertical SPIKE1)) 1 -12
                     (overlay/offset (rotate 65 SPIKE1) 10 0
                            (overlay/offset (rotate -65 SPIKE1) -8 0
                                   (overlay/offset (rotate 35 SPIKE1) 7 5
                                          (overlay/offset (rotate -35 SPIKE1) -7 5
                                                 (overlay/offset (rotate -3 SPIKE1) -1 10 BODY)))))))))

(define TANK
  (above (rectangle 1 15 "solid" "black")
         (above (rectangle 5 5 "solid" "black")
                (above (rectangle 17 2 "outline" "black")
                       (above (rectangle 18 12 "solid" "black")
                              (above (overlay/xy (rectangle 9 2 "solid" "black") -9 -4
                                                 (overlay/xy (rectangle 8 2 "solid" "black") -10 -9
                                                             (overlay/xy (rectangle 5 2 "solid" "black") -12 -14 (rectangle 17 20 "outline" "black"))))
                                     (above (rectangle 32 4 "solid" "black")
                                            (above (rectangle 10 12 "solid" "black")
                                                   (rectangle 28 4 "solid" "black")))))))))

(define GAME-OVER-IMAGE (place-image 
                            (text "Game\n Over\nAmigo" 30 "Light Goldenrod") (/ WIDTH 2) (/ HEIGHT 2)
                            (place-image 
                                   (scale/xy 8 8 INVADER) (/ WIDTH 2) (/ HEIGHT 2) BACKGROUND)))

(define INVADER-WIDTH/2 (/ (image-width INVADER) 2))

(define INVADER-HEIGHT/2 (/ (image-height INVADER) 2))

(define INVADER-LANDING-HEIGHT (- HEIGHT INVADER-HEIGHT/2))

(define TANK-HEIGHT (image-height TANK))

(define TANK-HEIGHT/2 (/ TANK-HEIGHT 2))

(define TANK-WIDTH/2 (/ (image-width TANK) 2))

(define TANK-Y (- HEIGHT TANK-HEIGHT/2))

(define MISSILE (ellipse 5 15 "solid" "red"))

(define MISSILE-HEIGHT/2 (/ (image-height MISSILE) 2))

(define MISSILE-WIDTH/2 (/ (image-width MISSILE) 2))

(define MISSILE-OUTSIDE-OF-SCREEN-Y 0) ; missiles above this value will be removed from the Game

(define TANK-MISSILE-LAUNCHER-Y (- (- HEIGHT TANK-HEIGHT) MISSILE-HEIGHT/2))

