#lang racket
(require 2htdp/universe)
(require 2htdp/image)
 
(require rackunit
         "space-invaders.rkt")

;; Game -> Image
;; Render the game state to an image
;(define (render-game game) BACKGROUND) ; stub
(check-equal? (render-game G0) (place-image TANK (/ WIDTH 2) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))
(check-equal? (render-game G1) (place-image TANK 50 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))
(check-equal? (render-game G2)
              (place-image TANK 50 (- HEIGHT TANK-HEIGHT/2)
                           (place-image MISSILE 150 300
                                        (place-image INVADER 150 100 BACKGROUND))))
(check-equal? (render-game G3)
              (place-image INVADER 150 HEIGHT
                           (place-image INVADER 150 100
                                        (place-image MISSILE 150 110
                                                     (place-image MISSILE 150 300
                                                                  (place-image TANK 50 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))))))

;; Tank Image -> Image
;; Render the tank in an image
;(define (render-tank tank img) BACKGROUND) ;stub
(check-equal? (render-tank T1 BACKGROUND) (place-image TANK (tank-x T1) TANK-Y BACKGROUND))
(check-equal? (render-tank T1 (place-image INVADER 150 100 BACKGROUND)) (place-image TANK (tank-x T1) TANK-Y (place-image INVADER 150 100 BACKGROUND)))


;; (listof Invader) Image -> Image
;; Renders all the invaders on an input image
;(define (render-invaders invaders img) img) ; stub
(check-equal? (render-invaders empty BACKGROUND) BACKGROUND)
(check-equal? (render-invaders (list I1) BACKGROUND) (place-image INVADER 150 100 BACKGROUND))
(check-equal? (render-invaders (list I1 I2) BACKGROUND) (place-image INVADER 150 HEIGHT
                                                                     (place-image INVADER 150 100 BACKGROUND)))


;; Invader Image -> Image
;; Renders a single Invader on an image
;(define (render-invader invader img) img) ;stub
(check-equal? (render-invader I1 BACKGROUND) (place-image INVADER (invader-x I1) (invader-y I1) BACKGROUND))
(check-equal? (render-invader I1 (place-image INVADER 10 10 BACKGROUND)) (place-image INVADER (invader-x I1) (invader-y I1) (place-image INVADER 10 10 BACKGROUND)))


;; (listof Missile) Image -> Image
;; Renders all missiles on an image
;(define (render-missiles missiles img) img) ;stub
(check-equal? (render-missiles empty BACKGROUND) BACKGROUND)
(check-equal? (render-missiles (list M1) BACKGROUND) (place-image MISSILE 150 300 BACKGROUND))
(check-equal? (render-missiles (list M1 M2) BACKGROUND) (place-image MISSILE 150 300
                                                                     (place-image MISSILE 150 110 BACKGROUND)))


;; Missile Image -> Image
;; Renders a single Missile on an image
;(define (render-missile missile img) img) ; stub
(check-equal? (render-missile M1 BACKGROUND) (place-image MISSILE (missile-x M1) (missile-y M1) BACKGROUND))
(check-equal? (render-missile M1 (place-image MISSILE 10 10 BACKGROUND)) (place-image MISSILE (missile-x M1) (missile-y M1) (place-image MISSILE 10 10 BACKGROUND)))

