#lang racket
(require 2htdp/universe)
(require 2htdp/image)

(require rackunit "constants.rkt")
(require rackunit "covid-invaders.rkt")

;; https://stackoverflow.com/questions/43072656/typed-racket-error-with-check-random
(define (run-with-seed thunk seed)
  (parameterize ([current-pseudo-random-generator
                  (make-pseudo-random-generator)])
    (random-seed seed)
    (thunk)))

;; run a check-equal where both sides get the same PRNG seed
(define-syntax check-random-equal?
  (syntax-rules ()
    [(_ a b) (let ([seed (add1 (random (sub1 (expt 2 31))))])
               (check-equal? (run-with-seed (λ () a) seed)
                             (run-with-seed (λ () b) seed)))]))

;; Helper game constructor with default 0 score for the tests
(define (make-game-with-default-score inv mis tank)
  (make-game inv mis tank 0))



;; Game Game -> Game
;; Computes the new game score based on the previous game state and the new one 
;(define (advance-game-score new-game-state old-game-state) 0) ;stub
(check-equal? (advance-game-score (make-game-with-default-score empty empty T1) (make-game-with-default-score empty empty T1)) 0)
(check-equal? (advance-game-score (make-game-with-default-score (list I1) empty T1) (make-game-with-default-score empty empty T1)) 0)
(check-equal? (advance-game-score (make-game-with-default-score (list I1 I2) empty T1) (make-game-with-default-score empty empty T1)) 0)
(check-equal? (advance-game-score (make-game-with-default-score (list I1 I2) empty T1) (make-game-with-default-score (list I1 I2) empty T1)) 0)
(check-equal? (advance-game-score (make-game (list I1 I2) empty T1 10) (make-game (list I1 I2) empty T1 10)) 10)
(check-equal? (advance-game-score (make-game (list I1 I2) empty T1 10) (make-game (list I1) empty T1 10)) 10)
(check-equal? (advance-game-score (make-game (list I1 I2) empty T1 10) (make-game empty empty T1 10)) 10)
(check-equal? (advance-game-score (make-game (list I1) empty T1 10) (make-game (list I1 I2) empty T1 10)) 11)
(check-equal? (advance-game-score (make-game empty empty T1 10) (make-game (list I1 I2) empty T1 10)) 12)



;; Game Game -> Score
;; Returns the new game score based on the number of invaders in the new game state and the old game state
;(define (number-of-invaders-killed  new-game-state old-game-state) 0) ; stub
(check-equal? (number-of-invaders-killed (make-game-with-default-score empty empty T1) (make-game-with-default-score empty empty T1)) 0)
(check-equal? (number-of-invaders-killed (make-game-with-default-score (list I1) empty T1) (make-game-with-default-score empty empty T1)) 0)
(check-equal? (number-of-invaders-killed (make-game-with-default-score (list I1 I2) empty T1) (make-game-with-default-score empty empty T1)) 0)
(check-equal? (number-of-invaders-killed (make-game-with-default-score (list I1 I2) empty T1) (make-game-with-default-score (list I1 I2) empty T1)) 0)
(check-equal? (number-of-invaders-killed (make-game-with-default-score (list I1) empty T1) (make-game-with-default-score (list I1 I2) empty T1)) 1)
(check-equal? (number-of-invaders-killed (make-game-with-default-score empty empty T1) (make-game-with-default-score (list I1 I2) empty T1)) 2)
(check-equal? (number-of-invaders-killed (make-game-with-default-score empty empty T1) (make-game-with-default-score (list I1 I2 I3) empty T1)) 3)



;; Game Game -> Boolean
;; Returns True if invaders have been killed
;(define (have-invaders-been-killed? new-game-state old-game-state) false) ; stub
(check-equal? (have-invaders-been-killed? (make-game-with-default-score empty empty T1) (make-game-with-default-score empty empty T1)) false)
(check-equal? (have-invaders-been-killed? (make-game-with-default-score (list I1 I2) empty T1) (make-game-with-default-score empty empty T1)) false)
(check-equal? (have-invaders-been-killed? (make-game-with-default-score empty empty T1) (make-game-with-default-score (list I1 I2) empty T1)) true)
(check-equal? (have-invaders-been-killed? (make-game-with-default-score empty empty T1) (make-game-with-default-score (list I1) empty T1)) true)



;; Game -> Game
;; advances tank based on its direction
;(define (advance-game-tank game) game) ; stub
(check-equal? (advance-game-tank (make-game-with-default-score empty empty (make-tank (/ WIDTH 2) -1))) (make-game-with-default-score empty empty (make-tank (- (/ WIDTH 2) TANK-SPEED) -1)))
(check-equal? (advance-game-tank (make-game-with-default-score (list I1) (list M1) (make-tank (/ WIDTH 2) -1))) (make-game-with-default-score (list I1) (list M1) (make-tank (- (/ WIDTH 2) TANK-SPEED) -1)))
(check-equal? (advance-game-tank (make-game-with-default-score (list I1 I2) (list M1 M2) (make-tank (/ WIDTH 2) 1))) (make-game-with-default-score (list I1 I2) (list M1 M2) (make-tank (+ (/ WIDTH 2) TANK-SPEED) 1)))
(check-equal? (advance-game-tank (make-game-with-default-score (list I1 I2) (list M1 M2) (make-tank TANK-WIDTH/2 -1))) (make-game-with-default-score (list I1 I2) (list M1 M2) (make-tank TANK-WIDTH/2 -1)))
(check-equal? (advance-game-tank (make-game-with-default-score (list I1 I2) (list M1 M2) (make-tank TANK-WIDTH/2 1))) (make-game-with-default-score (list I1 I2) (list M1 M2) (make-tank (+ TANK-WIDTH/2 TANK-SPEED) 1)))
(check-equal? (advance-game-tank (make-game-with-default-score (list I1 I2) (list M1 M2) (make-tank (- WIDTH TANK-WIDTH/2) 1))) (make-game-with-default-score (list I1 I2) (list M1 M2) (make-tank (- WIDTH TANK-WIDTH/2) 1)))
(check-equal? (advance-game-tank (make-game-with-default-score (list I1 I2) (list M1 M2) (make-tank (- WIDTH TANK-WIDTH/2) -1))) (make-game-with-default-score (list I1 I2) (list M1 M2) (make-tank (- (- WIDTH TANK-WIDTH/2) TANK-SPEED) -1)))



;; Tank -> Tank
;; Updates the x location of the tank based on its direction
;(define (advance-tank tank) tank) ; stub
(check-equal? (advance-tank (make-tank (/ WIDTH 2) -1)) (make-tank (- (/ WIDTH 2) TANK-SPEED) -1))
(check-equal? (advance-tank (make-tank (/ WIDTH 2) 1)) (make-tank (+ (/ WIDTH 2) TANK-SPEED) 1))
(check-equal? (advance-tank (make-tank TANK-WIDTH/2 -1)) (make-tank TANK-WIDTH/2 -1))
(check-equal? (advance-tank (make-tank TANK-WIDTH/2 1)) (make-tank (+ TANK-WIDTH/2 TANK-SPEED) 1))
(check-equal? (advance-tank (make-tank (- WIDTH TANK-WIDTH/2) 1)) (make-tank (- WIDTH TANK-WIDTH/2) 1))
(check-equal? (advance-tank (make-tank (- WIDTH TANK-WIDTH/2) -1)) (make-tank (- (- WIDTH TANK-WIDTH/2) TANK-SPEED) -1))


;; Tank -> Boolean
;; Returns true if the next tank movement is within the screen borders and thus valid
;(define (is-next-tank-position-valid? tank) true) ; stub
(check-equal? (is-next-tank-position-valid? (make-tank (/ WIDTH 2) -1)) true)
(check-equal? (is-next-tank-position-valid? (make-tank (/ WIDTH 2) 1)) true)
(check-equal? (is-next-tank-position-valid? (make-tank (- (- WIDTH TANK-WIDTH/2) TANK-SPEED 1) 1)) true)
(check-equal? (is-next-tank-position-valid? (make-tank (- (- WIDTH TANK-WIDTH/2) TANK-SPEED) 1)) false)
(check-equal? (is-next-tank-position-valid? (make-tank (- (- WIDTH TANK-WIDTH/2) TANK-SPEED) -1)) true)
(check-equal? (is-next-tank-position-valid? (make-tank TANK-WIDTH/2 1)) true)
(check-equal? (is-next-tank-position-valid? (make-tank TANK-WIDTH/2 -1)) false)


;; Tank -> Natural
;; Helper function that returns a tank's next x coordinate based on its current direction and TANK-SPEED
;(define (move-tank-based-on-direction tank) tank) ; stub
(check-equal? (next-tank-x-location (make-tank 0 -1)) (- TANK-SPEED))
(check-equal? (next-tank-x-location (make-tank 0 1)) TANK-SPEED)


;; Game -> Game
;; advances missiles upwards by MISSILE-SPEED
;(define (advance-game-missiles game) game) ; stub
(check-equal? (advance-game-missiles (make-game-with-default-score empty empty T0)) (make-game-with-default-score empty empty T0))
(check-equal? (advance-game-missiles (make-game-with-default-score (list I1 I2) empty T0)) (make-game-with-default-score (list I1 I2) empty T0))
(check-equal? (advance-game-missiles (make-game-with-default-score (list I1 I2)
                                                (list (make-missile 0 HEIGHT) (make-missile 0 0))
                                                T0))
              (make-game-with-default-score (list I1 I2) (list (make-missile 0 (- HEIGHT MISSILE-SPEED)) (make-missile 0 (- 0 MISSILE-SPEED))) T0))


;; (listOf Missile) -> (listOf Missile)
;; moves all missiles in a list of missiles upwards by MISSILE-SPEED
;(define (advance-missiles lom) lom) ; stub
(check-equal? (advance-missiles empty) empty)
(check-equal? (advance-missiles (list (make-missile 0 0))) (list (make-missile 0 (- MISSILE-SPEED))))
(check-equal? (advance-missiles (list (make-missile 0 HEIGHT))) (list (make-missile 0 (- HEIGHT MISSILE-SPEED))))
(check-equal? (advance-missiles (list (make-missile 0 HEIGHT)
                                      (make-missile 10 (- HEIGHT 10))
                                      (make-missile 20 (- HEIGHT 20))
                                      (make-missile 30 (- HEIGHT 30))))
              (list (make-missile 0 (- HEIGHT MISSILE-SPEED))
                    (make-missile 10 (- (- HEIGHT 10) MISSILE-SPEED))
                    (make-missile 20 (- (- HEIGHT 20) MISSILE-SPEED))
                    (make-missile 30 (- (- HEIGHT 30) MISSILE-SPEED))))


;; Missile -> Missile
;; Advances a single missile by MISSILE-SPEED upwards
; (define (advance-missile m) m) ; stub
(check-equal? (advance-missile (make-missile 0 0)) (make-missile 0 (- MISSILE-SPEED)))
(check-equal? (advance-missile (make-missile 100 HEIGHT)) (make-missile 100 (- HEIGHT MISSILE-SPEED)))


;; Game -> Game
;; Removes that missiles from the Game state that are outside of the screen
;(define (filter-missiles game) game) ; stub
(check-equal? (filter-game-missiles (make-game-with-default-score empty empty T0)) (make-game-with-default-score empty empty T0))
(check-equal? (filter-game-missiles (make-game-with-default-score (list I1 I2) empty T0)) (make-game-with-default-score (list I1 I2) empty T0))
(check-equal? (filter-game-missiles (make-game-with-default-score (list I1 I2) (list (make-missile 0 HEIGHT)) T0)) (make-game-with-default-score (list I1 I2) (list (make-missile 0 HEIGHT)) T0))
(check-equal? (filter-game-missiles (make-game-with-default-score (list I1 I2) (list (make-missile 0 MISSILE-OUTSIDE-OF-SCREEN-Y)) T0))
              (make-game-with-default-score (list I1 I2) (list (make-missile 0 MISSILE-OUTSIDE-OF-SCREEN-Y)) T0))
(check-equal? (filter-game-missiles (make-game-with-default-score (list I1 I2) (list (make-missile 0 MISSILE-OUTSIDE-OF-SCREEN-Y) (make-missile 0 (- MISSILE-OUTSIDE-OF-SCREEN-Y 1))) T0))
              (make-game-with-default-score (list I1 I2) (list (make-missile 0 MISSILE-OUTSIDE-OF-SCREEN-Y)) T0))
(check-equal? (filter-game-missiles (make-game-with-default-score (list I1 I2) (list (make-missile 0 HEIGHT) (make-missile 0 MISSILE-OUTSIDE-OF-SCREEN-Y) (make-missile 0 (- MISSILE-OUTSIDE-OF-SCREEN-Y 1))) T0))
              (make-game-with-default-score (list I1 I2) (list (make-missile 0 HEIGHT) (make-missile 0 MISSILE-OUTSIDE-OF-SCREEN-Y)) T0))


;; (listOf Missile) -> (listOf Missile)
;; Removes the missiles from a list of missiles that are outside of the screen (less than MISSILE-OUTSIDE-OF-SCREEN-Y)
;(define (filter-missiles lom) lom) ; stub
(check-equal? (filter-missiles empty) empty)
(check-equal? (filter-missiles (list (make-missile 0 HEIGHT))) (list (make-missile 0 HEIGHT)))
(check-equal? (filter-missiles (list (make-missile 0 MISSILE-OUTSIDE-OF-SCREEN-Y))) (list (make-missile 0 MISSILE-OUTSIDE-OF-SCREEN-Y)))
(check-equal? (filter-missiles (list (make-missile 0 MISSILE-OUTSIDE-OF-SCREEN-Y) (make-missile 0 (- MISSILE-OUTSIDE-OF-SCREEN-Y 1)))) (list (make-missile 0 MISSILE-OUTSIDE-OF-SCREEN-Y)))
(check-equal? (filter-missiles (list (make-missile 0 HEIGHT) (make-missile 0 MISSILE-OUTSIDE-OF-SCREEN-Y) (make-missile 0 (- MISSILE-OUTSIDE-OF-SCREEN-Y 1))))
              (list (make-missile 0 HEIGHT) (make-missile 0 MISSILE-OUTSIDE-OF-SCREEN-Y)))
(check-equal? (filter-missiles (list (make-missile 0 HEIGHT) (make-missile 0 (- MISSILE-OUTSIDE-OF-SCREEN-Y 1)) (make-missile 0 MISSILE-OUTSIDE-OF-SCREEN-Y)))
              (list (make-missile 0 HEIGHT) (make-missile 0 MISSILE-OUTSIDE-OF-SCREEN-Y)))
(check-equal? (filter-missiles (list (make-missile 0 (- MISSILE-OUTSIDE-OF-SCREEN-Y 1)) (make-missile 0 HEIGHT) (make-missile 0 MISSILE-OUTSIDE-OF-SCREEN-Y)))
              (list (make-missile 0 HEIGHT) (make-missile 0 MISSILE-OUTSIDE-OF-SCREEN-Y)))


;; Missile -> Boolean
;; Returns true is a missile's y location is outside of the screen (less than MISSILE-OUTSIDE-OF-SCREEN-Y)
;(define (missile-out-of-screen? m) false) ; stub
(check-equal? (missile-out-of-screen? (make-missile 100 MISSILE-OUTSIDE-OF-SCREEN-Y)) false)
(check-equal? (missile-out-of-screen? (make-missile 50 HEIGHT)) false)
(check-equal? (missile-out-of-screen? (make-missile 20 (- HEIGHT MISSILE-OUTSIDE-OF-SCREEN-Y))) false)
(check-equal? (missile-out-of-screen? (make-missile 10 (- MISSILE-OUTSIDE-OF-SCREEN-Y 1))) true)
(check-equal? (missile-out-of-screen? (make-missile 5 (- MISSILE-OUTSIDE-OF-SCREEN-Y 2))) true)


;; Game -> Game
;; Remove missiles and invaders that collided with each other
;(define (explode-game-objects game) game) ; stub
(check-equal? (explode-game-objects (make-game-with-default-score empty empty T0)) (make-game-with-default-score empty empty T0))
(check-equal? (explode-game-objects (make-game-with-default-score (list (make-invader 150 300 0)) empty T0)) (make-game-with-default-score (list (make-invader 150 300 0)) empty T0))
(check-equal? (explode-game-objects (make-game-with-default-score empty (list (make-missile 150 300)) T0)) (make-game-with-default-score empty (list (make-missile 150 300)) T0))
(check-equal? (explode-game-objects (make-game-with-default-score (list (make-invader 0 0 0) (make-invader 10 10 0) (make-invader 100 100 100))
                                               (list (make-missile 150 300) (make-missile 250 350) (make-missile 109 105)) T0))
              (make-game-with-default-score (list (make-invader 0 0 0) (make-invader 10 10 0))
                         (list (make-missile 150 300) (make-missile 250 350)) T0))


;; Game -> (listOf Invader)
;; Removes invaders that collided with missiles
;(define (explode-game-invaders game) (game-missiles game)) ; stub
(check-equal? (explode-game-invaders (make-game-with-default-score empty empty T0)) empty)
(check-equal? (explode-game-invaders (make-game-with-default-score (list (make-invader 150 300 0)) empty T0)) (list (make-invader 150 300 0)))
(check-equal? (explode-game-invaders (make-game-with-default-score empty (list (make-missile 150 300)) T0)) empty)
(check-equal? (explode-game-invaders (make-game-with-default-score (list (make-invader 0 0 0) (make-invader 10 10 0) (make-invader 100 100 100))
                                                (list (make-missile 150 300) (make-missile 250 350) (make-missile 109 105)) T0))
              (list (make-invader 0 0 0) (make-invader 10 10 0)))


;; (listOf Invader) (listOf Missile) -> (listOf Invader)
;; Returns the invaders that did not collide with a missile
;(define (game-invaders-that-didnt-collide-with-missiles loi lom) loi) ; stub
(check-equal? (game-invaders-that-didnt-collide-with-missiles empty empty) empty)
(check-equal? (game-invaders-that-didnt-collide-with-missiles empty (list (make-missile 150 300))) empty)
(check-equal? (game-invaders-that-didnt-collide-with-missiles (list (make-invader 150 300 0)) (list (make-missile 150 300))) empty)
(check-equal? (game-invaders-that-didnt-collide-with-missiles (list (make-invader 250 350 0)) (list (make-missile 150 300))) (list (make-invader 250 350 0)))
(check-equal? (game-invaders-that-didnt-collide-with-missiles
               (list (make-invader 150 300 0) (make-invader 250 350 0))
               (list (make-missile 150 300) (make-missile 250 350) (make-missile 350 450)))
              empty)


;; Invader (listOf Missile) -> Boolean
;; Returns true if an invader collided with a missile
;(define (invader-hit-missiles? i lom) false ) ; stub
(check-equal? (invader-hit-missiles? I1 empty) false)
(check-equal? (invader-hit-missiles? (make-invader 150 300 0) (list (make-missile 150 300))) true)
(check-equal? (invader-hit-missiles? (make-invader 150 300 0) (list (make-missile 141 300))) true)
(check-equal? (invader-hit-missiles? (make-invader 150 300 0) (list (make-missile 140 300))) false)
(check-equal? (invader-hit-missiles? (make-invader 150 300 0) (list (make-missile 150 309))) true)
(check-equal? (invader-hit-missiles? (make-invader 150 300 0) (list (make-missile 150 310))) false)
(check-equal? (invader-hit-missiles? (make-invader 150 300 0) (list (make-missile 160 300))) false)
(check-equal? (invader-hit-missiles? (make-invader 150 300 0) (list
                                                               (make-missile 0 0) 
                                                               (make-missile 150 309))) true)
(check-equal? (invader-hit-missiles? (make-invader 150 300 0) (list
                                                               (make-missile 10 10) 
                                                               (make-missile 150 309))) true)
(check-equal? (invader-hit-missiles? (make-invader 150 300 0) (list
                                                               (make-missile 10 10) 
                                                               (make-missile 0 0)
                                                               (make-missile 150 309)
                                                               )) true)
(check-equal? (invader-hit-missiles? (make-invader 150 300 0) (list
                                                               (make-missile 10 10) 
                                                               (make-missile 0 0)
                                                               (make-missile 150 310)
                                                               )) false)



;; Game -> (listOf Missile)
;; Remove missiles that collided with invaders
;(define (invader-hit-missiles? i lom) false ) ; stub
(check-equal? (explode-game-missiles (make-game-with-default-score empty empty T0)) empty)
(check-equal? (explode-game-missiles (make-game-with-default-score (list (make-invader 150 300 0)) empty T0)) empty)
(check-equal? (explode-game-missiles (make-game-with-default-score empty (list (make-missile 150 300)) T0)) (list (make-missile 150 300)))
(check-equal? (explode-game-missiles (make-game-with-default-score (list (make-invader 0 0 0) (make-invader 10 10 0) (make-invader 100 100 100))
                                                (list (make-missile 150 300) (make-missile 250 350) (make-missile 109 105)) T0))
              (list (make-missile 150 300) (make-missile 250 350)))


;; (listOf Missile) (listOf Invader) -> (listOf Missile)
;; Returns the missiles that did not collide with an invader
;(define (game-missiles-that-didnt-collide-with-invaders lom loi) lom) ; stub
(check-equal? (game-missiles-that-didnt-collide-with-invaders empty empty) empty)
(check-equal? (game-missiles-that-didnt-collide-with-invaders empty (list (make-invader 150 300 0))) empty)
(check-equal? (game-missiles-that-didnt-collide-with-invaders (list (make-missile 150 300))
                                                              (list (make-invader 150 300 0))) empty)
(check-equal? (game-missiles-that-didnt-collide-with-invaders (list (make-missile 150 300))
                                                              (list (make-invader 250 350 0))) (list (make-missile 150 300)))
(check-equal? (game-missiles-that-didnt-collide-with-invaders
               (list (make-missile 150 300) (make-missile 250 350) (make-missile 350 450))
               (list (make-invader 150 300 0) (make-invader 250 350 0)))
              (list (make-missile 350 450)))


;; Missile (listOf Invader) -> Boolean
;; Returns true if a missile hits an invader
;(define (missile-hit-invaders? m loi) false ) ; stub
(check-equal? (missile-hit-invaders? M1 empty) false)
(check-equal? (missile-hit-invaders? (make-missile 150 300) (cons (make-invader 150 300 0) empty)) true)
(check-equal? (missile-hit-invaders? (make-missile 150 300) (cons (make-invader 141 300 0) empty)) true)
(check-equal? (missile-hit-invaders? (make-missile 150 300) (cons (make-invader 140 300 0) empty)) false)
(check-equal? (missile-hit-invaders? (make-missile 150 300) (cons (make-invader 150 309 0) empty)) true)
(check-equal? (missile-hit-invaders? (make-missile 150 300) (cons (make-invader 150 310 0) empty)) false)
(check-equal? (missile-hit-invaders? (make-missile 150 300) (cons (make-invader 160 300 0) empty)) false)
(check-equal? (missile-hit-invaders? (make-missile 150 300) (list
                                                             (make-invader 0 0 0) 
                                                             (make-invader 150 309 0))) true)
(check-equal? (missile-hit-invaders? (make-missile 150 300) (list
                                                             (make-invader 10 10 0) 
                                                             (make-invader 150 309 0))) true)
(check-equal? (missile-hit-invaders? (make-missile 150 300) (list
                                                             (make-invader 10 10 0) 
                                                             (make-invader 0 0 0)
                                                             (make-invader 150 309 0)
                                                             )) true)
(check-equal? (missile-hit-invaders? (make-missile 150 300) (list
                                                             (make-invader 10 10 0) 
                                                             (make-invader 0 0 0)
                                                             (make-invader 150 310 0)
                                                             )) false)


;; Missile Invader -> Boolean
;; Returns true if the center point (x,y) of a missile are within a HIT-RANGE distance from the center point (x,y) of an invader
;(define (missile-hit-invader? m i) false) ; stub
(check-equal? (missile-hit-invader? (make-missile 100 100) (make-invader 100 100 0)) true)
(check-equal? (missile-hit-invader? (make-missile 100 100) (make-invader 100 (+ 100 HIT-RANGE) 0)) false)
(check-equal? (missile-hit-invader? (make-missile 100 100) (make-invader (+ 100 HIT-RANGE) 100 0)) false)
(check-equal? (missile-hit-invader? (make-missile 100 (+ 100 HIT-RANGE)) (make-invader 100 100 0)) false)
(check-equal? (missile-hit-invader? (make-missile (+ 100 HIT-RANGE) 100) (make-invader 100 100 0)) false)
(check-equal? (missile-hit-invader? (make-missile 100 100) (make-invader 100 (+ 100 (sub1 HIT-RANGE)) 0)) true)
(check-equal? (missile-hit-invader? (make-missile 100 100) (make-invader (+ 100 (sub1 HIT-RANGE)) 100 0)) true)
(check-equal? (missile-hit-invader? (make-missile 100 (+ 100 (sub1 HIT-RANGE))) (make-invader 100 100 0)) true)
(check-equal? (missile-hit-invader? (make-missile (+ 100 (sub1 HIT-RANGE)) 100) (make-invader 100 100 0)) true)


;; Number Number Number Number Number-> Boolean
;; Given some (xp,yp) coordinates and some (xc, yc) it will return true if (xp,yp) fall inside the rectangle defined by xc, yc and some hit-range distance
;(define (is-point-within-hit-range? xp yp xc yc hit-range) false) ; stub
(check-equal? (is-point-within-hit-range? 0 0 100 100 10) false)
(check-equal? (is-point-within-hit-range? 100 100 100 100 10) true)
(check-equal? (is-point-within-hit-range? 90 90 100 100 10) false)
(check-equal? (is-point-within-hit-range? 91 91 100 100 10) true)
(check-equal? (is-point-within-hit-range? 109 109 100 100 10) true)
(check-equal? (is-point-within-hit-range? 100 100 109 91 10) true)
(check-equal? (is-point-within-hit-range? 100 100 120 120 10) false)
(check-equal? (is-point-within-hit-range? 100 100 80 80 10) false)


;; Game -> Game
;; creates a new invader randomly and returns a new game state
;(define (spawn-new-game-invaders game) game) ; stub
(check-random-equal? (spawn-new-game-invaders (make-game-with-default-score empty empty T1)) (make-game-with-default-score (spawn-new-invader-randomly empty) empty T1))
(check-random-equal? (spawn-new-game-invaders (make-game-with-default-score empty (list M1 M2) T1)) (make-game-with-default-score (spawn-new-invader-randomly empty) (list M1 M2) T1))
(check-random-equal? (spawn-new-game-invaders (make-game-with-default-score (list I1 I2) (list M1 M2) T1)) (make-game-with-default-score (spawn-new-invader-randomly (list I1 I2)) (list M1 M2) T1))


;; (listOf Invader) -> (listOf Invader)
;; Spawns a new Invader at a random x position from the top of the screen (y=0)
;(define (spawn-new-invader-randomly loi) loi) ; stub
(check-random-equal? (spawn-new-invader-randomly empty) (spawn-new-invader-randomly empty))
(check-random-equal? (spawn-new-invader-randomly (list I1)) (spawn-new-invader-randomly (list I1)))
(check-random-equal? (spawn-new-invader-randomly (list I1 I2)) (spawn-new-invader-randomly (list I1 I2)))
(check-random-equal? (spawn-new-invader-randomly (list I1 I2 I1 I2)) (spawn-new-invader-randomly (list I1 I2 I1 I2)))


;; Game -> Game
;; Given a Game state, advance all game invaders by one step and return a new game state
;(define (advance-game-invaders game) game) ; stub
(check-equal? (advance-game-invaders (make-game-with-default-score empty empty T1)) (make-game-with-default-score empty empty T1))
(check-equal? (advance-game-invaders (make-game-with-default-score empty (list M1 M2) T1)) (make-game-with-default-score empty (list M1 M2) T1))
(check-equal? (advance-game-invaders (make-game-with-default-score
                                      (list (make-invader INVADER-WIDTH/2 0 1) (make-invader 0 0 0))
                                      (list M1 M2)
                                      T1))
              (make-game-with-default-score (list (make-invader (+ INVADER-WIDTH/2 (* 1 INVADER-X-SPEED)) (+ 0 (* (abs 1) INVADER-Y-SPEED)) 1) (make-invader 0 0 0))
                         (list M1 M2)
                         T1))
(check-equal? (advance-game-invaders (make-game-with-default-score
                                      (list (make-invader INVADER-WIDTH/2 0 1) (make-invader (- WIDTH INVADER-WIDTH/2) 0 1))
                                      (list M1 M2)
                                      T1))
              (make-game-with-default-score (list (make-invader (+ INVADER-WIDTH/2 (* 1 INVADER-X-SPEED)) (+ 0 (* (abs 1) INVADER-Y-SPEED)) 1) (make-invader (+ (- WIDTH INVADER-WIDTH/2) (* -1 INVADER-X-SPEED)) (+ 0 (* (abs 1) INVADER-Y-SPEED)) -1))
                         (list M1 M2)
                         T1))


;; (listOf Invader) -> (listOf Invader)
;; Advances all invaders by one iteration
;(define (advance-invaders loi) loi) ; stub
(check-equal? (advance-invaders empty) empty)
(check-equal? (advance-invaders (list (make-invader 0 0 0))) (list (make-invader 0 0 0)))
(check-equal? (advance-invaders (list (make-invader 0 0 0) (make-invader 0 0 0))) (list (make-invader 0 0 0) (make-invader 0 0 0)))
(check-equal? (advance-invaders (list (make-invader INVADER-WIDTH/2 0 1) (make-invader 0 0 0)))
              (list (make-invader (+ INVADER-WIDTH/2 (* 1 INVADER-X-SPEED)) (+ 0 (* (abs 1) INVADER-Y-SPEED)) 1) (make-invader 0 0 0)))
(check-equal? (advance-invaders (list (make-invader INVADER-WIDTH/2 0 1) (make-invader (- WIDTH INVADER-WIDTH/2) 0 1)))
              (list (make-invader (+ INVADER-WIDTH/2 (* 1 INVADER-X-SPEED)) (+ 0 (* (abs 1) INVADER-Y-SPEED)) 1) (make-invader (+ (- WIDTH INVADER-WIDTH/2) (* -1 INVADER-X-SPEED)) (+ 0 (* (abs 1) INVADER-Y-SPEED)) -1)))


;; Invader -> Invader
;; Advances a single Invader based on its current and future position
;(define (advance-invader inv) inv) ; stub
(check-equal? (advance-invader (make-invader 0 0 0)) (make-invader 0 0 0))
(check-equal? (advance-invader (make-invader INVADER-WIDTH/2 0 1)) (make-invader (+ INVADER-WIDTH/2 (* 1 INVADER-X-SPEED)) (+ 0 (* (abs 1) INVADER-Y-SPEED)) 1))
(check-equal? (advance-invader (make-invader INVADER-WIDTH/2 0 -1)) (make-invader (+ INVADER-WIDTH/2 (* 1 INVADER-X-SPEED)) (+ 0 (* (abs 1) INVADER-Y-SPEED)) 1))
(check-equal? (advance-invader (make-invader (- WIDTH INVADER-WIDTH/2) 0 1)) (make-invader (+ (- WIDTH INVADER-WIDTH/2) (* -1 INVADER-X-SPEED)) (+ 0 (* (abs 1) INVADER-Y-SPEED)) -1))


;; Invader Boolean -> Invader
;; Updates the position of an Invader using the current dx if use-same-dx? is true, otherwise the negated dx
;(define (update-invader (make-invader 0 0 -1) true)) ; stub
(check-equal? (update-invader (make-invader 0 0 0) true) (make-invader 0 0 0))
(check-equal? (update-invader (make-invader 0 0 0) false) (make-invader 0 0 0))
(check-equal? (update-invader (make-invader 0 0 -1) true) (make-invader (+ 0 (* -1 INVADER-X-SPEED)) (+ 0 (* (abs -1) INVADER-Y-SPEED)) -1))
(check-equal? (update-invader (make-invader 0 0 -1) false) (make-invader (+ 0 (* 1 INVADER-X-SPEED)) (+ 0 (* (abs 1) INVADER-Y-SPEED)) 1))
(check-equal? (update-invader (make-invader 0 0 1) true) (make-invader (+ 0 (* 1 INVADER-X-SPEED)) (+ 0 (* (abs 1) INVADER-Y-SPEED)) 1))
(check-equal? (update-invader (make-invader 0 0 1) false) (make-invader (+ 0 (* -1 INVADER-X-SPEED)) (+ 0 (* (abs -1) INVADER-Y-SPEED)) -1))


;; Invader Number -> Invader
;; Makes a new Invader using a new dx and ignoring the one defined in the invader
;(define (make-invader-with-custom-dx inv new-dx) inv) ; stub
(check-equal? (make-invader-with-custom-dx (make-invader 0 0 -1) 0) (make-invader 0 0 0))
(check-equal? (make-invader-with-custom-dx (make-invader 0 0 -1) 1) (make-invader (+ 0 (* 1 INVADER-X-SPEED)) (+ 0 (* (abs 1) INVADER-Y-SPEED)) 1))
(check-equal? (make-invader-with-custom-dx (make-invader 0 0 10) -10) (make-invader (+ 0 (*  -10 INVADER-X-SPEED)) (+ 0 (* (abs -10) INVADER-Y-SPEED)) -10))


;; Invader Number -> Number
;; Returns Invader's next x position given its current x, a custom dx value and INVADER-X-SPEED
;(define (next-x-for-invader-with-custom-dx inv new-dx) 0) ; stub
(check-equal? (next-x-for-invader-with-custom-dx (make-invader 0 0 -1) 1) (+ 0 (* 1 INVADER-X-SPEED)))
(check-equal? (next-x-for-invader-with-custom-dx (make-invader 0 0 -10) 10) (+ 0 (* 10 INVADER-X-SPEED)))
(check-equal? (next-x-for-invader-with-custom-dx (make-invader 0 0 10) -10) (+ 0 (*  -10 INVADER-X-SPEED)))


;; Invader Number -> Number
;; Returns Invader's next y position given its current y, the absolute value of a custom dx and INVADER-Y-SPEED
;(define (next-y-for-invader-with-custom-dx inv new-dx) 0) ; stub
(check-equal? (next-y-for-invader-with-custom-dx (make-invader 0 0 -1) 1) (+ 0 (* (abs 1) INVADER-Y-SPEED)))
(check-equal? (next-y-for-invader-with-custom-dx (make-invader 0 0 -10) 10) (+ 0 (* (abs 10) INVADER-Y-SPEED)))
(check-equal? (next-y-for-invader-with-custom-dx (make-invader 0 0 10) -10) (+ 0 (* (abs -10) INVADER-Y-SPEED)))


;; Invader -> Number
;; Returns the negated dx given an Invader eg (invert-invader-dx (make-invader 0 0 -10)) -> 10)
;(define (invert-invader-dx inv) -1) ; stub
(check-equal? (invert-invader-dx (make-invader 0 0 -1)) 1)
(check-equal? (invert-invader-dx (make-invader 0 0 1)) -1)
(check-equal? (invert-invader-dx (make-invader 0 0 -10.5)) 10.5)
(check-equal? (invert-invader-dx (make-invader 0 0 10.5)) -10.5)


;; Invader -> Boolean
;; Returns true if Invader's next position is within the screen WIDTH coordinates
;(define (is-next-invader-position-valid? (make-invader 0 0 0) true) ; stub
(check-equal? (is-next-invader-position-valid? (make-invader (/ WIDTH 2) (/ HEIGHT 2) -1)) true)
(check-equal? (is-next-invader-position-valid? (make-invader (/ WIDTH 2) (/ HEIGHT 2) 1)) true)
(check-equal? (is-next-invader-position-valid? (make-invader (add1 (+ INVADER-X-SPEED INVADER-WIDTH/2)) INVADER-WIDTH/2 -1)) true)
(check-equal? (is-next-invader-position-valid? (make-invader 0 (/ HEIGHT 2) -1)) false)
(check-equal? (is-next-invader-position-valid? (make-invader (- WIDTH INVADER-WIDTH/2) (/ HEIGHT 2) -1)) true)
(check-equal? (is-next-invader-position-valid? (make-invader WIDTH (/ HEIGHT 2) 1)) false)
(check-equal? (is-next-invader-position-valid? (make-invader (- (- WIDTH INVADER-WIDTH/2) (add1 INVADER-X-SPEED)) (/ HEIGHT 2) 1)) true)
(check-equal? (is-next-invader-position-valid? (make-invader (+ 2 (- WIDTH INVADER-WIDTH/2)) (/ HEIGHT 2) -1)) true)
(check-equal? (is-next-invader-position-valid? (make-invader 0 (/ HEIGHT 2) 1)) true)


;; Invader -> Boolean
;; Helpers used by is-next-invader-position-valid? to make the code more readable: Returns true if invader is within (but not equal to) the screen borders
;(define (is-invader-inside-the-screen? inv) false) ; stub
(check-equal? (is-invader-inside-the-screen? I1) true)
(check-equal? (is-invader-inside-the-screen? (make-invader (add1 INVADER-WIDTH/2) 0 0)) true)
(check-equal? (is-invader-inside-the-screen? (make-invader INVADER-WIDTH/2 0 0)) false)
(check-equal? (is-invader-inside-the-screen? (make-invader (sub1 (- WIDTH INVADER-WIDTH/2)) 0 0)) true)
(check-equal? (is-invader-inside-the-screen? (make-invader (- WIDTH INVADER-WIDTH/2) 0 0)) false)
(check-equal? (is-invader-inside-the-screen? (make-invader -10 0 0)) false)
(check-equal? (is-invader-inside-the-screen? (make-invader (add1 WIDTH) 0 0)) false)


;; Invader -> Boolean
;; Helpers used by is-next-invader-position-valid? to make the code more readable: Returns true if invader is outside the right border but it's moving left
;(define (is-invader-outside-the-right-border-but-moving-left? inv) false) ; stub
(check-equal? (is-invader-outside-the-right-border-but-moving-left? (make-invader -10 0 1)) true)
(check-equal? (is-invader-outside-the-right-border-but-moving-left? (make-invader -10 0 -1)) false)
(check-equal? (is-invader-outside-the-right-border-but-moving-left? I1) false)
              

;; Invader -> Boolean
;; Helpers used by is-next-invader-position-valid? to make the code more readable: Returns true if invader is outside the left border but dx is positive
;(define (is-invader-outside-the-left-border-but-moving-right? inv) false) ; stub
(check-equal? (is-invader-outside-the-left-border-but-moving-right? (make-invader WIDTH 0 1)) false)
(check-equal? (is-invader-outside-the-left-border-but-moving-right? (make-invader WIDTH 0 -1)) true)
(check-equal? (is-invader-outside-the-left-border-but-moving-right? I1) false)


;; Invader -> Natural
;; Helper function that returns the next x position of an Invader using the Invader's current dx
;(define (next-x-for-invader inv) 0) ; stub
(check-equal? (next-x-for-invader (make-invader 0 0 0)) 0)
(check-equal? (next-x-for-invader (make-invader 0 0 1)) INVADER-X-SPEED)
(check-equal? (next-x-for-invader (make-invader 0 0 -1)) (- 0 INVADER-X-SPEED))


;; Game -> Game
;; Produce the next game state by updating the tank, missiles (removing them if out of screen or hit an invader), moving and spawning new invaders
;(define (advance-game-state game) game) ; stub
(check-equal? (advance-game-state (make-game-with-default-score empty empty (make-tank (/ WIDTH 2) 1))) (make-game-with-default-score empty empty (make-tank (+ (/ WIDTH 2) TANK-SPEED) 1)))
(check-equal? (advance-game-state (make-game-with-default-score empty (list (make-missile (/ WIDTH 2) (/ HEIGHT 2))) (make-tank (/ WIDTH 2) 1)))
              (make-game-with-default-score empty (list (make-missile (/ WIDTH 2) (- (/ HEIGHT 2) MISSILE-SPEED))) (make-tank (+ (/ WIDTH 2) TANK-SPEED) 1)))
(check-equal? (advance-game-state (make-game-with-default-score (list (make-invader (/ WIDTH 2) (/ HEIGHT 2) 1)) (list (make-missile (/ WIDTH 2) 0)) (make-tank (/ WIDTH 2) 1)))
              (make-game-with-default-score (list (make-invader (+ (/ WIDTH 2) (* 1 INVADER-X-SPEED)) (+ (/ HEIGHT 2) (* 1 INVADER-Y-SPEED)) 1)) empty (make-tank (+ (/ WIDTH 2) TANK-SPEED) 1)))

;; Game -> Boolean
;; Checks if the game has ended, which happens when an invader reaches the bottom of the screen
;(define (terminate-game? game) false) ; stub
(check-equal? (terminate-game? (make-game-with-default-score empty empty T0)) false)
(check-equal? (terminate-game? (make-game-with-default-score (list (make-invader 0 0 10)) empty T0)) false)
(check-equal? (terminate-game? (make-game-with-default-score (list (make-invader 0 HEIGHT -10) (make-invader 0 0 10)) empty T0)) true)
(check-equal? (terminate-game? (make-game-with-default-score (list (make-invader 0 0 -10) (make-invader 0 HEIGHT 10)) empty T0)) true)
(check-equal? (terminate-game? (make-game-with-default-score (list (make-invader 0 0 -10) (make-invader 0 HEIGHT 10) (make-invader 0 0 -10)) (list M2) T0)) true)
(check-equal? (terminate-game? (make-game-with-default-score (list (make-invader 0 0 -10) (make-invader 0 0 11) (make-invader 0 HEIGHT 12)) (list M1) T0)) true)


;; (listOf Invader) -> Boolean
;; Returns true if any invader in a list of invaders has reached the bottom of the screen
;(define (any-invader-landed? loi) false) ; stub
(check-equal? (any-invader-landed? empty) false)
(check-equal? (any-invader-landed? (list (make-invader 0 0 10))) false)
(check-equal? (any-invader-landed? (list (make-invader 0 0 10) (make-invader 0 0 -10))) false)
(check-equal? (any-invader-landed? (list (make-invader WIDTH (- (- HEIGHT INVADER-HEIGHT/2) 1) 10) (make-invader 0 0 10) (make-invader 0 0 -10))) false)
(check-equal? (any-invader-landed? (list (make-invader 0 0 10) (make-invader WIDTH (- (- HEIGHT INVADER-HEIGHT/2) 1) 10) (make-invader 0 0 -10))) false)
(check-equal? (any-invader-landed? (list (make-invader 0 0 10) (make-invader WIDTH HEIGHT 10) (make-invader 0 0 -10))) true)
(check-equal? (any-invader-landed? (list (make-invader WIDTH HEIGHT 10) (make-invader 0 0 10) (make-invader 0 0 -10))) true)
(check-equal? (any-invader-landed? (list (make-invader 0 0 10) (make-invader 0 0 -10) (make-invader WIDTH HEIGHT 10))) true)


;; Invader -> Boolean
;; Returns true if an Invader has reached the bottom of the screen
;(define (invader-landed? (make-invader 0 0 10)) false) ; stub 
(check-equal? (invader-landed? (make-invader 0 0 10)) false)
(check-equal? (invader-landed? (make-invader 10 (sub1 INVADER-LANDING-HEIGHT) -10)) false)
(check-equal? (invader-landed? (make-invader 20 HEIGHT 10)) true)
(check-equal? (invader-landed? (make-invader 30 (add1 INVADER-LANDING-HEIGHT) -10)) true)


;; Game KeyEvent -> Game
;; Control the movement of the Tank with the left and right keys and fire missiles with the space bar.
;(define (control-game game kevnt) (make-game-with-default-score empty empty T1)) ;stub
(check-equal? (control-game G0 "a") G0)
(check-equal? (control-game G3 "a") G3)
(check-equal? (control-game G0 "left") (make-game-with-default-score (game-invaders G0) (game-missiles G0) (make-tank (tank-x (game-tank G0)) -1)))
(check-equal? (control-game G3 "left") (make-game-with-default-score (game-invaders G3) (game-missiles G3) (make-tank (tank-x (game-tank G3)) -1)))
(check-equal? (control-game G0 "right") (make-game-with-default-score (game-invaders G0) (game-missiles G0) (make-tank (tank-x (game-tank G0)) 1)))
(check-equal? (control-game G3 "right") (make-game-with-default-score (game-invaders G3) (game-missiles G3) (make-tank (tank-x (game-tank G3)) 1)))
(check-equal? (control-game G0 " ") (make-game-with-default-score (game-invaders G0)
                                               (cons (make-missile (tank-x (game-tank G0)) TANK-MISSILE-LAUNCHER-Y) (game-missiles G0))
                                               (game-tank G0)))
(check-equal? (control-game G3 " ") (make-game-with-default-score (game-invaders G3)
                                               (cons (make-missile (tank-x (game-tank G3)) TANK-MISSILE-LAUNCHER-Y) (game-missiles G3))
                                               (game-tank G3)))

              

;; Game Integer[-1, 1] -> Game
;; Change tank direction. -1 means direction left, 1 direction right
;(define (update-tank-direction game new-dir) game) ;stub
(check-equal? (update-tank-direction (make-game-with-default-score empty empty (make-tank 0 -1)) -1) (make-game-with-default-score empty empty (make-tank 0 -1)))
(check-equal? (update-tank-direction (make-game-with-default-score empty empty (make-tank 0 -1))  1) (make-game-with-default-score empty empty (make-tank 0  1)))
(check-equal? (update-tank-direction (make-game-with-default-score empty empty (make-tank 0  1))  1) (make-game-with-default-score empty empty (make-tank 0  1)))
(check-equal? (update-tank-direction (make-game-with-default-score empty empty (make-tank 0  1)) -1) (make-game-with-default-score empty empty (make-tank 0 -1)))
(check-equal? (update-tank-direction G3 -1) (make-game-with-default-score (game-invaders G3) (game-missiles G3) (make-tank (tank-x (game-tank G3)) -1)))
(check-equal? (update-tank-direction G3 1) (make-game-with-default-score (game-invaders G3) (game-missiles G3) (make-tank (tank-x (game-tank G3)) 1)))


;; Game -> Game
;; Fire a missile from the tank x,y position
;(define (fire-missile game) game) ; stub
(check-equal? (fire-missile (make-game-with-default-score empty empty (make-tank 0 -1))) (make-game-with-default-score empty (list (make-missile 0 TANK-MISSILE-LAUNCHER-Y)) (make-tank 0 -1)))
(check-equal? (fire-missile (make-game-with-default-score (list (make-invader 100 100 10)) empty (make-tank 0 -1)))
              (make-game-with-default-score (list (make-invader 100 100 10))
                         (list (make-missile 0 TANK-MISSILE-LAUNCHER-Y))
                         (make-tank 0 -1)))
(check-equal? (fire-missile
               (make-game-with-default-score (list (make-invader 100 100 10) (make-invader 200 200 -20))
                          (list (make-missile 10 10) (make-missile 20 20))
                          (make-tank 110 1)))
              (make-game-with-default-score (list (make-invader 100 100 10) (make-invader 200 200 -20))
                         (list (make-missile 110 TANK-MISSILE-LAUNCHER-Y) (make-missile 10 10) (make-missile 20 20))
                         (make-tank 110 1)))


;; Integer Integer (listof Missile) -> (listof Missile)
;; creates a new missile at location x, y and appends to a list of missiles
;(define (add-new-missile x y lom) lom) ; stub
(check-equal? (add-new-missile 0 0 empty) (cons (make-missile 0 0) empty))
(check-equal? (add-new-missile 0 0 (list (make-missile 10 10))) (list (make-missile 0 0) (make-missile 10 10)))
(check-equal? (add-new-missile 0 0 (list (make-missile 10 10) (make-missile 20 20))) (list (make-missile 0 0) (make-missile 10 10) (make-missile 20 20)))



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
