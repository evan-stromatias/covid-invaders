#lang racket

(require 2htdp/universe)
(require 2htdp/image)
(require threading)

(provide
 (all-defined-out))


;; Space Invaders
;;; Start the game with (main (make-game empty empty T0))


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
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


;; Data Definitions:

(define-struct game (invaders missiles tank) #:transparent)
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir) #:transparent)
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))


;;; !!! Add animation frame to invader
(define-struct invader (x y dx) #:transparent)
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y) #:transparent)
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))



(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))

;; ===============
;; Functions:

;; Game -> Game
;; start the world with (main (make-game empty empty T0))
(define (main game)
  (big-bang    game                    ; Game
    (on-tick   advance-game-state)     ; Game -> Game
    (to-draw   render-game)            ; Game -> Image
    (stop-when terminate-game?)        ; Game -> Boolean
    (on-key    control-game)))         ; Game KeyEvent -> Game






;; Game -> Game
;; advances tank based on its direction
;(define (advance-game-tank game) game) ; stub
(define (advance-game-tank game)
  (make-game (game-invaders game)
             (game-missiles game)
             (advance-tank (game-tank game))))


;; Tank -> Tank
;; Updates the x location of the tank based on its direction
;(define (advance-tank tank) tank) ; stub
(define (advance-tank tank)
  (if (is-next-tank-position-valid? tank)
      (make-tank (next-tank-x-location tank) (tank-dir tank))
      tank))



;; Tank -> Boolean
;; Returns true if the next tank movement is within the screen borders and thus valid
;(define (is-next-tank-position-valid? tank) true) ; stub
(define (is-next-tank-position-valid? tank)
  (and
   (> (next-tank-x-location tank) TANK-WIDTH/2)
   (< (next-tank-x-location tank) (- WIDTH TANK-WIDTH/2))))



;; Tank -> Natural
;; Helper function that returns a tank's next x coordinate based on its current direction and TANK-SPEED
;(define (move-tank-based-on-direction tank) tank) ; stub
(define (next-tank-x-location tank)
  (+ (tank-x tank) (* TANK-SPEED (tank-dir tank))))



;; Game -> Game
;; advances missiles upwards by MISSILE-SPEED
;(define (advance-game-missiles game) game) ; stub
(define (advance-game-missiles game)
  (make-game (game-invaders game)
             (advance-missiles (game-missiles game))
             (game-tank game)))



;; (listOf Missile) -> (listOf Missile)
;; moves all missiles in a list of missiles upwards by MISSILE-SPEED
;(define (advance-missiles lom) lom) ; stub
(define (advance-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (cons (advance-missile (first lom))
               (advance-missiles (rest lom)))]))



;; Missile -> Missile
;; Advances a single missile by MISSILE-SPEED upwards
; (define (advance-missile m) m) ; stub
(define (advance-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))



;; Game -> Game
;; Removes that missiles from the Game state that are outside of the screen
;(define (filter-missiles game) game) ; stub
(define (filter-game-missiles game)
  (make-game (game-invaders game)
             (filter-missiles (game-missiles game))
             (game-tank game)))



;; (listOf Missile) -> (listOf Missile)
;; Removes the missiles from a list of missiles that are outside of the screen (less than MISSILE-OUTSIDE-OF-SCREEN-Y)
;(define (filter-missiles lom) lom) ; stub
(define (filter-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (if (missile-out-of-screen? (first lom))
             (filter-missiles (rest lom))
             (cons (first lom) (filter-missiles (rest lom))))]))



;; Missile -> Boolean
;; Returns true is a missile's y location is outside of the screen (less than MISSILE-OUTSIDE-OF-SCREEN-Y)
;(define (missile-out-of-screen? m) false) ; stub
(define (missile-out-of-screen? m)
  (< (missile-y m) MISSILE-OUTSIDE-OF-SCREEN-Y))



;; Game -> Game
;; Remove missiles and invaders that collided with each other
;(define (explode-game-objects game) game) ; stub
(define (explode-game-objects game)
  (make-game (explode-game-invaders game)                
             (explode-game-missiles game)
             (game-tank game)))



;; Game -> (listOf Invader)
;; Removes invaders that collided with missiles
;(define (explode-game-invaders game) (game-missiles game)) ; stub
(define (explode-game-invaders game)
  (game-invaders-that-didnt-collide-with-missiles (game-invaders game) (game-missiles game) ))



;; (listOf Invader) (listOf Missile) -> (listOf Invader)
;; Returns the invaders that did not collide with a missile
;(define (game-invaders-that-didnt-collide-with-missiles loi lom) loi) ; stub
(define (game-invaders-that-didnt-collide-with-missiles loi lom)
  (cond [(empty? loi) empty]
        [else
         (if (invader-hit-missiles? (first loi) lom)
             (game-invaders-that-didnt-collide-with-missiles (rest loi) lom)
             (cons (first loi) (game-invaders-that-didnt-collide-with-missiles (rest loi) lom)))]))



;; Invader (listOf Missile) -> Boolean
;; Returns true if an invader collided with a missile
;(define (invader-hit-missiles? i lom) false ) ; stub
(define (invader-hit-missiles? i lom)
  (cond [(empty? lom) false]
        [else
         (if (missile-hit-invader? (first lom) i)
             true
             (invader-hit-missiles? i (rest lom)))]))



;; Game -> (listOf Missile)
;; Remove missiles that collided with invaders
;(define (explode-game-missiles game) (game-missiles game)) ; stub
(define (explode-game-missiles game)
  (game-missiles-that-didnt-collide-with-invaders (game-missiles game) (game-invaders game)))



;; (listOf Missile) (listOf Invader) -> (listOf Missile)
;; Returns the missiles that did not collide with an invader
;(define (game-missiles-that-didnt-collide-with-invaders lom loi) lom) ; stub
(define (game-missiles-that-didnt-collide-with-invaders lom loi)
  (cond [(empty? lom) empty]
        [else
         (if (missile-hit-invaders? (first lom) loi)
             (game-missiles-that-didnt-collide-with-invaders (rest lom) loi)
             (cons (first lom) (game-missiles-that-didnt-collide-with-invaders (rest lom) loi)))]))



;; Missile (listOf Invader) -> Boolean
;; Returns true if a missile hits an invader
;(define (missile-hit-invaders? m loi) false ) ; stub
(define (missile-hit-invaders? m loi)
  (cond [(empty? loi) false]
        [else
         (if (missile-hit-invader? m (first loi))
             true
             (missile-hit-invaders? m (rest loi)))]))



;; Missile Invader -> Boolean
;; Returns true if the center point (x,y) of a missile are within a HIT-RANGE distance from the center point (x,y) of an invader
;(define (missile-hit-invader? m i) false) ; stub
(define (missile-hit-invader? m i)
  (is-point-within-hit-range? (missile-x m) (missile-y m)
                              (invader-x i) (invader-y i)
                              HIT-RANGE))



;; Number Number Number Number Number-> Boolean
;; Given some (xp,yp) coordinates and some (xc, yc) it will return true if (xp,yp) fall inside the rectangle defined by xc, yc and some hit-range distance
;(define (is-point-within-hit-range? xp yp xc yc hit-range) false) ; stub
(define (is-point-within-hit-range? xp yp xc yc hit-range)
  (and (> xp (- xc hit-range))
       (< xp (+ xc hit-range))
       (> yp (- yc hit-range))
       (< yp (+ yc hit-range))))



;; Game -> Game
;; creates a new invader randomly and returns a new game state
;(define (spawn-new-game-invaders game) game) ; stub
(define (spawn-new-game-invaders game)
  (make-game (spawn-new-invader-randomly (game-invaders game))                
             (game-missiles game)
             (game-tank game)))



;; (listOf Invader) -> (listOf Invader)
;(define (spawn-new-invader-randomly loi) loi) ; stub
(define (spawn-new-invader-randomly loi)
  (if (<= (random INVADE-RATE-ADJUSTED) INVADE-RATE)
      (cons (make-invader (+ (random (- WIDTH INVADER-WIDTH/2)) INVADER-WIDTH/2) 0 INVADER-DX) loi)
      loi))   



;; Game -> Game
;; Given a Game state, advance all game invaders by one step and return a new game state
;(define (advance-game-invaders game) game) ; stub
(define (advance-game-invaders game)
  (make-game (advance-invaders (game-invaders game))                
             (game-missiles game)
             (game-tank game)))



;; (listOf Invader) -> (listOf Invader)
;; Advances all invaders by one iteration
;(define (advance-invaders loi) loi) ; stub
(define (advance-invaders loi)
  (cond [(empty? loi) empty]
        [else
         (cons (advance-invader (first loi))
               (advance-invaders (rest loi)))]))



;; Invader -> Invader
;; Advances a single Invader based on its current and future position
;(define (advance-invader inv) inv) ; stub
(define (advance-invader inv)
  (update-invader inv
                  (is-next-invader-position-valid? inv)))
      


;; Invader Boolean -> Invader
;; Updates the position of an Invader using the current dx if use-same-dx? is true, otherwise the negated dx
;(define (update-invader (make-invader 0 0 -1) true)) ; stub
(define (update-invader inv use-same-dx?)
  (make-invader-with-custom-dx inv (if use-same-dx?
                                       (invader-dx inv)
                                       (invert-invader-dx inv))))



;; Invader Number -> Invader
;; Makes a new Invader using a new dx and ignoring the one defined in the invader
;(define (make-invader-with-custom-dx inv new-dx) inv) ; stub
(define (make-invader-with-custom-dx inv new-dx)
  (make-invader (next-x-for-invader-with-custom-dx inv new-dx)
                (next-y-for-invader-with-custom-dx inv new-dx)
                new-dx))



;; Invader Number -> Number
;; Returns Invader's next x position given its current x, a custom dx value and INVADER-X-SPEED
;(define (next-x-for-invader-with-custom-dx inv new-dx) 0) ; stub
(define (next-x-for-invader-with-custom-dx inv new-dx)
  (+ (invader-x inv) (* new-dx INVADER-X-SPEED)))



;; Invader Number -> Number
;; Returns Invader's next y position given its current y, the absolute value of a custom dx and INVADER-Y-SPEED
;(define (next-y-for-invader-with-custom-dx inv new-dx) 0) ; stub
(define (next-y-for-invader-with-custom-dx inv new-dx)
  (+ (invader-y inv) (* (abs new-dx) INVADER-Y-SPEED)))



;; Invader -> Number
;; Returns the negated dx given an Invader eg (invert-invader-dx (make-invader 0 0 -10)) -> 10)
;(define (invert-invader-dx inv) -1) ; stub
(define (invert-invader-dx inv)
  (* -1 (invader-dx inv)))



;; Invader -> Boolean
;; Returns true if Invader's next position is within the screen WIDTH coordinates
;(define (is-next-invader-position-valid? (make-invader 0 0 0) true) ; stub
(define (is-next-invader-position-valid? inv)
  (cond [(is-invader-inside-the-screen? inv) true]
        [(is-invader-outside-the-right-border-but-moving-left? inv) true]
        [(is-invader-outside-the-left-border-but-moving-right? inv) true]
        [else false]))



;; Invader -> Boolean
;; Helpers used by is-next-invader-position-valid? to make the code more readable: Returns true if invader is within (but not equal to) the screen borders
;(define (is-invader-inside-the-screen? inv) false) ; stub
(define (is-invader-inside-the-screen? inv)
  (and (> (next-x-for-invader inv) INVADER-WIDTH/2) (< (next-x-for-invader inv) (- WIDTH INVADER-WIDTH/2))))



;; Invader -> Boolean
;; Helpers used by is-next-invader-position-valid? to make the code more readable: Returns true if invader is outside the right border but it's moving left
;(define (is-invader-outside-the-right-border-but-moving-left? inv) false) ; stub
(define (is-invader-outside-the-right-border-but-moving-left? inv)
  (and (<= (next-x-for-invader inv) INVADER-WIDTH/2) (>= (invader-dx inv) 0)))



;; Invader -> Boolean
;; Helpers used by is-next-invader-position-valid? to make the code more readable: Returns true if invader is outside the left border but dx is positive
;(define (is-invader-outside-the-left-border-but-moving-right? inv) false) ; stub
(define (is-invader-outside-the-left-border-but-moving-right? inv)
  (and (>= (next-x-for-invader inv) (- WIDTH INVADER-WIDTH/2)) (<= (invader-dx inv) 0)))



;; Invader -> Natural
;; Helper function that returns the next x position of an Invader using the Invader's current dx
;(define (next-x-for-invader inv) 0) ; stub
(define (next-x-for-invader inv)
  (next-x-for-invader-with-custom-dx inv (invader-dx inv)))

;; Game -> Game
;; Produce the next game state by updating the tank, missiles (removing them if out of screen or hit an invader), moving and spawning new invaders
;(define (advance-game-state game) game) ; stub
(define (advance-game-state game)
  (~> game
      advance-game-tank
      advance-game-missiles
      filter-game-missiles
      explode-game-objects
      advance-game-invaders
      spawn-new-game-invaders))

;; Game -> Image
;; Render the game state to an image
;(define (render-game game) BACKGROUND) ; stub
(define (render-game game)
  (~>> BACKGROUND
       (render-tank (game-tank game))
       (render-missiles (game-missiles game))
       (render-invaders (game-invaders game))))



;; Tank Image -> Image
;; Render the tank in an image
;(define (render-tank tank img) BACKGROUND) ;stub
(define (render-tank tank img)
  (place-image TANK (tank-x tank) TANK-Y img))



;; (listof Invader) Image -> Image
;; Renders all the invaders on an input image
;(define (render-invaders invaders img) img) ; stub
(define (render-invaders invaders img)
  (cond [(empty? invaders) img]
        [else
         (render-invader (first invaders)
                         (render-invaders (rest invaders) img))]))



;; Invader Image -> Image
;; Renders a single Invader on an image
;(define (render-invader invader img) img) ;stub
(define (render-invader invader img)
  (place-image INVADER (invader-x invader) (invader-y invader) img))



;; (listof Missile) Image -> Image
;; Renders all missiles on an image
;(define (render-missiles missiles img) img) ;stub
(define (render-missiles missiles img)
  (cond [(empty? missiles) img]
        [else
         (render-missile (first missiles)
                         (render-missiles (rest missiles) img))]))



;; Missile Image -> Image
;; Renders a single Missile on an image
;(define (render-missile missile img) img) ; stub
(define (render-missile missile img)
  (place-image MISSILE (missile-x missile) (missile-y missile) img))



;; Game -> Boolean
;; Checks if the game has ended, which happens when an invader reaches the bottom of the screen
;(define (terminate-game? game) false) ; stub
(define (terminate-game? game)
  (any-invader-landed? (game-invaders game)))



;; (listOf Invader) -> Boolean
;; Returns true if any invader in a list of invaders has reached the bottom of the screen
;(define (any-invader-landed? loi) false) ; stub
(define (any-invader-landed? loi)
  (cond [(empty? loi) false]
        [else
         (if (invader-landed? (first loi))
             true
             (any-invader-landed? (rest loi)))]))



;; Invader -> Boolean
;; Returns true if an Invader has reached the bottom of the screen
;(define (invader-landed? (make-invader 0 0 10)) false) ; stub 
(define (invader-landed? invader)
  (> (invader-y invader) INVADER-LANDING-HEIGHT))



;; Game KeyEvent -> Game
;; Control the movement of the Tank with the left and right keys and fire missiles with the space bar.
;(define (control-game game kevnt) (make-game empty empty T1)) ;stub
(define (control-game game kevnt)
  (cond [(key=? kevnt "left") (update-tank-direction game -1)]
        [(key=? kevnt "right") (update-tank-direction game 1)]
        [(key=? kevnt " ") (fire-missile game)]
        [else
         game]))



;; Game Integer[-1, 1] -> Game
;; Change tank direction. -1 means direction left, 1 direction right
;(define (update-tank-direction game new-dir) game) ;stub
(define (update-tank-direction game new-dir)
  (make-game (game-invaders game)
             (game-missiles game)
             (make-tank (tank-x (game-tank game)) new-dir)))



;; Game -> Game
;; Fire a missile from the tank x,y position
;(define (fire-missile game) game) ; stub
(define (fire-missile game)
  (make-game (game-invaders game)
             (add-new-missile (tank-x (game-tank game)) TANK-MISSILE-LAUNCHER-Y (game-missiles game))
             (game-tank game)))



;; Integer Integer (listof Missile) -> (listof Missile)
;; creates a new missile at location x, y and appends to a list of missiles
;(define (add-new-missile x y lom) lom) ; stub
(define (add-new-missile x y lom)
  (cons (make-missile x y) lom))