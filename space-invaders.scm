(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 3)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))
(define MTS (empty-scene WIDTH HEIGHT))

;; Controls
(define RIGHT "l")
(define LEFT "h")
(define SHOOT " ")

;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dx))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dx -1, right if dx 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dx t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
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


;; =================
;; Functions:
(define T101 (make-tank 0 -1))
;; Game -> Game
;; start the world with (main (make-game empty empty T101))
;; 
(define (main game)
  (big-bang game                      ; Game
    (on-tick   next-game)     ; Game -> Game
    (to-draw   render-game)   ; Game -> Image
    (on-key    handle-key)))  ; Game KeyEvent -> Game

;; Game -> Game
;; produce the next state of the game
;; !!!

; Movement
;; NORMAL going right increment x by SPEED
#;
(check-expect (next-game (make-game empty empty (make-tank 10 -1)))
              (make-game empty empty (make-tank (+ TANK-SPEED (- -1) 10) -1)))

;(define (next-game game) game) ; stub
(define (next-game game) (make-game empty
                                    empty
                                    (advance-tank (game-tank game))))

;; Tank -> Tank
;; advance x of tank given speed & dx until edges of world

(check-expect (advance-tank (make-tank 10 -1))              ; normal advance right
              (make-tank (+ 10 (* TANK-SPEED (- -1))) -1))

(check-expect (advance-tank (make-tank 20 1))               ; normal advance left
              (make-tank (+ 20 (* TANK-SPEED (- 1))) 1))


; (define (advance-tank t) t) ;stub
(define (advance-tank t)
  (make-tank (+ (tank-x t) (* TANK-SPEED (- (tank-dx t)))) (tank-dx t)))

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dx t)))


;; Game -> Image
;; render game with all its elements 
;; !!!
#;
(check-expect (render-game (make-game empty empty T0)) ;; exapmle 
              (place-image TANK (tank-x T1) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))

(define (render-game g)
  (render-tank (game-tank g)))

(define (render-tank t)
  (place-image TANK (tank-x t) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))

;; Game KeyEvent -> Game
;; controle direction of tank & missle shooting

; SHOOTING CASE
;;!!!

; RIGHT CASES
(check-expect (handle-key (make-game empty empty (make-tank 40 1)) RIGHT)
              (make-game empty empty (make-tank 40 -1)))

; LEFT CASES
(check-expect (handle-key (make-game empty empty (make-tank 40 -1)) LEFT)
              (make-game empty empty (make-tank 40 1)))
; ELSE CASE
(check-expect (handle-key (make-game empty empty (make-tank 40 -1)) "w")
              (make-game empty empty (make-tank 40 -1)))


; (define (handle-key game ke) game) ;stub

; <Template from recepie handle-key>

(define (handle-key g ke)
  (cond [(key=? ke LEFT) (make-game  (game-invaders g)
                                     (game-missiles g)
                                     (turn-tank (game-tank g) 1))]
        [(key=? ke RIGHT) (make-game  (game-invaders g)
                                      (game-missiles g)
                                      (turn-tank (game-tank g) -1))]
        [else g]))

;; Tank ndx -> Tank
;; produce correct new x position based given ndx(next direction)

; (define (turn-tank t ndx) t) ;stub

(check-expect (turn-tank (make-tank 100   1)  1) (make-tank 100  1)) ; in left  stay left
(check-expect (turn-tank (make-tank 100  -1) -1) (make-tank 100 -1)) ; in right stay right

(check-expect (turn-tank (make-tank 100   1) -1) (make-tank 100 -1)) ; in left  switch right
(check-expect (turn-tank (make-tank 100  -1)  1) (make-tank 100  1)) ; in right switch left

(define (turn-tank t ndx)
  (if (< ndx 0)
      (make-tank (tank-x t) -1)  ; right
      (make-tank (tank-x t) 1))) ; left

