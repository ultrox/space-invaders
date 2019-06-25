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

(define INVADE-RATE 1) ; 1% chance to get invader per tick

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
(define TANK-WIDTH/2 (/ (image-width TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))
(define MISSLE-ORIGIN (- HEIGHT (image-height TANK)))

(define MTS (empty-scene WIDTH HEIGHT))

;; Controls
(define RIGHT "l")
(define LEFT "h")
(define SPACE " ")

;; FOR TESTING
(define RENDER-TANKx100 (place-image TANK 100 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))

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

;; Missiles is one of:
;;  - empty
;;  - (cons Missle Missles)
;; interp. list of missles

(define MSLS1 empty)
(define MSLS2 (list (make-missile 100 100) (make-missile 200 200)))

#;
(define (fn-for-msls msls)
  (cond [(empty? msls) ...]
        [else
         (... (fn-for-missile (first msls))
              (fn-for-msls (rest msls)))]))

;; =================
;; Functions:
(define T101 (make-tank 0 -1))
;; Game -> Game
;; start the world with (main (make-game empty empty T101))
;; 
(define (main dx)
  (big-bang (make-game empty empty (make-tank 0 dx))                      ; Game
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
(define (next-game game) (make-game (manage-invaders (game-invaders game))
                                    (advance-msls (game-missiles game))
                                    (advance-tank (game-tank game))))

;; Tank -> Tank
;; advance x of tank given speed & dx until edges of world

(check-expect (advance-tank (make-tank 15 -1))              ; normal advance right
              (make-tank (+ 15 (* TANK-SPEED (- -1))) -1))

(check-expect (advance-tank (make-tank 20 1))               ; normal advance left
              (make-tank (+ 20 (* TANK-SPEED (- 1))) 1))

(check-expect (advance-tank (make-tank (- WIDTH 2) -1))
              (make-tank (- WIDTH TANK-WIDTH/2) -1))

(check-expect (advance-tank (make-tank 0 1))               ; l stop before boundary edge
              (make-tank TANK-WIDTH/2 1))

; (define (advance-tank t) t) ;stub

(define (advance-tank t)
  (cond [(>= (+ (tank-x t) (* TANK-SPEED (- (tank-dx t)))) (- WIDTH TANK-WIDTH/2))
         (make-tank (- WIDTH TANK-WIDTH/2) (tank-dx t))]
        [(<= (+ (tank-x t) (* TANK-SPEED (- (tank-dx t)))) TANK-WIDTH/2)
         (make-tank TANK-WIDTH/2 (tank-dx t))]
        [else
         (make-tank (+ (tank-x t) (* TANK-SPEED (- (tank-dx t))))
                    (tank-dx t))]))


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
  (render-invaders (game-invaders g) (render-missiles
                                      (game-missiles g)
                                      (render-tank (game-tank g)))))


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
        [(key=? ke SPACE) (make-game  (game-invaders g)
                                      (shoot (game-missiles g) (game-tank g))
                                      (game-tank g))]
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


;; Missles Tank -> Missles
;; add new missile to the list 'shooting' of missiles from tank x-axis
(check-expect (shoot empty (make-tank 10 -1)) (list (make-missile 10 MISSLE-ORIGIN))) 
(check-expect (shoot (list (make-missile 10 20)) (make-tank 100 -1))
              (list (make-missile 100 MISSLE-ORIGIN) (make-missile 10 20)))


; (define (shoot lom t) empty) ; stub

; <Template from Missiles>

(define (shoot msls t)
  (cond [(empty? msls) (list (make-missile (tank-x t) MISSLE-ORIGIN))]
        [else
         (cons (make-missile (tank-x t) MISSLE-ORIGIN) msls)]))

;; Missiles Image -> Image
;; render missiles on appropriate x,y possitions
(check-expect (render-missiles empty RENDER-TANKx100)
              RENDER-TANKx100)                                    ; no missiles (base case)
(check-expect (render-missiles (list
                                (make-missile 100 MISSLE-ORIGIN)  ; just fired
                                (make-missile 200 200)            ; normal 
                                (make-missile 20 0))              ; exiting world view
                               RENDER-TANKx100)             
              (place-image MISSILE 20 0
                           (place-image MISSILE 200 200
                                        (place-image MISSILE 100 MISSLE-ORIGIN RENDER-TANKx100))))


; (define (render-missiles msls img) img) ; stub
; <Template from Missiles>

(define (render-missiles msls img)
  (cond [(empty? msls) img]
        [else
         (place-image MISSILE
                      (missile-x (first msls)) ; helper not needed, not complex
                      (missile-y (first msls))
                      (render-missiles (rest msls) img))]))


;; Missiles -> Missiles
;; advance each y-axis of a given missile in the list
(check-expect (advance-msls empty) empty)
(check-expect (advance-msls (list (make-missile 20 MISSLE-ORIGIN)))
              (list (make-missile 20 (- MISSLE-ORIGIN MISSILE-SPEED))))

(check-expect (advance-msls (list (make-missile 20 100) (make-missile 20 200)))
              (list (make-missile 20 (- 100 MISSILE-SPEED))
                    (make-missile 20 (- 200 MISSILE-SPEED))))

(check-expect (advance-msls (list (make-missile 20 -10) (make-missile 20 MISSLE-ORIGIN)))
              (list (make-missile 20 (- MISSLE-ORIGIN MISSILE-SPEED))))

; (define (advance-msls msls) msls) ;stub
; <Template from Missiles>

(define (advance-msls msls)
  (cond [(empty? msls) empty]
        [else
         (if (out-range? (first msls)) ; knowlage shift
             (advance-msls (rest msls))
             (cons (advance-y (first msls)) (advance-msls (rest msls))))]))

;; Missile -> Boolean
;; return true if missile y is smaller then 0 (outside of visible canvas)
(check-expect (out-range? (make-missile 30            MISSLE-ORIGIN)) false) ; don't filter #false
(check-expect (out-range? (make-missile 30                   HEIGHT)) false) ; filter #true
(check-expect (out-range? (make-missile 30      (- 0 MISSILE-SPEED)))  true)  ; outside visible canvas

(define (out-range? m)
  (< (missile-y m) 0))

;; Missile -> Missile
;; advance y of missile by MISSILE-SPEED
(check-expect (advance-y (make-missile 20 200))
              (make-missile 20 (- 200 MISSILE-SPEED)))

; (define (advance-y m) m) ; stub
; <Template from Missile>

(define (advance-y m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))

;; INVADERS
;; ========

;; Invaders is one of:
;;  - empty
;;  - (cons Invader Invaders)
;; interp. list of invaders

(define INVS1 empty)
(define INVS2 (list I1 I2))

#;
(define (fn-for-invaders loi)
  (cond [(empty? loi) ...]
        [else
         (... (fn-for-invader (first loi))
              (fn-for-invaders (rest loi)))]))

;; Invaders -> Invaders
;; manages appearence rendering and advancing of enemies
;; composition, no tests, un-pure functions

(define (manage-invaders loi)
  (add-new-invader (advance-invaders loi)
                   (random 100)
                   (random WIDTH)
                   (random 100)))


(check-expect (advance-invaders empty) empty)
(check-expect (advance-invaders (list (make-invader 4 4 1)
                                      (make-invader 5 5 1)
                                      (make-invader 6 6 -1)))
              (list
               (make-invader (- 4 INVADER-X-SPEED) (+ 4 INVADER-Y-SPEED)  1)
               (make-invader (- 5 INVADER-X-SPEED) (+ 5 INVADER-Y-SPEED)  1)
               (make-invader (+ 6 INVADER-X-SPEED) (+ 6 INVADER-Y-SPEED) -1)))

; (define (advance-invaders loi) loi) ;stub

(define (advance-invaders loi)
  (cond [(empty? loi) empty]
        [else
         (cons (next-invader (first loi))
               (advance-invaders (rest loi)))]))


; Invader -> Invader
;; produce invaders next position in the same dx or bounce if hit boundary
;; !!!
(check-expect (next-invader (make-invader 10 11 -1))          ; continue right
              (make-invader (- 10 (* INVADER-X-SPEED -1)) (+ 11 INVADER-Y-SPEED) -1))

(check-expect (next-invader (make-invader 11 12 1))           ; continue left
              (make-invader (- 11 (* INVADER-X-SPEED 1)) (+ 12 INVADER-Y-SPEED) 1))


(define (next-invader i)
  (make-invader (- (invader-x i) (* INVADER-X-SPEED (invader-dx i))) ; go fruther right dx=-1
                (+ (invader-y i) INVADER-Y-SPEED)                    ; go left dx=1
                (invader-dx i)))



;; Invaders Number Number -> Invaders
;; add new invader to list of invaders on random x and random dx depending on INVADE-RATE

(check-random (add-new-invader empty (+ INVADE-RATE 5) 30 2) empty) ; will not gen invader

(check-random (add-new-invader empty INVADE-RATE 30 3)              ; generate invader with dx left 1
              (list (make-invader 30 0 1)))
 
(check-random (add-new-invader empty INVADE-RATE 30 2)              ; generate invader with dx right -1
              (list (make-invader 30 0 -1)))

(check-random (add-new-invader empty 10 30 2) empty)
; (define (add-new-invader loi rdr rw) empty) ; stub

;; rdr (random rate) rw random width

(define (add-new-invader loi rdr rw rdx)
  (if (>= INVADE-RATE rdr)
      (cons (make-invader rw 0 (random-dx rdx)) loi)
      loi))

;; Number -> -1 1
;; given even number, retrun -1(right dx) odd 1(left dx)
;; !!! make new Data Definition for direction
(define (random-dx n) (if (zero? (modulo n 2)) -1 1))

;; Invaders -> Image
;; render each invader from list at their respective x,y

; (define (render-invaders loi img) img) ; stub


(define (render-invaders loi img)
  (cond [(empty? loi) img]
        [else
         (place-image INVADER
                      (invader-x (first loi)) ; helper not needed, not complex
                      (invader-y (first loi))
                      (render-invaders (rest loi) img))]))
