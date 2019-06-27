;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

(define INVADE-RATE 2) ; 2% chance to get invader per tick, bigger number more invaders

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define INVADER-WIDTH/2 (/ (image-width INVADER) 2))
(define R-WALL-OFFSET (- WIDTH INVADER-WIDTH/2))
(define L-WALL-OFFSET INVADER-WIDTH/2)

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))
(define TANK-WIDTH/2 (/ (image-width TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))
(define MISSILE-ORIGIN (- HEIGHT (image-height TANK)))

(define MTS (empty-scene WIDTH HEIGHT))
(define FAIL-MSG-IMG (text "YOU LOST" 24 "olive"))
(define SUCC-MSG-IMG (text "YOU LOST" 24 "olive"))

;; Controls
(define RIGHT "right")
(define LEFT "left")
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

;; Game -> Game
;; start the world with (main -1)
;; 
(define (main dx)
  (big-bang (make-game empty empty (make-tank 0 dx))                      ; Game
    (on-tick   next-game)             ; Game -> Game
    (to-draw   render-game)           ; Game -> Image
    (on-release handle-shooting)      ; Game KeyEvent -> Game
    (stop-when game-end? render-last) ; Game -> Boolean
    (on-key    handle-key)))          ; Game KeyEvent -> Game

;; Game -> Game
;; produce the next state of the game

; Movement
;; NORMAL going right increment x by SPEED

(check-expect (next-game (make-game empty empty (make-tank 10 -1)))
              (make-game empty empty (make-tank 15 -1)))


;(define (next-game game) game) ; stub
(define (next-game game) (make-game (manage-invaders (game-invaders game)
                                                     (game-missiles game))
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

(define (render-game g)
  (render-invaders (game-invaders g) (render-missiles
                                      (game-missiles g)
                                      (render-tank (game-tank g)))))

;; Tank -> Image
;; render TANK on BACKGROUND based on tank-x
(check-expect (render-tank (make-tank 30 -1))
              (place-image TANK 30 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))

; (define (render-tank t) empty-image) ; stub

(define (render-tank t)
  (place-image TANK (tank-x t) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))

;; Game KeyEvent -> Game
;; controle direction of tank based on key-events in RIGHT LEFT constants

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

;; Game KeyEvent -> Game
;; shoot a missile when user relese SPACE key
(check-expect (handle-shooting (make-game empty empty (make-tank 40 -1)) "w")
              (make-game empty empty (make-tank 40 -1)))

(check-expect (handle-shooting (make-game empty empty (make-tank 40 -1)) SPACE)
              (make-game empty (list (make-missile 40 MISSILE-ORIGIN)) (make-tank 40 -1)))

; (define (handle-shooting game ke) game) ;stub

; <Template from recepie handle-key>

(define (handle-shooting g ke)
  (cond [(key=? ke SPACE) (make-game  (game-invaders g)
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
(check-expect (shoot empty (make-tank 10 -1)) (list (make-missile 10 MISSILE-ORIGIN))) 
(check-expect (shoot (list (make-missile 10 20)) (make-tank 100 -1))
              (list (make-missile 100 MISSILE-ORIGIN) (make-missile 10 20)))


; (define (shoot lom t) empty) ; stub

; <Template from Missiles>

(define (shoot msls t)
  (cond [(empty? msls) (list (make-missile (tank-x t) MISSILE-ORIGIN))]
        [else
         (cons (make-missile (tank-x t) MISSILE-ORIGIN) msls)]))

;; Missiles Image -> Image
;; render missiles on appropriate x,y possitions
(check-expect (render-missiles empty RENDER-TANKx100)
              RENDER-TANKx100)                                    ; no missiles (base case)
(check-expect (render-missiles (list
                                (make-missile 100 MISSILE-ORIGIN)  ; just fired
                                (make-missile 200 200)            ; normal 
                                (make-missile 20 0))              ; exiting world view
                               RENDER-TANKx100)             
              (place-image MISSILE 20 0
                           (place-image MISSILE 200 200
                                        (place-image MISSILE 100 MISSILE-ORIGIN RENDER-TANKx100))))


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
(check-expect (advance-msls (list (make-missile 20 MISSILE-ORIGIN)))
              (list (make-missile 20 (- MISSILE-ORIGIN MISSILE-SPEED))))

(check-expect (advance-msls (list (make-missile 20 100) (make-missile 20 200)))
              (list (make-missile 20 (- 100 MISSILE-SPEED))
                    (make-missile 20 (- 200 MISSILE-SPEED))))

(check-expect (advance-msls (list (make-missile 20 -10) (make-missile 20 MISSILE-ORIGIN)))
              (list (make-missile 20 (- MISSILE-ORIGIN MISSILE-SPEED))))

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
(check-expect (out-range? (make-missile 30            MISSILE-ORIGIN)) false) ; don't filter #false
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

(define (manage-invaders loi lom)
  (filter-dead (add-new-invader (advance-invaders loi)
                                (random 100)
                                (random WIDTH)
                                (random 100))
               lom))


(check-expect (advance-invaders empty) empty)
(check-expect (advance-invaders (list (make-invader 14 14 1)
                                      (make-invader 15 15 1)
                                      (make-invader 16 16 -1)))
              (list
               (make-invader (- 14 INVADER-X-SPEED) (+ 14 INVADER-Y-SPEED)  1)
               (make-invader (- 15 INVADER-X-SPEED) (+ 15 INVADER-Y-SPEED)  1)
               (make-invader (+ 16 INVADER-X-SPEED) (+ 16 INVADER-Y-SPEED) -1)))

; (define (advance-invaders loi) loi) ;stub

(define (advance-invaders loi)
  (cond [(empty? loi) empty]
        [else
         (cons (next-invader (first loi))
               (advance-invaders (rest loi)))]))


; Invader -> Invader
;; produce invaders next position in the same dx or bounce if hit boundary
(check-expect (next-invader (make-invader 30 11 -1))                  ; continue right
              (make-invader (- 30 (* INVADER-X-SPEED -1)) (+ 11 INVADER-Y-SPEED) -1))

(check-expect (next-invader (make-invader 40 12 1))                   ; continue left
              (make-invader (- 40 (* INVADER-X-SPEED 1)) (+ 12 INVADER-Y-SPEED) 1))

(check-expect (next-invader (make-invader INVADER-WIDTH/2 13 1))      ; exactly at l boundary going l flip to r
              (make-invader (- INVADER-WIDTH/2 (* INVADER-X-SPEED -1)) (+ 13 INVADER-Y-SPEED) -1))

(check-expect (next-invader (make-invader R-WALL-OFFSET 14 -1))       ; exactly at r boundary going r flip to l
              (make-invader (- R-WALL-OFFSET (* INVADER-X-SPEED 1)) (+ 14 INVADER-Y-SPEED) 1))

(check-expect (next-invader (make-invader (- INVADER-WIDTH/2 1) 15 1)); pass left boundary correct and flip l->r
              (make-invader INVADER-WIDTH/2 15 -1))

(check-expect (next-invader (make-invader (- R-WALL-OFFSET 1) 16 -1)) ; pass right boundary correct and flip r->l
              (make-invader R-WALL-OFFSET 16 1))

(define (next-invader i)
  (cond [(= (invader-x i) R-WALL-OFFSET)                                        ; exactly at r flip to l
         (make-invader (- R-WALL-OFFSET (* INVADER-X-SPEED 1))
                       (+ (invader-y i) INVADER-Y-SPEED) 1)] 
        [(= L-WALL-OFFSET (invader-x i))                                        ; exactly at l flip to r
         (make-invader (- (invader-x i) (* INVADER-X-SPEED -1))              
                       (+ (invader-y i) INVADER-Y-SPEED) -1)]
        [(< (- (invader-x i) (* INVADER-X-SPEED (invader-dx i))) L-WALL-OFFSET) ; pass left 
         (make-invader L-WALL-OFFSET
                       (invader-y i) -1)]
        [(> (- (invader-x i) (* INVADER-X-SPEED (invader-dx i))) R-WALL-OFFSET) ; pass right 
         (make-invader R-WALL-OFFSET (invader-y i) 1)]
        [else
         (make-invader (- (invader-x i) (* INVADER-X-SPEED (invader-dx i))) ; go fruther right dx=-1
                       (+ (invader-y i) INVADER-Y-SPEED)                    ; go left dx=1
                       (invader-dx i))]))

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

(define (random-dx n) (if (zero? (modulo n 2)) -1 1))

;; Game -> Boolean
;; return true when invader-y is equal to WIDTH
(check-expect (game-end? (make-game empty empty T0)) false)
(check-expect (game-end? (make-game (list (make-invader 40 40 -1)) empty T0)) false)
(check-expect (game-end? (make-game
                          (list (make-invader 10 0 -1)
                                (make-invader 40 HEIGHT -1))
                          empty
                          T0))
              true)

; (define (game-end? g) false) ;stub

(define (game-end? s)
  (cond [(empty? (game-invaders s)) false]
        [(invader-landed? (first (reverse (game-invaders s)))) true]
        [else false]))

;; Invader -> Boolean
;; produce true if invader has landed
;; landed means: invader-y >= WIDTH
(check-expect (invader-landed? (make-invader 30 40 -1)) false)
(check-expect (invader-landed? (make-invader 30 HEIGHT -1)) true)

; (define (invader-landed? i) false) ;stub

(define (invader-landed? i) (>= (invader-y i) HEIGHT))

;; Game -> Image
;; render FAIL-MSG-IMG when invader lands
(check-expect (render-last (make-game empty empty T1))
              (place-image FAIL-MSG-IMG (/ WIDTH 2) (/ HEIGHT 2) MTS))

; (define (render-last g) (empty-scene)) ;stub

(define (render-last g)
  (place-image FAIL-MSG-IMG (/ WIDTH 2) (/ HEIGHT 2) MTS))

;; Invaders Missles -> Invaders
;; remove specific hit Invader from Invaders based on result of hit? fn

(check-expect (filter-dead empty empty) empty)
(check-expect (filter-dead (list (make-invader 30 30 -1))
                           empty)
              (list (make-invader 30 30 -1)))

(check-expect (filter-dead (list (make-invader 20 30 -1) ; hit
                                 (make-invader 40 40 -1)
                                 (make-invader 50 50 -1))
                           (list (make-missile 30 30)    ; in HIT-RANGE 
                                 (make-missile 60 MISSILE-ORIGIN)))
              (list (make-invader 50 50 -1)))

; (define (filter-dead loi lom) empty) ; stub

(define (filter-dead loi lom)
  (cond [(empty? loi) empty]
        [else
         (cond [(hit? (first loi) lom) (filter-dead (rest loi) lom)]
               [else
                (cons (first loi) (filter-dead (rest loi) lom))])]))


;; Invader Missiles -> Boolean
;; return true when x, y of invader is the same of missile,
;; factoring in HIT-RANGE as offset

(check-expect (hit? (make-invader 200 250 -1) ; no missiles, miss
                    empty) false)

(check-expect (hit? (make-invader 210 260 -1) ; x & y in HIT-RANGE
                    (list (make-missile 20 250)
                          (make-missile 150 MISSILE-ORIGIN)
                          (make-missile 200 250))) true)

(check-expect (hit? (make-invader 211 261 -1) ; x & y out HIT-RANGE
                    (list (make-missile 20 250)
                          (make-missile 150 MISSILE-ORIGIN)
                          (make-missile 200 250))) false)

(check-expect (hit? (make-invader 210 261 -1) ; x in HIT-RANGE y out
                    (list (make-missile 20 250)
                          (make-missile 150 MISSILE-ORIGIN)
                          (make-missile 200 250))) false)

(check-expect (hit? (make-invader 211 260 -1) ; y in HIT-RANGE x out
                    (list (make-missile 20 250)
                          (make-missile 150 MISSILE-ORIGIN)
                          (make-missile 200 250))) false)

; the same y, different x miss
; the same x different y miss

;(define (hit? i lom) false) ;stub

; <Template from Missiles>

(define (hit? i msls)
  (cond [(empty? msls) false]
        [else
         (cond [(collision? (first msls) i) true]
               [else (hit? i (rest msls))])]))

;; Missile Invader -> Boolean
;; produce true if Missile is in HIT-RANGE of Invader

(check-expect (collision? (make-missile (- 200 HIT-RANGE) 120) ; just in HIT-RANGE
                          (make-invader 200 120 -1)) true)

(check-expect (collision? (make-missile (- 200 HIT-RANGE) 120) ; 1px OUT HIT-RANGE
                          (make-invader 201 120 -1)) false)

(check-expect (collision? (make-missile 100 MISSILE-ORIGIN)
                          (make-invader 100 10 -1)) false)

; (define (collision? m i) false) ; stub

(define (collision? m i)
  (if (and (<= (abs (- (invader-x i) (missile-x m))) HIT-RANGE)
           (<= (abs (- (invader-y i) (missile-y m))) HIT-RANGE))
      true
      false))


;; Invaders -> Image
;; render each invader from list at their respective x,y
(check-expect (render-invaders empty MTS) MTS)
(check-expect (render-invaders (list (make-invader 20 20 -1)
                                     (make-invader 50 50 -1)
                                     (make-invader 100 100 1)) MTS)
              (place-image INVADER 20 20
                           (place-image INVADER 50 50
                                        (place-image INVADER 100 100 MTS))))

; (define (render-invaders loi img) img) ; stub

; <Template from Invaders>

(define (render-invaders loi img)
  (cond [(empty? loi) img]
        [else
         (place-image INVADER
                      (invader-x (first loi))
                      (invader-y (first loi))
                      (render-invaders (rest loi) img))]))
