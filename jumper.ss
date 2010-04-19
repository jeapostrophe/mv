#lang scheme
(require sgl
         sgl/gl
         sgl/gl-vectors
         scheme/runtime-path
         (planet jaymccarthy/gl-world)
         (planet jaymccarthy/gl2d)
         (planet jaymccarthy/chipmunk)
         "gl-sprites.ss"
         "room-gen.ss"
         "room.ss"
         "game-gen.ss"
         "../rl/lib/random.ss"
         "../rl/lib/posn.ss"
         "../rl/lib/matrix-posn.ss")

;;; Parameters
(define jumper-width 0.5)
(define jumper-height 1.7)
(define jumper-mass 74.4)
(define jump-strength 1500.0)
(define move-strength 30.0)
(define x-speed-limit 20.0)
(define y-speed-limit 150.0)
(define normal-friction 0.3)
(define-runtime-path sprites-path "samus-sprites")
(define rate 1/60)
(define substeps 3)
(define dt (exact->inexact rate))

(define samus-sprites (find-sprites sprites-path))
(define display-scale 80) ; 120 is 1080, 80 is 720
(define display-width (* display-scale 16))
(define display-height (* display-scale 9))

;; Work
(define staticBody (cpBodyNew +inf.0 +inf.0))

(define col:surface-type (gen-collision-type))
(define col:player-type (gen-collision-type))
(define col:exit-type (gen-collision-type))
(define col:player-body-type (gen-collision-type))

(define (make-static p1 p2)
  (define shape (cpSegmentShapeNew staticBody p1 p2 0.1))
  (set-cpShape-e! shape 1.0)
  (set-cpShape-u! shape 0.3)
  shape)

(define shape->cpShape
  (match-lambda
    [(struct rect (_ x y h w))
     (define shape (cpPolyShapeNew staticBody (rectangle-vector 0.0 0.0 h w) (cpv x y)))
     (set-cpShape-e! shape 1.0)
     (set-cpShape-u! shape 0.3)
     shape]     
    [(struct line (_ x1 y1 x2 y2))
     (make-static (cpv x1 y1) (cpv x2 y2))]))

; XXX Abstract and manage better
(require scheme/foreign)
(define (make-ptr-hash)
  (make-weak-custom-hash ptr-equal? equal-hash-code equal-secondary-hash-code))

(define current-exits (make-parameter (make-ptr-hash)))
(define (dict-set!* ht k v)
  (dict-set! ht k v)
  k)

(define (shapes->chipmunk-shapes s)
  (for/list ([sh (in-list s)])
    (define cp (shape->cpShape sh))
    (match (regexp-match #rx"^exit:(.*)$" (shape-i sh))
      [(list _ dir)
       (set-cpShape-collision_type! cp col:exit-type)
       (dict-set!* (current-exits) cp (string->symbol dir))]
      [_
       (set-cpShape-collision_type! cp col:surface-type)])
    cp))

(define linear-component->shapes
  (match-lambda
    [(struct linear-component (x y (struct component ('an-exit w h doors))))
     (define the-shape
       (or (match (hash-ref doors 'left #f)
             [#f #f]
             [(struct door (_p s e)) 
              (make-rect "exit:right" x y h w)
              #;(make-line "exit:right" x y x (+ y h))])
           (match (hash-ref doors 'right #f)
             [#f #f]
             [(struct door (_p s e)) 
              (make-rect "exit:left" x y h w)
              #;(make-line "exit:left" (+ x w) y (+ x w) (+ y h))])
           (match (hash-ref doors 'top #f)
             [#f #f]
             [(struct door (_p s e))
              (make-rect "exit:bottom" x y h w)
              #;(make-line "exit:bottom" x (+ y h) (+ x w) (+ y h))])
           (match (hash-ref doors 'bottom #f)
             [#f #f]
             [(struct door (_p s e)) 
              (make-rect "exit:top" x y h w)
              #;(make-line "exit:top" x y (+ x w) y)])))
     (list the-shape)]
    [(struct linear-component (x y (struct component (id w h doors))))
     (define left-wall 
       (match (hash-ref doors 'left #f)
         [#f
          (list 
           (make-line "wall" x y x (+ y h)))]
         [(struct door (_p s e))
          (list
           (make-line "wall" x y x (+ y s))
           (make-line "wall" x (+ y e) x (+ y h)))]))
     (define right-wall
       (match (hash-ref doors 'right #f)
         [#f
          (list
           (make-line "wall" (+ x w) y (+ x w) (+ y h)))]
         [(struct door (_p s e))
          (list 
           (make-line "wall" (+ x w) y (+ x w) (+ y s))
           (make-line "wall" (+ x w) (+ y e) (+ x w) (+ y h)))]))
     (define ceiling-wall 
       (match (hash-ref doors 'top #f)
         [#f
          (list 
           (make-line "wall" x (+ y h) (+ x w) (+ y h)))]
         [(struct door (_p s e))
          (list
           (make-line "wall" x (+ y h) (+ x s) (+ y h))
           (make-line "wall" (+ x e) (+ y h) (+ x w) (+ y h)))]))
     (define floor-wall 
       (match (hash-ref doors 'bottom #f)
         [#f
          (list 
           (make-line "wall" x y (+ x w) y))]
         [(struct door (_p s e))
          (list 
           (make-line "wall" x y (+ x s) y)
           (make-line "wall" (+ x e) y (+ x w) y))]))
     (define shapes
       (append left-wall right-wall ceiling-wall floor-wall))
     shapes]))

(define linear-room->shapes
  (match-lambda
    [(struct linear-room (_ _ lcs))
     (append-map linear-component->shapes lcs)]))

(define adjust-shape
  (match-lambda*
    [(list mh mw (struct rect (i x y h w)))
     (make-rect i
                (* x (/ jumper-width mw))
                (* (- mh y h) (/ jumper-height mh))
                (* h (/ jumper-height mh)) 
                (* w (/ jumper-width mw)))]))
(define adjust-shapes
  (match-lambda
    [(and cs (struct shapes (mh mw ss)))
     (make-shapes 
      jumper-height jumper-width
      (map (curry adjust-shape mh mw) ss))]))

(define (rectangle-vector x y h w)
  (if (w . < . 0)
      (vector (cpv (+ x w) (+ y h))
              (cpv x (+ y h))
              (cpv x y)
              (cpv (+ x w) y))
      (vector (cpv x y)
              (cpv x (+ y h))
              (cpv (+ x w) (+ y h))
              (cpv (+ x w) y))))

(define (jumper-shapes->cp-shapes jumper-body cs)
  (for/list ([s (in-list (shapes-ss cs))])
    (match s
      [(struct rect (i x y h w))
       (define verts (rectangle-vector x y h w))
       (define jumper-shape (cpPolyShapeNew jumper-body verts cpvzero))
       (set-cpShape-e! jumper-shape 0.0)
       (set-cpShape-u! jumper-shape normal-friction)
       (set-cpShape-collision_type! jumper-shape col:player-body-type)
       (when (regexp-match #rx"feet" i)
         (set-cpShape-collision_type! jumper-shape col:player-type))
       jumper-shape])))

(define (position-in-linear-room from-dir l)
  (match l
    [(struct linear-room (_x _y lcs))
     (if from-dir
         (match
             (filter (match-lambda
                       [(struct linear-component (x y (struct component ('an-exit w h doors))))
                        (hash-has-key? doors from-dir)]
                       [_
                        #f])
                     lcs)
           [(list (struct linear-component (x y (struct component ('an-exit w h doors)))))
            ; XXX This isn't quite right, but is okay for now
            (define center
              (cpv (+ x (/ w 2.0))
                   (+ y (/ h 2.0))))
            (define wid
              (current-exit-width))
            (case from-dir
              [(bottom) (cpvadd center (cpv 0.0 (* -2.0 wid)))]
              [(top) (cpvadd center (cpv 0.0 (* 2.0 wid)))]
              [(left) (cpvadd center (cpv (* -1.5 wid) 0.0))]
              [(right) (cpvadd center (cpv (* 1.5 wid) 0.0))])])
         (match l
           [(struct linear-room (_x _y (list-rest (struct linear-component (x y (struct component (_id w h _doors)))) _others)))
            (cpv (+ x (/ w 2.0))
                 (+ y (/ h 2.0)))]))]))

(define gl-draw-shape
  (match-lambda
    [(struct line (i x1 y1 x2 y2))
     (unless (regexp-match #rx"^exit:" i) ; XXX abstract test
       (gl-draw-line x1 y1 x2 y2))]
    [(struct rect (i x y h w))
     ;(unless (regexp-match #rx"^exit:" i) ; XXX abstract test
      (with-translate x y
        (gl-draw-rectangle 'solid w h))
      ;)
      ]))

(define player%
  (class* object% ()
    (init-field body)   
    
    ; XXX Better init for last-*
    ; XXX Or get rid of last and use my ring-buffer library
    
    (define last-facing 'right)
    (define/public (facing)
      (match (sgn (cpVect-x (cpBody-v body)))
        [0.0 last-facing]
        [-1.0 'left]
        [1.0 'right]))
    (define/public (old-facing) last-facing)
    
    (define last-orient 'down)
    (define/public (orient)
      (match (sgn (cpVect-y (cpBody-v body)))
        [0.0 'stable]
        [1.0 'up]
        [-1.0 'down]))
    (define/public (old-orient) last-orient)
    
    (define last-mirrored? #f)
    (define/public (mirrored?)
      (symbol=? 'left (facing)))
    (define/public (old-mirrored?) last-mirrored?)
    
    (define last-animation 'down)
    (define/public (animation)
      (case (orient)
        [(stable) 
         (if ((abs (cpVect-x (cpBody-v body))) . > . 0)
             'run
             'stand)]
        [(up) 'up]
        [(down) 'down]))
    (define/public (old-animation) last-animation)
    
    (define last-x #f)
    (define/public (old-x) last-x)
    (define last-y #f)
    (define/public (old-y) last-y)
    
    (define/public (step!)
      (set! last-x (cpVect-x (cpBody-p body)))
      (set! last-y (cpVect-y (cpBody-p body)))
      (set! last-mirrored? (mirrored?))
      (set! last-animation (animation))
      (set! last-orient (orient))
      (set! last-facing (facing)))
    
    (super-new)))

(define (gl-viewport/restrict/center mw mh
                                     vw vh 
                                     cx cy)
  (define x1 (- cx (/ vw 2)))
  (define x2 (+ cx (/ vw 2)))
  (define y1 (- cy (/ vh 2)))
  (define y2 (+ cy (/ vh 2)))
  
  ; Don't go off the screen
  (define x1p (max 0.0 x1))
  (define x2p (min mw x2)) 
  (define y1p (max 0.0 y1))
  (define y2p (min mh y2))
  
  (gluOrtho2D 
   (if (mw . <= . vw)
       (* -1 (/ (- vw mw) 2))
       ; If x2 has gone off, then add more to the left
       (if (= x2 x2p)
           x1p
           (+ x1p (- x2p x2))))
   ; etc
   (if (mw . <= . vw)
       (+ (/ (- vw mw) 2) mw)
       (if (= x1 x1p)
           x2p
           (+ x2p (- x1p x1))))
   (if (mh . <= . vh)
       0
       (if (= y2 y2p)
           y1p
           (+ y1p (- y2p y2))))
   (if (mh . <= . vh)
       vh
       (if (= y1 y1p)
           y2p
           (+ y2p (- y1p y1))))))

(define (render-room jumper-body world-width world-height shapes player i)
  (define x (cpVect-x (cpBody-p jumper-body)))
  (define y (cpVect-y (cpBody-p jumper-body)))
  (define how-many-jumpers 5) ; 5 is CSotN
  
  (gl-init display-width display-height)
  (gl-viewport/restrict/center
   world-width world-height
   (* jumper-width 1.78 how-many-jumpers) (* jumper-height 1 how-many-jumpers)
   x y)
  
  (gl-clear-color 1 1 1 1)
  (gl-clear 'color-buffer-bit)
  
  (gl-color 1 1 1 1)
  (for ([s (in-list shapes)])
    (gl-draw-shape s))
  
  (local [(define x (send player old-x))
          (define y (send player old-y))]
    (when (and x y)
      (with-translate x y
        (gl-color 1 1 1 0.5)
        (render-sprite 
         #:mirror? (send player old-mirrored?) 
         samus-sprites (send player old-animation) (sub1 i) 4 jumper-width jumper-height))))
  
  (with-translate x y
    (gl-color 1 1 1 1)
    (render-sprite 
     #:mirror? (send player mirrored?) 
     samus-sprites (send player animation) i 4 jumper-width jumper-height)
    
    ; Outline collision shapes
    #;(for ([s (in-list (shapes-ss current-shapes))])
        (match s
          [(struct rect (i x y h w))
           (with-translate x y
             (gl-draw-rectangle 'outline w h))])))
  
  #;(local [(define how-long (- (current-seconds) start-time))]
      (unless (zero? how-long)
        (printf "FPS: ~S~n" (round (/ i how-long))))))

(define (jumper-tick left? right? up? can-jump? jumping? player jumper-body current-shapes cp-shapes space exitted? i)
  (define force
    (cpvadd
     (if (unbox left?) 
         (cpv (* -1 move-strength) 0.0)
         cpvzero)
     (if (unbox right?)
         (cpv move-strength 0.0)
         cpvzero)
     (if (and (unbox up?) (unbox can-jump?))
         (cpv 0.0 jump-strength)
         cpvzero)))
  (define force/limited
    (if ((abs (cpVect-x (cpBody-v jumper-body))) . >= . x-speed-limit)
        (cpv 0.0 (cpVect-y force))
        force))
  (when (zero? (modulo i 5))
    (cpBodyApplyImpulse jumper-body force cpvzero))
  
  (local [(define new-shapes
            (adjust-shapes
             (animation-shapes 
              #:mirror? (send player mirrored?) 
              samus-sprites (send player animation) (add1 i) 4)))]      
    (unless (equal? new-shapes (unbox current-shapes))
      (for-each (curry cpSpaceRemoveShape space) (unbox cp-shapes))
      (for-each cpShapeFree (unbox cp-shapes))
      (set-box! current-shapes new-shapes)
      (set-box! cp-shapes (jumper-shapes->cp-shapes jumper-body (unbox current-shapes)))
      (for-each (curry cpSpaceAddShape space) (unbox cp-shapes))))
  
  (send player step!)
  
  ; XXX Very hacky
  (set-box! can-jump? #f)
  ; If we were jumping before this
  ; XXX
  #;(if (jumping? . < . i)
        (set-cpShape-u! jumper-shape 0.0)
        (set-cpShape-u! jumper-shape normal-friction))
  (for ([i (in-range substeps)])
    (unless (unbox exitted?)
      (cpSpaceStep space (/ dt substeps))))
  
  (add1 i))

(define (jumper-on-key up? jumping? left? right? i k)
  (match (send k get-key-code)
    [(or #\x 'up)
     (set-box! up? #t)
     (set-box! jumping? i)]
    ['left (set-box! left? #t)]
    ['right (set-box! right? #t)]
    ['release
     (match (send k get-key-release-code)
       [(or #\x 'up)
        (set-box! up? #f)]
       ['left (set-box! left? #f)]
       ['right (set-box! right? #f)]
       [else (void)])]
    [else (void)])
  
  i)

(define (room->space jumping? can-jump? exitted? jumper-body cp-shapes from-dir a-room)
  (define exits (make-ptr-hash))
  (define world-width (linear-room-w a-room))
  (define world-height (linear-room-h a-room))     
  (define shapes
    (linear-room->shapes a-room))
  (define segments
    (parameterize ([current-exits exits])
      (shapes->chipmunk-shapes shapes)))
  (define space (cpSpaceNew))
  
  (printf "Space setup~n")
  (set-cpSpace-iterations! space 20)
  (set-cpSpace-elasticIterations! space 20)
  (cpSpaceResizeStaticHash space 40.0 1000)
  (cpSpaceResizeActiveHash space 40.0 1000)
  (set-cpSpace-gravity! space (cpv 0.0 -9.8))
  ;; Adding the room segments
  (for-each (curry cpSpaceAddStaticShape space) segments)
  
  (printf "Adding player~n")
  (set-cpBody-p! jumper-body (position-in-linear-room from-dir a-room))
  (cpSpaceAddBody space jumper-body)
  
  (for-each (curry cpSpaceAddShape space) (unbox cp-shapes))
  
  ; XXX Very hacky
  (cpSpaceAddCollisionPairFunc
   space col:player-type col:surface-type
   (lambda (player-shape surface-shape contacts normal_coef)
     (define coming-from-top?
       (for/or ([c (in-vector contacts)]
                [i (in-naturals)])
         ; Magic: Only allow a jump if coming sufficiently vertically
         (> (cpVect-y (cpContact-n c)) 0.5)))
     (when coming-from-top?
       (set-box! jumping? +inf.0))
     (set-box! can-jump? coming-from-top?)
     #t))
  
  ;;; XXX These should both be cleaner and wait until the player has gone all the way in
  (local [(define (exit-collider player-shape exit-shape contacts normal_coef)
            (define dir 
              (or (dict-ref exits player-shape #f)
                  (dict-ref exits exit-shape #f)))
            (when dir
              (set-box! exitted? dir))
            (symbol? dir))]  
    (cpSpaceAddCollisionPairFunc space col:player-type col:exit-type exit-collider)  
    (cpSpaceAddCollisionPairFunc space col:player-body-type col:exit-type exit-collider))
  
  (values world-width world-height shapes segments space))

;;; The work
; Infinite inertia makes it impossible to rotate
(define jumper-body (cpBodyNew jumper-mass +inf.0))
; XXX Hacky stuff
(define jumping? (box +inf.0))
(define can-jump? (box #f))
(define left? (box #f))
(define right? (box #f))
(define up? (box #f))       
(define exitted? (box #f))

(define start-time (current-seconds))

(define player (new player% [body jumper-body]))

; XXX Hack!
(define initial-shapes
  (make-shapes jumper-height jumper-width (list (make-rect "feet" 0.0 0.0 jumper-height jumper-width))))
(define current-shapes (box initial-shapes))
(define cp-shapes (box (jumper-shapes->cp-shapes jumper-body (unbox current-shapes))))

(define-struct room-rep (world-width world-height shapes segments space exitted?))
(define-struct world (i p room))

(define (p->room-rep p)
  (define exitted? (box #f))
  (define-values (world-width world-height shapes segments space)
    (room->space jumping? can-jump? exitted? jumper-body cp-shapes #f (matrix-ref/posn game p)))
  (make-room-rep world-width world-height shapes segments space exitted?))

(big-bang 
 world?
 (make-world 0 start-posn (p->room-rep start-posn))
 #:height display-height
 #:width display-width
 #:on-tick 
 (match-lambda
   [(struct world (i p (and rr (struct room-rep (world-width world-height shapes segments space exitted?)))))
    (define next-i (jumper-tick left? right? up? can-jump? jumping? player jumper-body current-shapes cp-shapes space exitted? i))
    ; Time to switch rooms
    (if (unbox exitted?)
        (local [(define the-room
                  (matrix-ref/posn game p))
                (define the-dir (unbox exitted?))
                (define next-p
                  (case the-dir
                    [(left) (posn-left p)]
                    [(right) (posn-right p)]
                    [(top) (posn-up p)]
                    [(bottom) (posn-down p)]))
                (define new-room-rep (p->room-rep next-p))]
          (printf "Exitted in ~S~n" the-dir)
          (for-each cpShapeFree segments)
          (cpSpaceFree space)
          (make-world next-i next-p new-room-rep))
        (make-world next-i p rr))])
 #:tick-rate rate
 #:on-key 
 (match-lambda*
   [(list (struct world (i p rr)) k)
    (define next-i (jumper-on-key up? jumping? left? right? i k))
    (make-world next-i p rr)])
 #:draw-init
 (lambda ()
   (printf "Loading sprites~n")
   (load-sprites! samus-sprites))
 #:on-draw 
 (match-lambda
   [(struct world (i p (struct room-rep (world-width world-height shapes segments space exitted?))))
    (render-room jumper-body world-width world-height shapes player i)])
 #:stop-when (lambda _ #f)
 #:stop-timer (lambda _ #f))

; XXX Free the cp shapes for the jumper also

(printf "Done forever~n")
(cpBodyFree staticBody)
