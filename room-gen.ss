#lang scheme
(require "amb.ss"
         "room.ss"
         "components.ss"
         (planet murphy/amb/amb)
         #;(except-in (planet murphy/amb/amb) amb-assert))

(define current-exit-width (make-parameter 1.0))
(define current-max-path-length (make-parameter 4))

#;(define-syntax-rule (amb-assert e)
    (unless e
      (printf "\tBacktracking at ~S~n" 'e)
      (amb-fail)))
(define (amb-fail* . fmt)
  #;(printf "\t~a" (apply format fmt))
  (amb-fail))

(with-contract room-struct
  ([connection/c contract?]
   room room-com room-l room-r room-t room-b room?
   [make-room (component? connection/c connection/c connection/c connection/c . -> . room?)])
  (define-struct room (com l r t b) #:prefab)
  (define connection/c
    (or/c 'prev boolean? room?)))

(define (construct-room l)
  (amb-find    
   (let* ([room (fixup-room (generate-room (current-max-path-length) l))]
          [lr (linearize-room room)])
     (amb-assert (not (overlapping? lr)))
     lr)))

(define (hash-keys ht)
  (for/list ([k (in-hash-keys ht)]) k))

(define (debug id v)
  (printf "\t~S ~S~n" id v)
  v)

; Associates each elements of l2 with a random selection of elements from l1 such that all of l1 are associated and one of each of l2 are associated
(define (random-partition exits doors)
  (define dirs (hash-keys doors))
  (define how-many (hash-count doors))
  (define randomized-exits 
    (amb-permutation exits))
  (define randomized-dirs
    (amb-permutation dirs))
  (define initial-ht
    (for/hasheq ([e (in-list randomized-dirs)]
                 [i (in-naturals)])
      (values e (list (list-ref randomized-exits i)))))
  (for/fold ([ht initial-ht])
    ([e (in-list (list-tail randomized-exits how-many))])
    (define ep (room-req-power-up e))
    (define possible-dirs 
      (filter (lambda (dir)
                (define d (hash-ref doors dir))
                (define p (door-power-up d))
                ; We can't use this component for this door if it is too hard for this exit
                (or (power-up= p no-power-up)
                    (power-up= p ep)))
              randomized-dirs))
    (amb-assert (not (empty? possible-dirs)))
    (hash-update ht (amb-list-ref possible-dirs) (curry list* e))))

; generate-room : non-empty-list of directions -> room
; XXX Still needs to be allowed to put in dead ends
(define (generate-room n l)
  (amb-assert (n . > . 0))
  (match l
    [(list-rest (struct room-req (f fp)) rcs)
     (define how-many-doors (length l))
     (define possible
       (filter (lambda (rc)
                 (define ds (component-doors rc))
                 (define how-many-exits (hash-count ds))
                 (and 
                  ; There are not too many exits
                  (how-many-doors . >= . how-many-exits)
                  ; There are enough exits
                  ((sub1 how-many-doors) . <= . (sub1 how-many-exits))
                  ; Now that basic conditions are fulfilled, do a hash lookup
                  (local [(define fdoor (hash-ref ds f #f))]
                    (and
                     ; We can come from where we were
                     fdoor
                     (local [(define dp (door-power-up fdoor))]
                       ; The exit has the right difficulty
                       (power-up= fp dp))))))
               components))
     (amb-assert (not (empty? possible)))
     (local [; Randomly picks one of the possible components
             (define this-part (amb-list-ref possible))
             (define ds (component-doors this-part))
             (define paths (random-partition rcs (hash-remove ds f)))
             (define (continue-path dir)
               (if (symbol=? dir f)
                   'prev
                   (match (hash-ref paths dir #f)
                     [#f
                      #f]
                     [(? list? dests)
                      (define this-power (door-power-up (hash-ref ds dir)))
                      (match 
                          ; Adjust all the destinations so that if this edge is hard enough, then they are satisfied
                          (map (match-lambda
                                 [(struct room-req (d dp))
                                  ; Return the new room req
                                  (make-room-req d 
                                                 (cond
                                                   ; If the power-up is the right amount, then we don't need the power-up constraint anymore
                                                   [(power-up= dp this-power)
                                                    no-power-up]
                                                   ; If there is no power-up req, then we keep the old
                                                   [(power-up= this-power no-power-up)
                                                    dp]
                                                   ; Otherwise it is too hard and we fail
                                                   [else
                                                    (amb-fail* "~S should be ~S or no-power-up~n" this-power dp)]))])
                               dests)
                        ; If there is only one thing remaing and this is the right direction exit
                        [(list (and this-req (struct room-req ((? (curry equal? dir)) dp))))
                         (if (amb-if)
                             ; If this is the final exit, then it must be hard enough to get too
                             (begin (amb-assert (power-up= dp this-power))
                                    #t)
                             ; But it doesn't matter if this isn't the final one
                             (generate-room 
                              (sub1 n) 
                              (list (make-room-req (reverse-direction dir) no-power-up)
                                    this-req)))]
                        ; If there are many things left
                        [new-dests
                         (generate-room 
                          (sub1 n)
                          (list* (make-room-req (reverse-direction dir) no-power-up)
                                 new-dests))])])))]
       (make-room this-part
                  (continue-path 'left)
                  (continue-path 'right)
                  (continue-path 'top)
                  (continue-path 'bottom)))]))

(define (prev->exit v)
  (if (eq? v 'prev) #t v))

(define fixup-room
  (match-lambda
    [(struct room (c l r t b))
     (make-room  c (prev->exit l) (prev->exit r) (prev->exit t) (prev->exit b))]))

(define (adjust-coordinates dx dy cs)
  (map (match-lambda
         [(struct linear-component (x y c))
          (make-linear-component (+ dx x) (+ dy y) c)])
       cs))
(define room-shallow-h
  (match-lambda
    [(struct linear-room (_ _ (list-rest (struct linear-component (_ _ (struct component (_ w h _)))) _)))
     h]
    [_
     0.0]))
(define room-shallow-w
  (match-lambda
    [(struct linear-room (_ _ (list-rest (struct linear-component (_ _ (struct component (_ w h _)))) _)))
     w]
    [_
     0.0]))
(define room-shallow-x
  (match-lambda
    [(struct linear-room (_ _ (list-rest (struct linear-component (x y (struct component (_ w h _)))) _)))
     x]
    [_
     0.0]))
(define room-shallow-y
  (match-lambda
    [(struct linear-room (_ _ (list-rest (struct linear-component (x y (struct component (_ w h _)))) _)))
     y]
    [_
     0.0]))

(define (align-components x y cs)
  (match cs
    [(list)
     empty]
    [(list-rest (struct linear-component (cx cy c)) rst)
     (adjust-coordinates (- x cx) (- y cy) cs)]))

(define (components-width*height cs)
  (for/fold ([width 0.0]
             [height 0.0])
    ([lc (in-list cs)])
    (match lc
      [(struct linear-component (x y (struct component (id w h cs))))
       (values (max (+ x w) width)
               (max (+ y h) height))])))

(define (normalize-components cs)
  (define mx (apply min (map linear-component-x cs)))
  (define my (apply min (map linear-component-y cs)))
  (adjust-coordinates
   (* -1 mx) (* -1 my)
   cs))  

(define (find-door dir cs)
  (hash-ref (component-doors cs) dir #f))

(define (door-start dir cs)
  (define c (find-door dir cs))
  (if c
      (door-s c)
      0.0))
(define (door-end dir cs)
  (define c (find-door dir cs))
  (if c
      (door-e c)
      0.0))

(define (door-alignment dir c1 r)
  (match r
    [(struct linear-room (_ _ (list-rest (struct linear-component (_ _ c2)) _)))
     (match (find-door dir c1)
       [(struct door (_ s1 e1))
        (define w1 (- e1 s1))
        (match (find-door (reverse-direction dir) c2)
          [(struct door (_ s2 e2))
           (define w2 (- e2 s2))
           ; Fail if the doors are not the same size
           (amb-assert (= w1 w2))
           (- s1 s2)])])]
    [_ 0]))

(define null-linear-room
  (make-linear-room 0.0 0.0 empty))

(define (linearize-room r [dir #f] [lc #f])
  (match r
    [(struct room ((and c (struct component (id w h cs))) l r t b))
     ; If left or top are exits, than we need to have the appropriate offset so we don't throw everything else off
     (define ll (linearize-room l 'left c))
     (define lr (linearize-room r 'right c))
     (define lt (linearize-room t 'top c))
     (define lb (linearize-room b 'bottom c))
     (define lx (room-shallow-x ll))
     (define by (room-shallow-y lb))
     (define cx (+ lx (room-shallow-w ll)))
     (define cy (+ by (room-shallow-h lb)))     
     (define first-pass-components
       (append 
        ; Render this in the middle
        (list (make-linear-component cx cy c))
        ; Align the left with the y component
        (align-components
         lx (+ cy (door-alignment 'left c ll))
         (linear-room-lcs ll))
        ; Align the right with the x+w and y components
        (align-components
         (+ cx w) (+ cy (door-alignment 'right c lr))
         (linear-room-lcs lr))
        ; Align the bottom with the x component
        (align-components
         (+ cx (door-alignment 'bottom c lb)) by
         (linear-room-lcs lb))
        ; Align the top with the x and y+h components
        (align-components
         (+ cx (door-alignment 'top c lt)) (+ cy h)
         (linear-room-lcs lt))))
     (define components
       (normalize-components
        first-pass-components))
     (define-values (cw ch) (components-width*height components))
     (make-linear-room cw ch components)]
    [#t
     (define e (door-end dir lc))
     (define s (door-start dir lc))
     (define door-width (- e s))
     (case dir
       [(left right)
        (make-linear-room
         (current-exit-width) door-width
         (list 
          (make-linear-component 
           0.0 0.0
           (make-component
            'an-exit (current-exit-width) door-width
            (make-immutable-hasheq
             (list (cons (reverse-direction dir)
                         (make-door no-power-up 0.0 door-width))))))))]
       [(top bottom)
        (make-linear-room
         door-width (current-exit-width)
         (list 
          (make-linear-component 
           0.0 0.0
           (make-component
            'an-exit door-width (current-exit-width)
            (make-immutable-hasheq
             (list (cons (reverse-direction dir)
                         (make-door no-power-up 0.0 door-width))))))))])]
    
    [(or #f 'prev)
     null-linear-room]))

(define (rectangle-overlap? x1 y1 w1 h1
                            x2 y2 w2 h2)
  (not 
   (or (<= (+ x1 w1) x2)
       (<= (+ x2 w2) x1)
       (<= (+ y1 h1) y2)
       (<= (+ y2 h2) y1))))

(define overlaps?
  (match-lambda*
    [(list (struct linear-component (x1 y1 (struct component (_ w1 h1 _))))
           (struct linear-component (x2 y2 (struct component (_ w2 h2 _)))))
     (rectangle-overlap? x1 y1 w1 h1
                         x2 y2 w2 h2)]))

(define (any-overlaps? lc lcs)
  (ormap (curry overlaps? lc) lcs))

(define overlapping-components?
  (match-lambda
    [(list) #f]
    [(list-rest f rs)
     (or (any-overlaps? f rs)
         (overlapping-components? rs))]))

(define overlapping?
  (match-lambda
    [(struct linear-room (w h lcs))
     (overlapping-components? lcs)]))

(define-struct room-req (direction power-up) #:prefab)

(provide/contract
 [current-exit-width (parameter/c inexact-real?)]
 [current-max-path-length (parameter/c exact-nonnegative-integer?)]
 [struct room-req 
         ([direction direction/c]
          [power-up power-up/c])]
 [construct-room ((listof room-req?) . -> . linear-room?)])