#lang scheme
(require 2htdp/universe
         "room.ss"
         "room-gen.ss")

(define the-scale 20)

(define render-component  
  (match-lambda
    [(struct component ('an-exit w h _))
     (define sw (* the-scale w))
     (define sh (* the-scale h))
     (put-pinhole
       (rectangle sw sh 'solid 'green)
      0 sh)]
    [(struct component (id w h cs))
     (define sw (* the-scale w))
     (define sh (* the-scale h))
     (define main
       (put-pinhole 
        (overlay
         (text (symbol->string id) 12 'black)
         (put-pinhole
          (rectangle sw sh 'outline 'black)
          (/ the-scale 4) (/ sh 2)))
        0 0))
     (put-pinhole
     (for/fold ([img main])
       ([(d c) (in-hash cs)])
       (match c
         [(struct door (p s e))
          (define ss (* the-scale s))
          (define se (* the-scale e))
          (case d
            [(left)
             (add-line img 0 (- sh ss) 0 (- sh se) 'red)]
            [(right)
             (add-line img sw (- sh ss) sw (- sh se) 'red)]
            [(top)
             (add-line img ss 0 se 0 'red)]
            [(bottom)
             (add-line img ss sh se sh 'red)])]))
     0 sh)]))

(define offset 10)
(define width 800)
(define height 600)
(define render-linear-room
  (match-lambda
    [(struct linear-room (w h lcs))
     (for/fold ([s (empty-scene width height)])
       ([lc (in-list lcs)])
       (match lc
         [(struct linear-component (x y c))
          (place-image (render-component c)
                       (+ offset (* the-scale x))
                       (- height (+ offset (* the-scale y)))
                       s)]))]))

(define rooms
  (for*/list ([l (in-list (list #f 'left))]
              [r (in-list (list #f 'right))]
              [t (in-list (list #f 'top))]
              [b (in-list (list #f 'bottom))])
    (define input
      (append (if l (list (make-room-req l no-power-up)) empty)
              (if r (list (make-room-req r no-power-up)) empty)
              (if t (list (make-room-req t no-power-up)) empty)
              (if b (list (make-room-req b no-power-up)) empty)))
    (if ((length input) . <= . 1)
        #f
        input)))

(define real-rooms
  (filter (lambda (x) x) rooms))

(big-bang 0
          (on-draw
           (lambda (i)
             (define input (list-ref real-rooms i))
             (printf "Rendering... ~S~n" input)
             (render-linear-room (construct-room input))))
          (on-key
           (lambda (i k)
             (define ni (add1 i))
             (if (key=? k "release")
                 (if (ni . >= . (length real-rooms))
                     0
                     ni)
                 i))))