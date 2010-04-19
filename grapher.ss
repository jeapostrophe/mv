#lang scheme
(require 2htdp/universe
         "graph-gen.ss"
         "../rl/lib/posn.ss"
         "../rl/lib/matrix-posn.ss")

(define rows 35)
(define cols 35)

(define-values (start-posn is-hidden? on-path? target-posn? the-map)
  (generate-map rows cols))

(define scale 14) ; 22 at work ; 14 at home
(define radius (/ scale 4))

(define (list-modulo l i)
  (list-ref l (modulo i (length l))))

(define colors
  '(Chocolate Fuchsia BlueViolet Khaki
              Aqua Olive DodgerBlue MistyRose
              Yellow Aquamarine Silver Orchid
              Lime Tomato Plum Gold
              IndianRed SpringGreen Purple Teal))
(define (path-circ t)
  (circle radius 'solid 
          (if t
              (list-modulo colors t)
              'black)))
(define hidden-circ
  (circle radius 'solid 'red))

(define (vline t)
  (put-pinhole
   (rectangle radius (add1 radius) 'solid
              (if (number? t)
                  (list-modulo colors t)
                  'black))
   (/ radius 2) 0))
(define (hline t)
  (put-pinhole
   (rectangle (add1 radius) radius 'solid
              (if (number? t)
                  (list-modulo colors t)
                  'black))
   0 (/ radius 2)))

(define (draw-edges ri ci
                    n s)
  (match n
    [(struct node (t u l r d))
     (let*
         ([s
           ; Top
           (if u
               (place-image (vline u)
                            (* (add1 ci) scale)
                            (- (* (add1 ri) scale) (* 2 radius))
                            s)
               s)]
          [s
           ; Left
           (if l
               (place-image (hline l)
                            (- (* (add1 ci) scale) (* 2 radius))
                            (* (add1 ri) scale)
                            s)
               s)]
          [s
           ; Right
           (if r
               (place-image (hline r)
                            (+ (* (add1 ci) scale) radius)
                            (* (add1 ri) scale)
                            s)
               s)]
          [s
           ; Bottom
           (if d
               (place-image (vline d)
                            (* (add1 ci) scale)
                            (+ (* (add1 ri) scale) radius)
                            s)
               s)])
       s)]))

(define (center-pinhole img)
  (put-pinhole 
   img
   (/ (image-width img) 2)
   (/ (image-height img) 2)))

; XXX Compute the count better
(define count 0)
(define (draw-graph the-map)
  (set! count 0)
  (for*/fold ([s (empty-scene (* scale (add1 cols)) (* scale (add1 rows)))])
    ([ri (in-range rows)]
     [ci (in-range cols)])
    (match (matrix-ref the-map ri ci)
      [(and n (struct node (t _ _ _ _)))
       (define p (make-posn ri ci))
       (define (add-img img)
         (set! count (add1 count))
         (place-image 
          img
          (* (add1 ci) scale)
          (* (add1 ri) scale)
          (draw-edges ri ci n s)))
       (cond
         [(target-posn? p) 
          => (lambda (i) 
               (add-img (center-pinhole (text (number->string i) scale 'blue))))]
         [(is-hidden? p)
          (add-img hidden-circ)]
         [(on-path? p)
          (add-img (path-circ t))]
         [else
          s])])))

(draw-graph the-map)

(printf "Count: ~a~n" count)