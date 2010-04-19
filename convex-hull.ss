#lang scheme
(require (planet "control.scm" ("soegaard" "control.plt" 2 0)))

(define point-x car)
(define point-y cdr)

(define S empty)
(define (push! i)
  (set! S (cons i S)))
(define (pop!)
  (set! S (rest S)))

(define (ccw p1 p2 p3)
  (- (* (- (point-x p2) (point-x p1))
        (- (point-y p3) (point-y p1)))
     
     (* (- (point-y p2) (point-y p1))
        (- (point-x p3) (point-x p1)))))

(require scheme/runtime-path
         "gl2d.ss"
         sgl/gl-vectors)
(define-runtime-path texture-path "Test.bmp")
(define-values (w h vec) (image->gl-vector texture-path))

(define (p x y j)
  (define i (+ (* y w) x))
  (gl-vector-ref vec (+ (* i 3) j)))

(define ops empty)
(for* ([x (in-range w)]
       [y (in-range h)])
  (define d (p x y 0))
  (define g (p x y 1))
  (define b (p x y 2))
  (printf "~S = ~S~n"
          (cons x y)
          (list d g b))
  (unless (and (>= d 255) (>= g 255) (>= b 255))
    (set! ops (list* (cons x y) ops))))

(define fps
  (sort
   ops
   (lambda (p1 p2)
     (if (= (point-y p1) (point-y p2))
         (<= (point-x p1) (point-x p2))
         (< (point-y p1) (point-y p2))))))

(define p0 (first fps))
(define ps
  (list*
   p0
   (sort
    (rest fps)
    <=
    #:key 
    (lambda (p)
      (angle (- (make-rectangular (point-x p) (point-y p))
                (make-rectangular (point-x p0) (point-y p0))))))))

(push! (list-ref ps 0))
(push! (list-ref ps 1))
(push! (list-ref ps 2))
(for ([i (in-range 3 (length ps))])
  (define c (list-ref ps i))
  (printf "~S ------- ~S~n" S c)
  (while ((ccw (second S) (first S) c) . < .  0)
         (pop!)
         (printf "Pop ~S~n" S))
  (push! c))

(for ([c (in-list S)])
  (define x (point-x c))
  (define y (point-y c))
  (printf "~S = ~S~n"
          c
          (list (p x y 0)
                (p x y 1)
                (p x y 2))))
