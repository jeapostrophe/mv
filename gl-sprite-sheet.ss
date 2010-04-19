#lang scheme
(require (planet jaymccarthy/gl2d))

(define-struct sprite-sheet (rs cs h w hr wr ho wo))
(define (make-gl-sprite-sheet rs cs h w)
  (make-sprite-sheet rs cs
                     h w 
                     (/ 1 rs) (/ 1 cs)
                     (/ 1 (* 2 (* rs h)))
                     (/ 1 (* 2 (* cs w)))))
(define (gl-draw-sprite ss r c w h)
  (match ss
    [(struct sprite-sheet (rs cs _ _ hr wr ho wo))
     (gl-draw-rectangle/texture-part 
      (+ (/ c cs) wo) 
      (+ (/ r rs) ho)
      (- wr wo)
      (- hr ho)
      w h)]))
(define (gl-draw-sprite/mirror ss r c w h)
  (with-mirror w
    (gl-draw-sprite ss r c w h)))

(provide/contract
 [sprite-sheet? (any/c . -> . boolean?)]
 [make-gl-sprite-sheet (exact-nonnegative-integer? exact-nonnegative-integer? real? real? . -> . sprite-sheet?)]
 [gl-draw-sprite (sprite-sheet? exact-nonnegative-integer? exact-nonnegative-integer? real? real? . -> . void)]
 [gl-draw-sprite/mirror (sprite-sheet? exact-nonnegative-integer? exact-nonnegative-integer? real? real? . -> . void)])
