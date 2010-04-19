#lang scheme

(define-struct door (power-up s e) #:prefab)
(define-struct component (id w h doors) #:prefab)

(define direction/c
  (symbols 'left 'right 'top 'bottom))

(define reverse-direction
  (match-lambda
    ['left 'right]
    ['right 'left]
    ['top 'bottom]
    ['bottom 'top]))

(define (component-directions c)
  (for/list ([k (in-hash-keys (component-doors c))]) k))

(define-struct linear-component (x y c) #:prefab)
(define-struct linear-room (w h lcs) #:prefab)

(define power-up/c symbol?)
(define no-power-up 'no-power-up)
(define power-up= symbol=?)

(provide/contract
 [power-up/c contract?]
 [no-power-up power-up/c] 
 [power-up= (power-up/c power-up/c . -> . boolean?)]
 [struct door 
         ([power-up power-up/c]
          [s inexact-real?]
          [e inexact-real?])]
 [struct component 
         ([id symbol?]
          [w inexact-real?]
          [h inexact-real?]
          [doors (hash/c direction/c door? #:immutable #t)])]
 [direction/c contract?]
 [reverse-direction (direction/c . -> . direction/c)]
 [component-directions (component? . -> . (listof direction/c))]
 [struct linear-component
         ([x inexact-real?]
          [y inexact-real?]
          [c component?])]
 [struct linear-room
         ([w inexact-real?]
          [h inexact-real?]
          [lcs (listof linear-component?)])])
