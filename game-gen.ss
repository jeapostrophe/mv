#lang scheme
(require 2htdp/universe
         "graph-gen.ss"
         "room.ss"         
         "room-gen.ss"
         "../rl/lib/posn.ss"
         "../rl/lib/matrix-posn.ss")

(define rows 35)
(define cols 35)

(define (current-memory-use-mb)
  (exact->inexact (/ (/ (current-memory-use) 1024) 1024)))

(printf "Before ~S~n" (current-memory-use-mb))

(define-values (start-posn is-hidden? on-path? target-posn? the-map)
  (generate-map rows cols))

(printf "After map gen ~S~n" (current-memory-use-mb))

(define game
  (build-matrix 
   rows cols 
   (lambda (ri ci)
     (define p (make-posn ri ci))
     (match (matrix-ref the-map ri ci)
       [(and n (struct node (t u l r d)))
        ; XXX Use t to select components / tileset
        ; XXX Use u ... d to select powerup reqs
        (define input
          (append (if u (list (make-room-req 'top no-power-up)) empty)
                  (if l (list (make-room-req 'left no-power-up)) empty)
                  (if r (list (make-room-req 'right no-power-up)) empty)
                  (if d (list (make-room-req 'bottom no-power-up)) empty)))
        (if (empty? input)
            #f
            (construct-room input))
        #;(if (or (target-posn? p) 
                  (is-hidden? p)
                  (on-path? p))
              (construct-room input)
              #f)]))))

(printf "After room gen ~S~n" (current-memory-use-mb))

(provide game
         start-posn)