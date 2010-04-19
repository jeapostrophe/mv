#lang scheme
(require "../rl/lib/random.ss"
         (planet murphy/amb/amb)
         (planet cobbe/contract-utils:3:0/contract-utils)
         (planet wmfarr/permutations/permutations))

(define (amb-list-ref l)
  (if (= 1 (length l))
      (first l)
      (for/amb ([i (in-list (permute-list l))])
               i)))

(define (all-permutations l)
  (define ll (length l))
  (if (ll . <= . 1)
      (list l)
      (for/list ([p (in-permutations ll)])
        (for/list ([i (in-range ll)])
          (list-ref l (vector-ref p i))))))

(define (amb-permutation l)
  (amb-list-ref (all-permutations l)))

(define (amb-if)
  (amb-list-ref (list #t #f)))

(define (random-list-ref l lo hi)
    (define len (length l))
    (list-ref l (random-in (inexact->exact (floor (* lo len)))
                           (inexact->exact (floor (* hi len))))))

(provide/contract
 [amb-list-ref ((nelistof/c any/c) . -> . any/c)]
 [all-permutations ((listof any/c) . -> . (listof (listof any/c)))]
 [amb-permutation ((listof any/c) . -> . (listof any/c))]
 [amb-if (-> boolean?)]
 [random-list-ref ((listof any/c) number? number? . -> . any/c)])