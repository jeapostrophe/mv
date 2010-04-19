#lang scheme
(require "../rl/lib/random.ss"
         (planet dyoo/tqueue:1/tqueue))

(define (tqueue->sorted a-tqueue #:satisfy? [satisfy? #t])
  (let loop ()
    (cond
      [(tqueue-try-get a-tqueue)
       => (lambda (next-elt)
            (when satisfy?
              (tqueue-satisfy! a-tqueue next-elt))
            (list* next-elt (loop)))]
      [else
       empty])))

(define (initialize-tqueue deps)
  (define a-tq (new-tqueue))
  (for ([(k v) (in-hash deps)])
    (tqueue-add! a-tq k v))
  a-tq)

(define (topographical-sort deps)
  (define a-tq (initialize-tqueue deps))
  (tqueue->sorted a-tq))

(define (tqueue-all-ready a-tq)
  (tqueue->sorted a-tq #:satisfy? #f))   

(define (random-topographical-sort deps)
  (define a-tq (initialize-tqueue deps))
  (let loop ()
    (match (tqueue-all-ready a-tq)
      [(list)
       empty]
      [ready
       (for-each (curry tqueue-satisfy! a-tq) ready)
       (append (permute-list ready)
               (loop))])))

(define dependencies/c
  (hash/c symbol? (listof symbol?)))
   
(provide/contract
 [dependencies/c contract?]
 [topographical-sort (dependencies/c . -> . (listof symbol?))]
 [random-topographical-sort (dependencies/c . -> . (listof symbol?))])