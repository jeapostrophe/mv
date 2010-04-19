#lang scheme
(require "amb.ss"
         "../rl/lib/posn.ss"
         "../rl/lib/random.ss"
         "../rl/lib/matrix-posn.ss"
         (planet jaymccarthy/batched-deque))

; XXX Expose the magic in an api

; XXX I should be able to push power ups away from each, not just the prev

;; Magic
(define how-many-powerups 20)
(define how-many-hidden (* 3 how-many-powerups))

(define (powerup-interval i)
  (cond
    [(zero? i)
     (list 0.05 0.10)]
    [(i . <= . (* 0.5 how-many-powerups))
     (list 0.15 0.25)]
    [(i . <= . (* 0.75 how-many-powerups))
     (list 0.30 0.60)]
    [else
     (list 0.50 0.80)]))

;; Helpers
(define-struct node (t u l r d) #:prefab)

(define (interval-in? v i)
  (<= (first i) v (second i)))

(define (posn-neighbors/m m p)
  (match (matrix-ref/posn m p)
    [(struct node (t u l r d))
     (append (if u (list (posn-up p)) empty)
             (if l (list (posn-left p)) empty)
             (if r (list (posn-right p)) empty)
             (if d (list (posn-down p)) empty))]))

(define (greedy-dfs-path m start end max-length bad?)
  (define visited (make-hash))
  (define (search p i)
    (cond      
      ; We don't want to loop, but we don't want to disallow coming
      ; from a shorter pre-path, so this always us through if the
      ; previous path was longer; which is never the case with a
      ; cycle
      [(i . <= . (hash-ref visited p -inf.0))
       #f]
      ; If we're at the end, stop
      [(equal? p end)
       (list p)]
      ; If we're out of juice, fail
      [(i . <= . 0)
       #f]
      [(bad? p)
       #f]
      [else
       ; Remember how long it took to get here
       (hash-set! visited p i)
       ; And continue...
       (let ([inner
              (for/or ([n (in-list (permute-list (posn-neighbors/m m p)))])
                ; Remember to take away some juice
                (search n (sub1 i)))])
         (if inner
             (list* p inner)
             #f))]))
  (or (search start max-length)
      (error 'greedy-dfs-path "Cannot find path!")))

(define (bfs-search m r lim f i)
  (define visited (make-hash))
  (let loop ([lim lim]
             [q (deque-push r deque-empty)]
             [acc i])
    (cond
      [(deque-empty? q)
       acc]
      [(<= lim 0)
       acc]
      [else
       (let ([q (deque-shift q)]
             [p (deque-first q)])
         (cond
           [(hash-has-key? visited p)
            (loop lim q acc)]
           [else
            (hash-set! visited p #t)
            (let-values ([(add? acc) (f p acc)])
              (loop (sub1 lim)
                    (if add?
                        (for/fold ([q q])
                          ([n (in-list (posn-neighbors/m m p))])
                          (deque-push n q))
                        q)
                    acc))]))])))

(define (add-edge src dst m [kind #t])
  (matrix-update/posn 
   m src
   (lambda (n)
     (cond
       [(equal? (posn-up src) dst)
        (struct-copy node n [u kind])]
       [(equal? (posn-down src) dst)
        (struct-copy node n [d kind])]
       [(equal? (posn-left src) dst)
        (struct-copy node n [l kind])]
       [(equal? (posn-right src) dst)
        (struct-copy node n [r kind])]))))

(define (add-edge* src dst m [kind #t])
  (add-edge src dst (add-edge dst src m kind) kind))

(define (fold-edges path m f)
  (for/fold ([m m])
    ([src (in-list path)]
     [dst (in-list (rest path))])
    (f src dst m)))

(define (add-edges path m)
  (fold-edges path m add-edge*))

(define (hash-set* ht l)
  (for/fold ([ht ht])
    ([i (in-list l)])
    (hash-set ht i #t)))

;; Generate Path
(define (generate-path rows cols random-posn/prop start-posn)
  
  (define max-distance
    (posn-distance 
     (make-posn 0 0)
     (make-posn (sub1 rows) (sub1 cols))))
  
  (define (abs-powerup-interval i)
    (map 
     (curry * max-distance)
     (powerup-interval i)))
  
  (define m1 
    (build-matrix 
     rows cols 
     (lambda (ri ci)
       (make-node #f 
                  (not (= ri 0))
                  (not (= ci 0))
                  (not (= ci (sub1 cols)))
                  (not (= ri (sub1 rows)))))))
  (define (okay-distance-to-powerup? i d)
    (interval-in? d (abs-powerup-interval i)))
  
  (define-values (_mid-posn goals paths on-path?-ht target-posn?-ht)
    (for/fold ([start-posn start-posn]
               [goals empty]
               [paths empty]
               [on-path?-ht (make-immutable-hasheq empty)]
               [target-posn?-ht (make-immutable-hasheq empty)])
      ([which-powerup (in-range how-many-powerups)])
      
      (define end-posn 
        (random-posn/prop 
         (lambda (p)
           (and (not (hash-has-key? on-path?-ht p))
                (okay-distance-to-powerup? which-powerup (posn-distance start-posn p))))))
      ; XXX Magic number on next line: Maximum allowed distance of path... always fulfillable but leads to less meandering
      (define max-length (* 2.5 (posn-distance start-posn end-posn)))
      
      (define new-path 
        ; By starting at the end we encourage meandering there rather than at the beginning
        (reverse (greedy-dfs-path m1 end-posn start-posn max-length
                                  ; Don't allow it to cross a power-up
                                  (curry hash-has-key? target-posn?-ht))))
      
      (define next-start 
        ; XXX Magic number on next line: Where the new path starts along the old
        (random-list-ref new-path 0.6 0.8))
      
      (printf "\tDone with powerup ~a~n" which-powerup)
      (values next-start
              (cons end-posn goals)
              (cons new-path paths)
              (hash-set* on-path?-ht new-path)
              (hash-set target-posn?-ht end-posn which-powerup))))
  (values (reverse goals)
          (reverse paths)
          (lambda (p) (hash-has-key? on-path?-ht p))
          (lambda (p) (hash-ref target-posn?-ht p #f))))

;; Trimming edges
(define (trim-edges rows cols paths)
  (for/fold ([m
              (build-matrix 
               rows cols 
               (lambda (ri ci)
                 (make-node #f #f #f #f #f)))])
    ([path (in-list paths)])
    (add-edges path m)))

;; Normalizing regions
(define (mark-regions paths goals im)
  (for/fold ([m im])
    ([g (in-list goals)]
     [pth (in-list paths)]
     [i (in-naturals)])
    
    (bfs-search m g (length pth)
                (lambda (np m)
                  (define n (matrix-ref/posn m np))
                  (if (node-t n)
                      (values #f m)
                      (values #t
                              (matrix-set/posn m np (struct-copy node n [t i])))))
                m)))

;; Hidden Rooms
(define (add-hidden-rooms rows cols random-posn/prop on-path?/posn how-many-hidden im)
  (define valid? (make-valid-posn? rows cols))
  (define (posn-sq-neighbors* p)
    (filter 
     valid?
     (list (posn-up p)
           (posn-down p)
           (posn-left p)
           (posn-right p))))
  
  (define-values (m hiddens is-hidden?-ht)
    (for/fold ([fm im]
               [hiddens empty]
               [is-hidden?-ht (make-immutable-hasheq empty)])
      ([i (in-range how-many-hidden)])
      
      (define hidden-p
        (random-posn/prop
         (lambda (p)
           (and 
            ; Not used...
            (not (hash-has-key? is-hidden?-ht p))
            ; Not on the path...
            (not (on-path?/posn p))
            ; But a neighbor is
            (ormap on-path?/posn
                   (posn-sq-neighbors* p))))))
      
      (define connector
        (random-element
         (filter on-path?/posn
                 (posn-sq-neighbors* hidden-p))))
      
      (values (add-edge* hidden-p connector fm 'hidden)
              (list* hidden-p hiddens)
              (hash-set is-hidden?-ht hidden-p #t))))
  (values m hiddens 
          (lambda (p) (hash-has-key? is-hidden?-ht p))))

;; Cleaning up landlocked nodes
; The only nodes that are reachable on blank nodes are one color
(define (landlocked? m rp)
  (define an-id #f)
  (define visited (make-hash))
  (define (visit p)
    (cond
      [(hash-has-key? visited p)
       #t]
      [else
       (local [(define t (node-t (matrix-ref/posn m p)))]
         (hash-set! visited p #t)
         (if t
             (if an-id
                 (= t an-id)
                 (begin (set! an-id t)
                        #t))
             (for/and ([p (in-list (posn-neighbors/m m p))])
               (visit p))))]))
  (and (visit rp)
       an-id))

(define (normalize-landlocked hiddens paths m)
  (for*/fold ([m m])
    ([pth (in-list (list* hiddens paths))]
     [p (in-list pth)])
    (define n (matrix-ref/posn m p))
    (cond
      [(node-t n)
       m]
      [(landlocked? m p)
       => (lambda (c) 
            (matrix-set/posn m p (struct-copy node n [t c])))]
      [else
       m])))

;; Adding powerups
; Remember early edges to preserve solvability
(define (add-powerups-to-edges paths goals m)
  (define earlier (make-hash))
  (for/fold ([m m])
    ([pth (in-list paths)]
     [goal (in-list goals)]
     [i (in-naturals)])
    (fold-edges 
     pth m
     (lambda (src dst m)
       (define to-edge (cons src dst))
       (define from-edge (cons dst src))
       (define goal? (equal? dst goal))
       (begin0
         (if (or (zero? i) 
                 (hash-has-key? earlier to-edge)
                 (hash-has-key? earlier from-edge)
                 ; XXX Magic number: How often an edge requires a power up
                 (and (not goal?)
                      (not (zero? (random 3)))))
             m
             (add-edge src dst m
                       (if goal?
                           (max 0 (sub1 i))
                           (random i))))
         (hash-set! earlier to-edge #t))))))

;; Work
(define (generate-map rows cols)
  
  (define (random-posn)
    (make-posn (random rows) (random cols)))
  
  (define (random-posn/prop p?)
    (define p (random-posn))
    (if (p? p) p (random-posn/prop p?)))
  
  (define start-posn (random-posn))
  
  #;(printf "Generating path...~n")
  (define-values (goals paths on-path?/posn target-posn?/posn) 
    (generate-path rows cols random-posn/prop start-posn))
  
  ;; Now that we have the path... modify the graph to only allow it
  #;(printf "Trimming edges...~n")
  (define m2 (trim-edges rows cols paths))
  
  ;; Next add some hidden rooms
  #;(printf "Adding hidden rooms~n")
  (define-values (m3 hiddens is-hidden?/posn)
    (add-hidden-rooms rows cols random-posn/prop on-path?/posn how-many-hidden m2))
  
  #;(printf "Marking regions~n")
  (define m4 (mark-regions paths goals m3))
  
  #;(printf "Marking land-locked areas~n")  
  (define m5 (normalize-landlocked hiddens paths m4))
  
  #;(printf "Annotating edges~n")
  (define m6 (add-powerups-to-edges paths goals m5))
  
  (values start-posn is-hidden?/posn on-path?/posn target-posn?/posn m6))

(provide/contract
 ; XXX Better / use objects
 [generate-map (-> exact-nonnegative-integer? exact-nonnegative-integer?
                   (values posn? 
                           (posn? . -> . boolean?)
                           (posn? . -> . (or/c boolean? exact-nonnegative-integer?))
                           (posn? . -> . (or/c boolean? exact-nonnegative-integer?))
                           matrix?))]
 [struct node ([t (or/c false/c exact-nonnegative-integer?)]
               [u (or/c #f #t 'hidden exact-nonnegative-integer?)]
               [l (or/c #f #t 'hidden exact-nonnegative-integer?)]
               [r (or/c #f #t 'hidden exact-nonnegative-integer?)]
               [d (or/c #f #t 'hidden exact-nonnegative-integer?)])])