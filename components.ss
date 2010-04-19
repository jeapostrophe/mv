#lang scheme
(require "room.ss")

(define hallway
  (make-component
   'hallway
   5.0 4.0
   (make-immutable-hasheq
    (list (cons 'left (make-door no-power-up 0.5 3.5))
          (cons 'right (make-door no-power-up 0.5 3.5))))))
(define hallway/bhole
  (make-component
   'hallway/bhole
   5.0 4.0
   (make-immutable-hasheq
    (list (cons 'left (make-door no-power-up 0.5 3.5))
          (cons 'right (make-door no-power-up 0.5 3.5))
          (cons 'bottom (make-door no-power-up 1.5 2.5))))))
(define hallway/thole
  (make-component
   'hallway/thole
   5.0 4.0
   (make-immutable-hasheq
    (list (cons 'left (make-door no-power-up 0.5 3.5))
          (cons 'right (make-door no-power-up 0.5 3.5))
          (cons 'top (make-door no-power-up 1.5 2.5))))))
(define junction
  (make-component
   'junction
   5.0 5.0
   (make-immutable-hasheq
    (list (cons 'left (make-door no-power-up 0.5 3.5))
          (cons 'right (make-door no-power-up 0.5 3.5))
          (cons 'top (make-door no-power-up 2.0 3.0))
          (cons 'bottom (make-door no-power-up 2.0 3.0))))))
(define elbow/bottom/left
  (make-component
   'elbow/bottom/left
   5.0 5.0
   (make-immutable-hasheq
    (list (cons 'left (make-door no-power-up 0.5 3.5))
          (cons 'bottom (make-door no-power-up 2.0 3.0))))))
(define elbow/top/left
  (make-component
   'elbow/top/left
   5.0 5.0
   (make-immutable-hasheq
    (list (cons 'left (make-door no-power-up 0.5 3.5))
          (cons 'top (make-door no-power-up 2.0 3.0))))))
(define elbow/bottom/right
  (make-component
   'elbow/bottom/right
   5.0 5.0
   (make-immutable-hasheq
    (list (cons 'right (make-door no-power-up 0.5 3.5))
          (cons 'bottom (make-door no-power-up 2.0 3.0))))))
(define elbow/top/right
  (make-component
   'elbow/top/right
   5.0 5.0
   (make-immutable-hasheq
    (list (cons 'right (make-door no-power-up 0.5 3.5))
          (cons 'top (make-door no-power-up 2.0 3.0))))))
(define platform-pipe
  (make-component
   'platform-pipe
   5.0 5.0
   (make-immutable-hasheq
    (list (cons 'top (make-door no-power-up 2.0 3.0))
          (cons 'bottom (make-door no-power-up 2.0 3.0))))))
(define platform-pipe/left
  (make-component
   'platform-pipe/left
   5.0 5.0
   (make-immutable-hasheq
    (list (cons 'top (make-door no-power-up 2.0 3.0))
          (cons 'bottom (make-door no-power-up 2.0 3.0))
          (cons 'left (make-door no-power-up 1.0 4.0))))))
(define platform-pipe/right
  (make-component
   'platform-pipe/right
   5.0 5.0
   (make-immutable-hasheq
    (list (cons 'top (make-door no-power-up 2.0 3.0))
          (cons 'bottom (make-door no-power-up 2.0 3.0))
          (cons 'right (make-door no-power-up 1.0 4.0))))))

(define dead-end/left
  (make-component
   'dead-end/left
   5.0 4.0
   (make-immutable-hasheq
    (list (cons 'left (make-door no-power-up 0.5 3.5))))))
(define dead-end/right
  (make-component
   'dead-end/right
   5.0 4.0
   (make-immutable-hasheq
    (list (cons 'right (make-door no-power-up 0.5 3.5))))))
(define dead-end/bottom
  (make-component
   'dead-end/bottom
   5.0 5.0
   (make-immutable-hasheq
    (list (cons 'bottom (make-door no-power-up 2.0 3.0))))))
(define dead-end/top
  (make-component
   'dead-end/top
   5.0 5.0
   (make-immutable-hasheq
    (list (cons 'top (make-door no-power-up 2.0 3.0))))))

;; Power up components!
(define platform-pipe/double
  (make-component
   'platform-pipe/double
   5.0 10.0
   (make-immutable-hasheq
    (list (cons 'top (make-door 'double-jump 2.0 3.0))
          (cons 'bottom (make-door no-power-up 2.0 3.0))))))
(define high-ledge/right
  (make-component
   'high-ledge/right
   5.0 10.0
   (make-immutable-hasheq
    (list (cons 'right (make-door 'double-jump 0.5 3.5))
          (cons 'bottom (make-door no-power-up 2.0 3.0))))))
(define high-ledge/left
  (make-component
   'high-ledge/left
   5.0 10.0
   (make-immutable-hasheq
    (list (cons 'left (make-door 'double-jump 0.5 3.5))
          (cons 'bottom (make-door no-power-up 2.0 3.0))))))

;; Collection
(define components
  (list hallway
        hallway/bhole
        hallway/thole
        junction
        platform-pipe
        platform-pipe/left
        platform-pipe/right
        elbow/bottom/left
        elbow/top/left
        elbow/bottom/right
        elbow/top/right
        dead-end/left
        dead-end/right
        dead-end/top
        dead-end/bottom
        ;; power-up!
        high-ledge/left
        high-ledge/right
        platform-pipe/double))

(provide/contract
 [components (listof component?)])