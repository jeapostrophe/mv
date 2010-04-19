#lang scheme
(require (planet jaymccarthy/gl2d))

; SVG
(require xml)
(define (file->xexpr pth)
  (with-input-from-file pth
    (lambda ()
      (parameterize ([collapse-whitespace #t]
                     [xexpr-drop-empty-attributes #t])
        (xml->xexpr (document-element (read-xml)))))))

; Sprites
(define (list-sprites pth)
  (filter-map
   (lambda (p)
     (define s (path->string p))
     (and (not (regexp-match #rx"^\\." s))
          (match (regexp-match #rx"^(.*?)([0-9]*)\\.png" s)
            [(list _ anim id)
             (cons (string->symbol anim) (string->number id))]
            [_
             #f])))
   (directory-list pth)))

(define-struct sprites (dir anims frames shapes) #:prefab)

(define (find-sprites pth)
  (define anims
    (for/fold ([ht (make-immutable-hasheq empty)])
      ([l (in-list (list-sprites pth))])
      (match l
        [(list-rest anim i)
         (hash-update ht anim (compose inexact->exact (curry max (add1 i))) -inf.0)])))
  (make-sprites pth anims (make-hasheq) (make-hasheq)))

(define-struct shapes (mh mw ss) #:prefab)
(define-struct shape (i) #:prefab)
(define-struct (rect shape) (x y h w) #:prefab)
(define-struct (line shape) (x1 y1 x2 y2) #:prefab)

(define (s->n v [round floor]) (exact->inexact (round (string->number v))))
(define xexpr->shapes
  (match-lambda
    [`(svg ,(list-no-order `[height ,h] `[width ,w] _ ...) .
           ,(list-no-order `(g ,(list-no-order `[id "Shapes"] _ ...) . ,shapes) _ ...))
     (define mh (s->n h))
     (define mw (s->n w))
     (make-shapes
      mh mw 
      (filter-map
       (match-lambda
         [`(rect ,(list-no-order `[id ,id] `[height ,h] `[width ,w]
                                 `[x ,x] `[y ,y] _ ...))
          (make-rect id (s->n x) (s->n y) (s->n h ceiling) (s->n w ceiling))]
         [_
          #f])
       shapes))]))

(define load-sprites!
  (match-lambda
    [(struct sprites (dir anims frames shapes))
     (for ([(id m) (in-hash anims)])
       (hash-set! 
        frames id
        (list->vector
         (for/list ([i (in-range m)])
           (gl-load-texture (build-path dir (format "~a~a.png" id i))))))
       (hash-set! 
        shapes id
        (list->vector
         (for/list ([i (in-range m)])
           (xexpr->shapes (file->xexpr (build-path dir (format "~a~a.svg" id i))))))))]))

(define (frame-i anims anim i delay)
  (floor (/ (modulo i (* (hash-ref anims anim) delay)) delay)))

(define mirror-shape
  (match-lambda*
    [(list mh mw (struct rect (i x y h w)))
     (make-rect i (- mw x) y h (* -1 w))]))
(define mirror-shapes
  (match-lambda
    [(and cs (struct shapes (mh mw ss)))
     (struct-copy shapes cs 
                  [ss
                   (map (curry mirror-shape mh mw) ss)])]))

(define (animation-shapes ss anim i delay #:mirror? [mirror? #f])
  (match ss
    [(struct sprites (dir anims frames shapes-ht))
     (define shapes (vector-ref (hash-ref shapes-ht anim) (frame-i anims anim i delay)))
     (if mirror?
         (mirror-shapes shapes)
         shapes)]))

(define (render-sprite ss anim i delay w h #:mirror? [mirror? #f])
  (match ss
    [(struct sprites (dir anims frames shapes))
     (gl-bind-texture (vector-ref (hash-ref frames anim) (frame-i anims anim i delay)))
     (if mirror?
         (with-mirror w (gl-draw-rectangle/texture w h))
         (gl-draw-rectangle/texture w h))]))

(provide/contract
 [list-sprites (path-string? . -> . (listof (cons/c symbol? exact-nonnegative-integer?)))]
 [struct sprites 
         ([dir path-string?]
          [anims (hash/c symbol? exact-nonnegative-integer? #:immutable #t)]
          [frames (hash/c symbol? (vectorof gl-texture?) #:immutable #f)]
          [shapes (hash/c symbol? (vectorof shapes?) #:immutable #f)])]
 [find-sprites (path-string? . -> . sprites?)]
 [struct shapes
         ([mh number?]
          [mw number?]
          [ss (listof shape?)])]
 ; XXX Change i field to set of symbols
 [struct shape
         ([i string?])]
 [struct rect 
         ([i string?]
          [x number?]
          [y number?]
          [h number?]
          [w number?])]
 [struct line
         ([i string?]
          [x1 number?]
          [y1 number?]
          [x2 number?]
          [y2 number?])]
 [load-sprites! (sprites? . -> . void)]
 [animation-shapes
  (->d ([ss sprites?] [anim symbol?] [i exact-nonnegative-integer?] [delay exact-nonnegative-integer?])
       (#:mirror? [mirror? boolean?])
       #:pre-cond (and (hash-has-key? (sprites-anims ss) anim)
                       (hash-has-key? (sprites-shapes ss) anim))
       [ans shapes?])]
 [render-sprite 
  (->d ([ss sprites?] [anim symbol?] [i exact-nonnegative-integer?] [delay exact-nonnegative-integer?] [w real?] [h real?])
       (#:mirror? [mirror? boolean?])
       #:pre-cond (and (hash-has-key? (sprites-anims ss) anim)
                       (hash-has-key? (sprites-frames ss) anim))
       [ans void])])