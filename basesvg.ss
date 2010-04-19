#lang scheme/gui
(require "gl-sprites.ss"
         xml)

(define path
  (command-line 
   #:program "basesvg"
   #:args (path)
   path))

(for-each
 (match-lambda
   [(list-rest id i)
    (define label (format "~a~a" id i))
    (define png (build-path path (format "~a.png" label)))
    (define svg (build-path path (format "~a.svg" label)))
    (define bm (make-object bitmap% png 'png/mask #f))
    (define w (send bm get-width))
    (define h (send bm get-height))
    (define svg-xexpr
      `(svg:svg ([xmlns:svg "http://www.w3.org/2000/svg"]
                 [xmlns:xlink "http://www.w3.org/1999/xlink"]
                 [xmlns:sodipodi "http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd"]
                 [xmlns:inkscape "http://www.inkscape.org/namespaces/inkscape"]
                 [xmlns "http://www.w3.org/2000/svg"]
                 [version "1.0"]
                 [id ,label]
                 [height ,(number->string h)]
                 [width ,(number->string w)])
                (sodipodi:namedview ([id "base"]
                                     [inkscape:current-layer "Shapes"]
                                     [showgrid "true"]))
                (svg:image ([height ,(number->string h)]
                            [width ,(number->string w)]
                            [x "0"]
                            [y "0"]
                            #;[sodipodi:absref ,(format "~a.png" label)]
                            [xlink:href ,(format "~a.png" label)]))
                (svg:g ([id "Shapes"]
                        [inkscape:groupmode "layer"]
                        [inkscape:label "Shapes"]
                        [style "opacity: 0.2"]))))
    (with-output-to-file svg
      (lambda ()
        (display (xexpr->string svg-xexpr)))
      #:exists 'replace)])
 (list-sprites path))