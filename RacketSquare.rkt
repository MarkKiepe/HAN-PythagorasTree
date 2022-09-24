#lang racket
(require racket/draw)


; Main Function

(define (draw-pythagoras width height start-size depth)
  ; Creating new bitmap
  (define target (make-bitmap width height))

  ; Creating drawing content (dc)
  (define dc (new bitmap-dc% [bitmap target]))

  ; Creating 'painting tools'
  (send dc set-brush "brown" 'solid)
  (send dc set-pen "transparant" 1 'solid)

  ; Drawing pytagoras tree
  (calculate-and-draw-tree-parts
      dc
      depth
      (- (/ width 2) (/ start-size 2)) ; x1
      height ; y1
      (+ (/ width 2) (/ start-size 2)) ; x2
      height ; y2
  )

  ; Saving file
  (send target save-file "Pythagoras.png" 'png)
)


; Square Functions

(define (calculate-and-draw-tree-parts dc depth-left x1 y1 x2 y2)
  ; Calculating original size
  (define width (- x2 x1))
  (define height (- y1 y2))

  ; Calculating additional points
  (define x3 (- x2 height))
  (define y3 (- y2 width))
  (define x4 (- x1 height))
  (define y4 (- y1 width))
  (define x5 (+ x4 (/ (- width height) 2)))
  (define y5 (- y4 (/ (+ width height) 2)))

  ; Changing color
  (send dc set-brush (get-color-for-depth depth-left) 'solid)
  
  ; Drawing square
  (draw-square dc x1 y1 x2 y2 x3 y3 x4 y4)

  ; Recursion?
  (cond
    ; Base Case
    [(<= depth-left 0) "break"]

    ; Making progress
    [
     ; Drawing Left square
     (calculate-and-draw-tree-parts
        dc ; drawing content
        (- depth-left 1) ; making progress
        ;
        x4
        y4
        x5
        y5
     )

     ; Drawing Right square
     (calculate-and-draw-tree-parts
        dc ; drawing content
        (- depth-left 1) ; making progress
        ;
        x5
        y5
        x3
        y3
     )
    ]
  )
)

(define (draw-square dc x1 y1 x2 y2 x3 y3 x4 y4)
  ; Drawing new square from positions
  (define square-path
    (let ([p (new dc-path%)])
      (send* p (move-to x1 y1)
        (line-to x2 y2)
        (line-to x3 y3)
        (line-to x4 y4)
        (line-to x1 y1)
        )
      p)
    )
  (send dc draw-path square-path)
)


; Misc functions

(define (get-color-for-depth depth)
  (cond
    [(<= depth 2) "green"]
    ; else
    ["brown"]
  )
)


; Initial Function Call

(draw-pythagoras
    750 ; image width
    500 ; image height
    100 ; tree root square size
    6   ; tree depth
)