#lang racket

(require
  2htdp/image
  racket/trace
  rackunit
  "player.rkt"
  "posn.rkt"
  "kv.rkt")

(provide
  Board.new
  Board.draw
  Board.draw-empty
  Board.add-piece
  Board.move-piece
)

(define-struct piece (posn owner-id) #:prefab)

(define (Board.new)
  (--> (KV.new)
       (curryr KV.set 'width 20)
       (curryr KV.set 'height 20)
       (curryr KV.set 'walls (list
                               (make-posn 6 6)
                               (make-posn 7 6)
                               (make-posn 8 6)
                               (make-posn 9 6)
                               (make-posn 10 6)
                               (make-posn 11 6)
                               (make-posn 12 7)
                               (make-posn 12 8)
                               (make-posn 12 9)
                               (make-posn 12 10)
                               (make-posn 12 11)
                               (make-posn 11 12)
                               (make-posn 10 12)
                               (make-posn 9 12)
                               (make-posn 8 12)
                               (make-posn 7 12)
                               (make-posn 6 12)))
       (curryr KV.set 'pieces empty)))

; owner is expected to be a player
(define (Board.add-piece board owner)
  (let*
    ([owner-id (Player.id owner)]
     [new-piece (make-piece (make-posn 0 0) owner-id)])
    (KV.update board 'pieces (curry cons new-piece))))


(define (Board.move-piece board player direction)
  (let* ([posn (--> player (curry player->piece board) piece-posn)]
         [new-position
           (match direction
             ['up (posn-add posn (make-posn 0 -1))]
             ['down (posn-add posn (make-posn 0 1))]
             ['left (posn-add posn (make-posn -1 0))]
             ['right (posn-add posn (make-posn 1 0))])])
    (if (equal? 'floor (posn->tile board new-position))
      (Board.set-piece-position board player new-position)
      #false)))

(define (Board.set-piece-position board player posn)
  (let* ([player-piece (player->piece board player)]
         [pieces (KV.get board 'pieces)]
         [piece-index (index-of pieces player-piece)]
         [new-piece (struct-copy piece player-piece [posn posn])]
         [new-pieces (list-set pieces piece-index new-piece)])
    (KV.set board 'pieces new-pieces)))


; checks if posn is visible from the piece's position
(define (visibile? board player posn)
  (let* ([walls (KV.get board 'walls)]
         [wall-segments (apply append (map tile->segments walls))]
         [piece (player->piece board player)])
    (not (ormap
           (lambda (segment)
             (intersection?
               (first segment)
               (second segment)
               (piece-posn piece)
               posn))
           wall-segments))))
#| (trace visibile?) |#

(define (Board.draw-empty board)
  (let*
    ([width (KV.get board 'width)]
     [height (KV.get board 'height)])
    (empty-scene (scale-to-screen width) (scale-to-screen height))))

(define (Board.draw board player)
  (let*
    ([width (KV.get board 'width)]
     [height (KV.get board 'height)]
     [background (empty-scene (scale-to-screen width) (scale-to-screen height))])
    (--> (cartesian-product (range width) (range height))
         (curry map (curry apply make-posn))
         (curry foldl
                (lambda (posn acc-bg)
                  (let ([tile (posn->tile board posn)])
                  (place-tile-image
                    (if (or (visibile? board player posn) (equal? tile 'wall))
                      (tile->image tile)
                      (tile->image 'fog))
                    posn 
                    acc-bg)))
                  background))))

; AUX FUNCTIONS

(define (scale-to-screen x) (* 20 x))

(define (player->piece board player)
  (let ([pieces (KV.get board 'pieces)]
        [owner-id (Player.id player)])
    (findf
      (lambda (piece) (equal? owner-id (piece-owner-id piece)))
      pieces)))

(define (place-tile-image tile-image posn background)
  (let
    ([offset (/ (scale-to-screen 1) 2)])
  (place-image
    tile-image
    (--> posn posn-x scale-to-screen (curry + offset))
    (--> posn posn-y scale-to-screen (curry + offset))
    background)))

(define (posn->tile board posn)
  (let 
    ([piece-positions (map piece-posn (KV.get board 'pieces))]
     [wall-positions (KV.get board 'walls)])
    (cond
      [(member posn piece-positions) 'piece]
      [(member posn wall-positions) 'wall]
      [else 'floor])))

(define (load-bitmap path)
  (let ([img (bitmap/file path)])
    (scale (/ (scale-to-screen 1) (image-width img)) img)))

(define FLOOR-IMG (load-bitmap "images/floor.png"))
(define WALL-IMG (load-bitmap "images/wall.png"))
(define FOG-IMG (load-bitmap "images/fog.png"))
(define PIECE-IMG (load-bitmap "images/piece.png"))

(define (tile->image tile)
  (match tile
    #| ['floor (square (scale 1) "outline" "black")] |#
    ['floor FLOOR-IMG]
    ['wall WALL-IMG]
    ['fog FOG-IMG]
    ['piece PIECE-IMG]))

; get the.5 segments of a tile.
(define (tile->segments tile)
  (let* ([x (posn-x tile)]
         [y (posn-y tile)])
    (list
      (list (make-posn (- x 0.5) (- y 0.5)) (make-posn (+ x 0.5) (- y 0.5)))
      (list (make-posn (+ x 0.5) (- y 0.5)) (make-posn (+ x 0.5) (+ y 0.5)))
      (list (make-posn (+ x 0.5) (+ y 0.5)) (make-posn (- x 0.5) (+ y 0.5)))
      (list (make-posn (- x 0.5) (+ y 0.5)) (make-posn (- x 0.5) (- y 0.5))))))

