#lang racket
(require
  2htdp/image
  2htdp/universe
  "board.rkt"
  "kv.rkt")

(define (render state)
  (cond
    [(empty? state) (Board.draw-empty (Board.new))]
    [else (Board.draw
            (KV.get state 'board)
            (first (KV.get state 'players)))]))

(define (handle-key state key)
  (cond
    [(key=? key "up") (make-package state '(go up))]
    [(key=? key "down") (make-package state '(go down))]
    [(key=? key "left")  (make-package state '(go left))]
    [(key=? key "right") (make-package state '(go right))]
    [else state]))

(define (handle-message _state msg) msg)

(define (start-client)
  (big-bang
    empty
    #| (state #t) ; NOTE great for debugging |#
    (name "Player 1")
    (on-key handle-key)
    (register LOCALHOST)
    (on-receive handle-message)
    (to-draw render)))

#| (launch-many-worlds |#
#|   (start-client) |#
#|   (start-client)) |#

(start-client)

