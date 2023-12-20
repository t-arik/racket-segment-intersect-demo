#lang racket

(require
  2htdp/universe
  racket/trace
  "board.rkt"
  "player.rkt"
  "kv.rkt")


(define (broadcast state)
  (let ([worlds (KV.get state 'worlds)]
        [msg (KV.remove state 'worlds)]) ; Send over everything but worlds
    (map (curryr make-mail msg) worlds)))


(define (handle-message state world msg)
  (let* ([player (world->player state world)]
         [new-state
           (match msg
             [(list 'go direction) (move-player state player direction)]
             [_ state])])
    (make-bundle new-state (broadcast new-state) empty)))

; TODO check world bounds
(define (move-player state player direction)
  (let* ([board (KV.get state 'board)]
        [new-board (Board.move-piece board player direction)])
    (if (false? new-board)
      state
      (KV.set state 'board new-board))))

(define (world->player state world)
  (let ([players (KV.get state 'players)]
        [world-id (iworld-name world)])
    (findf
      (lambda (player) (equal? world-id (Player.id player)))
      players)))


(define (handle-new state world) ; TODO reject if name/id is already taken
  (let* ([new-player (Player.new (iworld-name world))]
         [new-state (--> state
                         (curryr KV.update 'worlds (curry cons world))
                         (curryr KV.update 'players (curry cons new-player))
                         (curryr KV.update 'board (curryr Board.add-piece new-player)))])
    (make-bundle
      new-state
      (broadcast new-state)
      empty)))

(universe
  (--> (KV.new)
       (curryr KV.set 'board (Board.new))
       (curryr KV.set 'worlds empty)
       (curryr KV.set 'players empty))
  (on-msg handle-message)
  (on-new handle-new))

