#lang racket

(require "kv.rkt")

(provide
  Player.new
  Player.id)

(define (Player.new id)
  (--> (KV.new)
       (curryr KV.set 'id id)))

(define (Player.id player)
  (KV.get player 'id))

