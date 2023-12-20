#lang racket

(require racket/trace)

(provide
  -->
  KV.new
  KV.set
  KV.get
  KV.remove
  KV.update)

; pipe/threading operator like in clojure or elixir
(define (--> value . functions)
  (foldl (λ (f v) (f v)) value functions))


(define (KV.new) '())

; list? any? -> any? or #false
(define (KV.get store key)
  (let ([pair (findf (lambda (pair) (eq? key (first pair))) store)])
    (if pair (second pair) #false)))


; list? any? -> list?
(define (KV.remove store key)
  (filter
    (lambda (pair)
      (--> pair first (curry eq? key) not))
    store))


; list? any? any? -> list?
(define (KV.set store key value)
  (let ([pair (KV.get store key)])
    (if pair
      (cons (list key value) (KV.remove store key))
      (cons (list key value) store))))


; Updates the value associated with the given key in the given store
; using the given function.
; list? any? (any? -> any?) -> list?
(define (KV.update store key function)
  (let ([value (KV.get store key)])
    (if value
      (KV.set store key (function value))
      store)))

