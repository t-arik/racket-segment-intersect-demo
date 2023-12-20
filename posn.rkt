#lang racket

(require racket/trace)

(provide (all-defined-out))

(define-struct posn (x y) #:prefab)

(define (posn-add posn1 posn2)
  (make-posn
    (+ (posn-x posn1) (posn-x posn2))
    (+ (posn-y posn1) (posn-y posn2))))

(define (posn-sub posn1 posn2)
  (make-posn
    (- (posn-x posn1) (posn-x posn2))
    (- (posn-y posn1) (posn-y posn2))))

(define (cross-product posn1 posn2)
  (- (* (posn-x posn1) (posn-y posn2))
     (* (posn-y posn1) (posn-x posn2))))

(define (compute-direction p1 p2 p3)
  (let* ([vector1 (posn-sub p2 p1)]
         [vector2 (posn-sub p3 p1)])
    (cross-product vector1 vector2)))

; Source: Cormen page 1018
(define (on-segment p1 p2 p3)
  (let ([min-x (min (posn-x p1) (posn-x p2))]
        [max-x (max (posn-x p1) (posn-x p2))]
        [min-y (min (posn-y p1) (posn-y p2))]
        [max-y (max (posn-y p1) (posn-y p2))])
    (and (<= min-x (posn-x p3) max-x)
         (<= min-y (posn-y p3) max-y))))

; Source: Cormen page 1018
; intersection of segments p1-p2 and p3-p4
(define (intersection? p1 p2 p3 p4)
  (let ([d1 (compute-direction p3 p4 p1)]
        [d2 (compute-direction p3 p4 p2)]
        [d3 (compute-direction p1 p2 p3)]
        [d4 (compute-direction p1 p2 p4)])
    (or
      (and (or (and (> d1 0) (< d2 0)) (and (< d1 0) (> d2 0)))
           (or (and (> d3 0) (< d4 0)) (and (< d3 0) (> d4 0))))
      (and (= d1 0) (on-segment p3 p4 p1))
      (and (= d2 0) (on-segment p3 p4 p2))
      (and (= d3 0) (on-segment p1 p2 p3))
      (and (= d4 0) (on-segment p1 p2 p4)))))


#| (trace intersection?) |#
