#lang racket
; 2. Higher-order functions 
 
; a. make-linear
(define (make-linear a b)
  (lambda (x) (+ (* a x) b)))


; b. add-linear
(define (add-linear f g)
  (lambda (x) (+ (f x) ( g x))))


; c. make-linear-list 
(define (make-linear-list a b) 
  (lambda (lis)
    (map (make-linear a b) lis)))

