#lang racket
; 2. Higher-order functions 
; 
; a. make-linear

(define (make-linear a b)
  (lambda (x) (+ (* a x) b)))

(define f (make-linear 2 1))
(f 5)

(define g (make-linear 3 4))
(g 5)

; b. add-linear

(define (add-linear f g)
  (lambda (x) (+ (f x) ( g x))))

((add-linear f g) 5)

; c. make-linear-list 

(define (make-linear-list a b) 
  (lambda (lis)
    (map (make-linear a b) lis)))

(define h (make-linear-list 2 1))
(h '(1 2 3))