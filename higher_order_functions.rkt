#lang racket
; 2. Higher-order functions 
; 
; a. Write the function make-linear which accepts the numbers a and b as 
; arguments and returns a new function f(x) = a*x + b

(define (make-linear a b)
  (lambda (x) (+ (* a x) b)))

; b. Write the function add-linear which accepts linear functions f and g as
; arguments and returns a new function h(x) = f (x) + g (x).

(define (add-linear f g)
  (lambda (x) (+ (f x) ( g x))))

; c. Write the function make-linear-list which accepts numbers a and b
; as arguments and returns a function which accepts a list lis as an argument
; and applies the function f(x) = a*x + b to each element of lis 

(define (make-linear-list a b) 
  (lambda (lis)
    (map (make-linear a b) lis)))