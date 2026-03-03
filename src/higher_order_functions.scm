(declare (unit higher_order_functions))
(import (chicken sort))

(define (make-linear a b)
  (lambda (x) (+ (* a x) b)))


(define (add-linear f g)
  (lambda (x) (+ (f x) ( g x))))


(define (make-linear-list a b) 
  (lambda (lis)
    (map (make-linear a b) lis)))

