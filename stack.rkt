#lang racket

(define (make-stack stack)
  (lambda (command . args)
    (cond
      ((eq? command 'empty) (null? stack))
      ((eq? command 'top)
       (if (null? stack)
           'error
           (car stack)))
      ((eq? command 'pop)
       (if (null? stack)
           'error
           (let ((val (car stack)))
             (set! stack (cdr stack))
             val)))
      ((eq? command 'push)
       (let ((val (car args)))
         (set! stack (cons val stack))))
      (else 'error)
      )
    ))



(define stack1 (make-stack '()))
(stack1 'empty)
(stack1 'bla)
(stack1 'top)
(stack1 'pop)
(stack1 'push 2)
(stack1 'push 3)
(stack1 'top)
(stack1 'pop)
(stack1 'pop)
