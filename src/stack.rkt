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
