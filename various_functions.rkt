; 1. Various Functions
; a - Countdown
(define (countdown n)
  (if (= n 0)
      '()
      (cons n (countdown (- n 1)))))

(countdown 5)

; b - remove every second
(define (remove-every-second lis)
  (cond
    ((null? lis) '()) ; base case empty list
    ((null? (cdr lis)) lis) ; if only one element return
    (else (cons (car lis) ; keep first
                (remove-every-second (cddr lis))))))
(remove-every-second '(a b c d))
(remove-every-second '(a b d e f))

; c - insert at
(define (insert-at lis, i, elem)
  