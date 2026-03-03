#lang racket
; 1. Various Functions
; 
; a. Write the function countdown which takes an integer i>= 1 as an
; argument and returns the list (n n-1 ... 2 1).
; 
; Test Cases:
; (countdown 5)
; (5 4 3 2 1)

(define (countdown n)
  (if (= n 0)
      '()
      (cons n (countdown (- n 1)))))

(countdown 5)

; b. Write the function remove-every-second which removes every other
; element of its parameter list. Given the list (a1 a2 ... an), the function returns
; the list (a1 a3... ak), where k = n if n is odd, otherwise k = n-1.
; 
; Test Cases:
; (remove-every-second '(a b c d))
; (a c)
; (remove-every-second '(a b d e f))
; (a d f)

(define (remove-every-second lis)
  (cond
    ((null? lis) '()) ; base case empty list
    ((null? (cdr lis)) lis) ; if only one element return
    (else (cons (car lis) ; keep first
                (remove-every-second (cddr lis))))))

(remove-every-second '(a b c d))
(remove-every-second '(a b d e f))

; c. Write the function insert-at that, given a list lis, an integer i>= 1,
; and an element elem, returns a copy of lis with elem inserted at position
; i (the first position is i=1). If i > length(lis)+1, return lis unchanged.
; You are allowed to use the built-in function length in your solution
; 
; Test Cases:
; (insert-at '(a b c d) 3 'z)
; (a b z c d)
; (insert-at '(a b c d) 5 'z)
; (a b c d z)
; (insert-at '(a b c d) 6 'z)
; (a b c d)

(define (insert-at lis i elem)
  (cond 
    [(null? lis) (cons elem '())]
    [(> i (+ (length lis) 1)) lis]
    [(equal? i 1) (cons elem lis)]
    [else(cons (car lis) 
      (insert-at (cdr lis) (- i 1) elem))
    ]
  )
)

(insert-at '(a b c d) 3 'z)
(insert-at '(a b c d) 5 'z)
(insert-at '(a b c d) 6 'z)

; d. Write the function sequence which takes an integer n >= 1 as argument
; and returns the list (1 2 … n-1 n). Hint: You need a helper function.
; 
; Test Cases:
; (sequence 6)
; (1 2 3 4 5 6)
; (sequence 1)
; '(1)

(define (seq_helper n i)
  (cond
    [(equal? n i) (cons i '())]
    [else (cons i (seq_helper n (+ i 1)))]
  )
) 

(define (sequence n)
  (if (equal? n 0) 
    '()
    (seq_helper n 1)
  )
)

(sequence 6)
(sequence 1)


; e. Write the function combine which takes three arguments: fun (a
; function), init (an initial value), and lis (a list). The function combines
; the elements of the list lis into a single value using fun and the initial
; value init. If the list lis is empty, then combine returns init 
; 
; Test Cases:
; (combine + 0 '(1 2 3 4 5))
; 15
; (combine * 1 '(1 3 5 2 4))
; 120
; (combine max 0 '(1 3 5 2 4))
; 5



; g. Write the function split which takes a boolean function fun and a list
; lis as arguments. split returns a list of two lists (lis1 lis2) where
; lis1 contains elments from lis which satisfy the predicate fun and
; lis2 contains elements from lis which do not satisfy the predicate fun.
; Note: You need a helper function.
; 
; Test Cases:
; (split even? '(1 2 3 4 5 6))
; ((6 4 2) (5 3 1))
; (split (lambda(x) (> x 2)) '(1 2 3 4 5 6))
; ((6 5 4 3) (2 1)) 