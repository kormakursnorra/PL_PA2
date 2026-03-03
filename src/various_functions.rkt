#lang racket
; 1. Various Functions
; 
; a. countdown

(define (countdown n)
  (if (= n 0)
      '()
      (cons n (countdown (- n 1)))))

(countdown 5)

; b. remove-every-second

(define (remove-every-second lis)
  (cond
    ((null? lis) '()) ; base case empty list
    ((null? (cdr lis)) lis) ; if only one element return
    (else (cons (car lis) ; keep first
                (remove-every-second (cddr lis))))))

(remove-every-second '(a b c d))
(remove-every-second '(a b d e f))

; c. insert-at

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

; d. sequence

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


; e. combine

(define (combine fun init lis)
  (cond
    [(null? lis) init]
    [else(fun (combine fun init (cdr lis)) (car lis))]
  )
)

(combine + 0 '(1 2 3 4 5))
(combine * 1 '(1 3 5 2 4))
(combine max 0 '(1 3 5 2 4))

; g. split

(define (split_helper fun lis lis1 lis2)
  (cond
    [(null? lis) (list lis1 lis2)]
    [(fun (car lis))  
        (split_helper fun (cdr lis) 
          (cons (car lis) lis1) lis2)
    ]
    [else
        (split_helper fun (cdr lis) 
          lis1 (cons (car lis) lis2))
    ]
  )
)

(define (split fun lis)
  (split_helper fun lis '() '())
)

(split even? '(1 2 3 4 5 6))
(split (lambda(x) (> x 2)) '(1 2 3 4 5 6))