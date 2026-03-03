#lang racket

; 1. Various Functions

; a. countdown
(define (countdown n)
  (if (= n 0)
      '()
      (cons n (countdown (- n 1)))))



; b. remove-every-second
(define (remove-every-second lis)
  (cond
    ((null? lis) '()) ; base case empty list
    ((null? (cdr lis)) lis) ; if only one element return
    (else (cons (car lis) ; keep first
                (remove-every-second (cddr lis))))))


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

; e. combine
(define (combine fun init lis)
  (cond
    [(null? lis) init]
    [else(fun (combine fun init (cdr lis)) (car lis))]
  )
)


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
