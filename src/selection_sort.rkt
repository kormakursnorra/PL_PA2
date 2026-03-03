#lang racket
; 3. Selection sort
; 
; a. delete-elem 

(define (delete-elem elem lis)
    (cond 
        [(null? lis) '()]
        [(equal? elem (car lis)) (cdr lis)]
        [else (cons (car lis) 
            (delete-elem elem (cdr lis)))]
    )
)

(delete-elem 1 '(1 2 3 4))
(delete-elem 3 '(1 2 3 4 3))

; b. select-min 

(define (select-min lis)
    (if (null? lis) '()
    (if (null? (cdr lis)) 
        (car lis)
        (let ([min (select-min (cdr lis))])
        (if (< min (car lis)) 
            min 
            (car lis)
        ))
    ))
)

(select-min '(4 2 1 5 7))

; c. selection-sort

(define (selection-sort lis) 
    (if (null? lis) 
        '()
        (cons (select-min lis) 
            (selection-sort (delete-elem (select-min lis) lis))
        )
    ) 
)

(selection-sort '(3 5 2 1 2 5 4 6))