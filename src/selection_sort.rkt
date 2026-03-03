#lang racket
; 3. Selection sort


; a. delete-elem 
(define (delete-elem elem lis)
    (cond 
        [(null? lis) '()]
        [(equal? elem (car lis)) (cdr lis)]
        [else (cons (car lis) 
            (delete-elem elem (cdr lis)))]
    )
)


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


; c. selection-sort
(define (selection-sort lis) 
    (if (null? lis) 
        '()
        (cons (select-min lis) 
            (selection-sort (delete-elem (select-min lis) lis))
        )
    ) 
)
