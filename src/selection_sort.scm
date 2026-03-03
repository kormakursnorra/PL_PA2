(declare (unit selection_sort))
(import (chicken sort))

(define (delete-elem elem lis)
    (cond 
        [(null? lis) '()]
        [(equal? elem (car lis)) (cdr lis)]
        [else (cons (car lis) 
            (delete-elem elem (cdr lis)))]
    )
)


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



(define (selection-sort lis) 
    (if (null? lis) 
        '()
        (cons (select-min lis) 
            (selection-sort (delete-elem (select-min lis) lis))
        )
    ) 
)
