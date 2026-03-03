#lang racket
; 3. Selection sort
; 
; a. Write the function delete-elem which accepts two arguments: the
; element elem and the list lis. The function returns a new list, in which
; the first occurrence of elem has been removed from lis
; 
; Test Cases:
; (delete-elem 1 '(1 2 3 4))
; '(2 3 4)
; (delete-elem 3 '(1 2 3 4 3))
; '(1 2 4 3)


(define (delete-elem elem lis)
    (cond 
        [(null? lis) empty]
        [(equal? elem (car lis)) (cdr lis)]
        [else (cons (car lis) 
            (delete-elem elem (cdr lis)))]
    )
)

; b. Write the function select-min which returns the smallest element from
; the parameter list lis. Hint: Use a let statement to find the smallest
; element in the tail and compare it to the head.
; 
; Test Cases:
; (select-min '(4 2 1 5 7))
; 1


(define (select-min lis)
    (if (null? lis) empty
    (if (null? (cdr lis)) 
        (car lis)
        (let ([min (select-min (cdr lis))])
        (if (< min (car lis)) 
            min 
            (car lis)
        ))
    ))
)

; c. Write the function selection-sort which accepts a list lis as an
; argument and returns a new list, a sorted version of lis (in ascending
; order). The function must use both delete-item from a) and select-
; min from b).
; 
; Test Cases:
; (selection-sort '(3 5 2 1 2 5 4 6))
; (1 2 2 3 4 5 5 6)

(define (selection-sort lis) 
    (if (null? lis) 
        empty
        (cons (select-min lis) 
            (selection-sort (delete-elem (select-min lis) lis))
        )
    ) 
)