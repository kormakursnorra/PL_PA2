(declare (unit project2))
(import (chicken sort))

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
    [(eq? i 1) (cons elem lis)]
    [else(cons (car lis) 
      (insert-at (cdr lis) (- i 1) elem))
    ]
  )
)


; d. sequence
(define (seq_helper n i)
  (cond
    [(eq? n i) (cons i '())]
    [else (cons i (seq_helper n (+ i 1)))]
  )
) 

(define (sequence n)
  (if (eq? n 0) 
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

; 2. Higher-order functions 
 
; a. make-linear
(define (make-linear a b)
  (lambda (x) (+ (* a x) b)))


; b. add-linear
(define (add-linear f g)
  (lambda (x) (+ (f x) ( g x))))


; c. make-linear-list 
(define (make-linear-list a b) 
  (lambda (lis)
    (map (make-linear a b) lis)))


; 3. Selection sort

; a. delete-elem 
(define (delete-elem elem lis)
    (cond 
        [(null? lis) '()]
        [(eq? elem (car lis)) (cdr lis)]
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

; 4. stack

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
