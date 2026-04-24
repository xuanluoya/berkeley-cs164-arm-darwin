; 函数也是值
(define (f g) (g 2))
(define (mul2 x) (+ x x))

; Map
(define (range lo hi)
    (if (< lo hi)
        (cons lo (range (add1 lo) hi))
        #f))

(define (map f l)
    (if (not l) l
        (cons (f (car l)) (map f (cdr l)))))
(define (g x) (+ x 1))

(do
    (display (f mul2))
    (newline)
    (let ((newname mul2)) (display (newname 5)))
    (newline)
    (display (map g (range 0 4)))
    (newline)
    (display (f (lambda (x) (+ x x))))
    (newline)
    (display (map (lambda (x) (+ x 1)) (range 0 4))))
