(define (f g) (g 2))
(define (loop n) (if (zero? n) 0 (loop (sub1 n))))

(do
    (let ((y 3)) (display (f (lambda (x) (+ x y)))))
    (newline)
    ; TCO
    (display (loop 10000)))
