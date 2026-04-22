(do
    (display (let ((x 1)) (add1 x)))
    (newline)
    (display (+ 1 2))
    (newline)
    (if #t (display (cons 1 2)) 2)
    (newline)
    (display (car (cons #f #t))))
