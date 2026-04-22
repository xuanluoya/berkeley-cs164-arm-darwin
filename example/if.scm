(display (if (cdr (cons #t #f)) 2 (+ (let ((x 2)) (add1 x)) 2)))
