#|
  在 ref_sum 中，递归调用并不是函数的最后一步
  没有尾递归优化，程序疯狂调用自己，成功栈溢出
    - 为了计算 (+ n (ref_sum (dec n)))，程序必须先去算 (ref_sum (dec n))
    - 在等待子任务返回结果时，当前的栈帧（Stack Frame）必须保留，因为它还得记住那个 n，好等会儿回来做加法
    - 如果你给的 n 是 1,000,000，计算机会开辟一百万个栈空间，最终导致 Stack Overflow
|#

#|
(define (ref_sum n)
  (if (zero? n)
    n
    (+ n (ref_sum (dec n)))))
|#

#|
  在 sum 函数中，递归调用是整个函数的最后一件动作
    - 当程序运行到 (sum (dec n) (+ n total)) 时，它已经计算出了新的 total。当前函数已经没有什么“家当”需要留守了（不需要再做任何加法或其他操作）
    - 编译器发现这一点后，会直接复用当前的栈帧，而不是开辟新的。它本质上把递归转化成了一个 while 循环
    - 所有的中间状态（累加的和）都通过 total 参数显式传递了
|#
(define (sum n total)
  (if (zero? n)
    total
    (sum (dec n) (+ n total))))

(do
  #|
    (print (ref_sum (read_num)))
    (newline)
  |#
  (print (sum (read_num) 0)))
