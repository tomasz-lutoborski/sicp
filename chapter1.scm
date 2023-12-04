(define size 2)

;;(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

(define (square x) (* x x))

(define (sum-of-squares x y z)
  (cond
    [(and (< x y) (< x z)) (+ (square y) (square z))]
    [(and (< y x) (< y z)) (+ (square x) (square z))]
    [(and (< z x) (< z y)) (+ (square x) (square y))]))

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(define (sqrt-iter guess x)
  (if (good-enough-2? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (good-enough-2? guess x)
  (< (abs (- (improve guess x) guess))
     (* 0.01 guess)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (cube-root x)
  (define (cube-root-iter guess x)
    (if (good-enough-cube? guess x)
      guess
      (cube-root-iter (improve-cube guess x) x)))
  (cube-root-iter 1.0 x))

(define (improve-cube guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (good-enough-cube? guess x)
  (< (abs (- (improve-cube guess x) guess))
     (* 0.01 guess)))

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(define (factorial-iter product counter max-count)
  (if (> counter max-count)
      product
      (factorial-iter (* counter product)
                      (+ counter 1)
                      max-count)))

(define (factorial2 n)
  (factorial-iter 1 1 n))

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                  (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination
                           kinds-of-coins))
                    kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(define (f-recursive n)
  (cond ((and (< n 3) (>= n 0) n))
        (else (+ (f-recursive (- n 1))
                 (* 2 (f-recursive (- n 2)))
                 (* 3 (f-recursive (- n 3)))))))


(define (f-iter n)
  (define (f-i a b c count)
    (cond ((< n 3) n)
          ((<= count 0) a)
          (else (f-i (+ a (* 2 b) (* 3 c)) a b (- count 1)))))
  (f-i 2 1 0 (- n 2)))

(define (pascal r c)
   (if (or (= c 1) (= c r))
       1
       (+ (pascal (- r 1) (- c 1)) (pascal (- r 1) c))))

(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (n - 1)))))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((= (remainder n 2) 0) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (expt* b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
        (- counter 1)
        (* b product))))

(define (fast-expt-iter b n a)
  (cond ((= n 0) a)
        ((= (remainder n 2) 0) (fast-expt-iter (square b) (/ n 2) a))
        (else (fast-expt-iter b (- n 1) (* b a)))))

(define (mult a b)
  (if (= b 0)
      0
      (+ a (mult a (- b 1)))))

(define (fast-mult a b)
  (cond ((= b 0) 0)
        ((= (remainder b 2) 0) (* 2 (fast-mult a (/ b 2))))
        (else (+ a (fast-mult a (- b 1))))))

(define (fast-mult-iter a b product)
  (cond ((= b 0) product)
        ((= (remainder b 2) 0) (fast-mult-iter (* 2 a) (/ b 2) product))
        (else (fast-mult-iter a (- b 1) (+ a product)))))

(define (fib* n)
  (fib-iter* 1 0 0 1 n))
(define (fib-iter* a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter* a
                   b
                   (+ (* p p) (* q q))
                   (+ (* 2 p q) (* q q))
                   (/ count 2)))
        (else (fib-iter* (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (define (next divisor)
    (if (= divisor 2)
        3
        (+ divisor 2)))
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
          (remainder
            (* base (expmod base (- exp 1) m))
            m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n)
    (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes start end)
  (if (even? start)
      (search-for-primes (+ start 1) end)
      (cond ((> start end) (newline) (display "Done!"))
            (else (timed-prime-test start) (search-for-primes (+ start 2) end)))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod-checked a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (expmod-checked base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder-square-checked (expmod-checked base (/ exp 2) m) m))
        (else
         (remainder (* base (expmod-checked base (- exp 1) m))
                    m))))

(define (remainder-square-checked a m)
  (if (not (or (= a 1) (= a (- m 1))))
      0
      (remainder (square a) m)))

(define (miller-rabin-prime?  n times)
  (cond ((= times 0) #t)
        ((miller-rabin-test n)
         (miller-rabin-prime? n (- times 1)))
        (else #f)))

(define (cube x) (* x x x))

(define (sum-integers a b)
  (if (> a b)
    0
    (+ a (sum-integers (+ a 1) b))))

(define (pi-sum a b)
  (if (> a b)
    0
    (+ (/ 1.0 (* a (+ a 2)))
       (pi-sum (+ a 4) b))))

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (inc n) (+ n 1))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (integral f a b dx)
  (* (sum f (+ a (/ dx 2.0)) (lambda (x) (+ x dx)) b)
    dx))

(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (y k)
    (f (+ a (* k h))))
  (define (term k)
    (cond ((or (= k 0) (= k n)) (y k))
          ((even? k) (* 2 (y k)))
          (else (* 4 (y k)))))
  (* (/ h 3) (sum term 0 inc n)))

(define (sum* term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (factorial* n)
  (product identity 1 inc n))

(define (product* term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (accumulate* combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (filter-accumulate combiner null-value predicate term a next b)
  (if (> a b)
      null-value
      (if (predicate a)
          (combiner (term a)
                    (filter-accumulate combiner null-value predicate term (next a) next b))
          (filter-accumulate combiner null-value predicate term (next a) next b))))

(define (divisible? a b) (= (remainder a b) 0))

(define divisible-by-3? (lambda (x) (divisible? x 3)))

(define (relatively-prime? a b)
  (= (gcd a b) 1))

(define (product-of-relatively-prime n)
  (filter-accumulate * 1
                     (lambda (i) (relatively-prime? i n))
                     identity
                     1
                     inc
                     n))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
      midpoint
      (let ((test-value (f midpoint)))
        (cond ((positive? test-value) (search f neg-point midpoint))
              ((negative? test-value) (search f midpoint pos-point))
              (else midpoint))))))

(define (close-enough? x y) (< (abs (- x y)) 0.001))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value)) (search f a b))
          ((and (negative? b-value) (positive? a-value)) (search f b a))
          (else
            (error "Values are not of opposite sign" a b)))))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
      tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))


(define (cont-frac n d k)
  (define (iter i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (iter (+ i 1))))))
  (iter 1))

(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter k 0))

(define (tan-cf x k)
  (if (= k 0)
      x
      (+ x (cont-frac-iter (lambda (i) (* x x))
                           (lambda (i) (- (* i 2) 1))
                           k))))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt-v2 x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define dx 0.00001)

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x))
                 dx)))

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt-v3 x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))

(define (sqrt-v4 x)
  (fixed-point-of-transform (lambda (y) (- x (square y)))
                            newton-transform
                            1.0))

(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))

(define (double f)
  (lambda (x) (f (f x))))

(define (compose f g)
  (lambda (x) (f (g x))))


(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx))
                    (f x)
                    (f (+ x dx)))
                 3)))

(define (nth-root n)
  (lambda (x) (fixed-point (repeated (average-damp (lambda (y) (/ x (expt y (- n 1))))) 3) 1.0)))
