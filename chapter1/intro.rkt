#lang sicp

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
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

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
  (if (prime? n)
    (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes start end)
  (if (even? start)
      (search-for-primes (+ start 1) end)
      (cond ((> start end) (newline) (display "Done!"))
            (else (timed-prime-test start) (search-for-primes (+ start 2) end)))))
