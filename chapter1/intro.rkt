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
