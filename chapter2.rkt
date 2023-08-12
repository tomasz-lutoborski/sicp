#lang racket

(define (square x) (* x x))

(define (gcd x y)
  (cond ((= x 0) y)
        (else (gcd (remainder y x) x))))
  
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
        (make-rat (- (* (numer x) (denom y))
                     (* (numer y) (denom x)))
                  (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
  
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (let ((num (/ n g))
          (denom (/ d g)))
      (cond ((and (< num 0) (< d 0)) (cons (abs num) (abs denom)))
            ((and (> num 0) (< denom 0)) (cons (- num) (abs denom)))
            (else (cons num denom))))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-point x y)
  (cons x y))

(define (x-point point) (car point))

(define (y-point point) (cdr point))

(define (make-segment start end)
  (cons start end))

(define (start-segment seg) (car seg))

(define (end-segment seg) (cdr seg))

(define (midpoint-segment seg)
  (make-point (/ (+ (x-point (start-segment seg)) (x-point (end-segment seg))) 2)
              (/ (+ (y-point (start-segment seg)) (y-point (end-segment seg))) 2)))

(define (make-rect left-bot right-top)
  (cons left-bot right-top))

(define (lb-cor-rect rect) (car rect))

(define (rt-cor-rect rect) (cdr rect))

(define (area-rect rect)
  (* (- (x-point (rt-cor-rect rect)) (x-point (lb-cor-rect rect)))
     (- (y-point (rt-cor-rect rect)) (y-point (lb-cor-rect rect)))))

(define (perim-rect rect)
    (+ (* 2 (- (x-point (rt-cor-rect rect)) (x-point (lb-cor-rect rect))))
       (* 2 (- (y-point (rt-cor-rect rect)) (y-point (lb-cor-rect rect))))))
  
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if
   (or (= (- (lower-bound x) (upper-bound x)))
       (= (- (lower-bound y) (upper-bound y))))
   (error "divison by interval of span 0")
   (mul-interval
    x
    (make-interval (/ 1.0 (upper-bound y))
                   (/ 1.0 (lower-bound y))))))

(define (make-interval a b) (cons a b))

(define (upper-bound interval) (cdr interval))
(define (lower-bound interval) (car interval))

(define (sub-interval x y) 
  (make-interval (- (lower-bound x) (upper-bound y)) 
                 (- (upper-bound x) (lower-bound y)))) 

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c percent)
  (let ((width (* c (/ percent 100))))
    (make-interval (- c width) (+ c width))))

(define (percent-interval interval)
  (let ((width (/ (- (upper-bound interval) (lower-bound interval)) 2))
        (center (/ (+ (upper-bound interval) (lower-bound interval)) 2)))
    (* (/ width center) 100)))
    
(define (last-pair items)
  (if (null? (cdr items))
      (car items)
      (last-pair (cdr items))))

(define (reverse items)
  (if (null? (cdr items))
      (car items)
      (cons (reverse (cdr items)) (car items))))
      
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination
                 coin-values))
            (cc (- amount
                   (first-denomination
                    coin-values))
                coin-values)))))

(define (first-denomination coin-values)
  (car coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (no-more? coin-values)
  (null? coin-values))

(define (same-parity fst . xs)       
  (if (odd? fst) (cons fst (filter odd? xs)) (cons fst (filter even? xs))))

(define (filter fn items)
  (if (null? (cdr items))
      (if (fn (car items)) (car items) '())
      (cons (if (fn (car items)) (car items) '()) (filter fn (cdr items))))) 

(define (square-list items)
  (if (null? items)
      nil
      (cons (expt (car items) 2) (square-list (cdr items)))))

(define (square-list-2 items)
  (map (lambda (x) (expt x 2)) items))

(define (square-list-iter items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer (square (car things))))))
  (iter items nil))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))
  
(define (deep-reverse lst) 
  (cond ((null? lst) nil) 
        ((pair? (car lst)) 
         (append 
          (deep-reverse (cdr lst)) 
          (list (deep-reverse (car lst))))) 
        (else 
         (append 
          (deep-reverse (cdr lst)) 
          (list (car lst)))))) 

(define (fringe lst)
  (cond ((null? lst) '())
        ((pair? (car lst)) (append (fringe (car lst)) (fringe (cdr lst))))
        (else (cons (car lst) (fringe (cdr lst))))))

(define (make-mobile left right) 
   (list left right)) 
(define (left-branch mobile) 
   (car mobile)) 
(define (right-branch mobile) 
   (car (cdr mobile))) 
  
(define (make-branch length structure) 
   (list length structure)) 
  
(define (branch-length branch) 
   (car branch)) 
(define (branch-structure branch) 
   (car (cdr branch))) 

 (define (square-tree tree) 
   (define nil '()) 
   (cond ((null? tree) nil) 
         ((not (pair? tree)) (square tree)) 
         (else (cons (sq-tree-2 (car tree)) 
                     (sq-tree-2 (cdr tree)))))) 

 (define (tree-map proc tree) 
   (cond ((null? tree) nil) 
         ((pair? tree)  
          (cons  
           (tree-map proc (car tree))  
           (tree-map proc (cdr tree)))) 
         (else (proc tree)))) 
  
 (define (count-leaves-recursive t) 
   (accumulate + 0 (map (lambda (node) 
                          (if (pair? node) 
                              (count-leaves-recursive node) 
                              1)) 
                        t))) 

(define (accumulate ops initial sequence) 
 (if (null? sequence) 
     initial 
     (ops (car sequence) 
         (accumulate ops initial (cdr sequence))))) 

(define (accumulate-n op init seqs) 
 (if (null? (car seqs)) 
     () 
     (cons (accumulate op init (map car seqs)) 
           (accumulate-n op init (map cdr seqs))))) 

(define (dot-product v w) 
 (accumulate + 0 (map * v w))) 

(define (matrix-*-vector m v) 
 (map (lambda (w) 
        (dot-product v w)) m)) 

(define (transpose m) 
 (accumulate-n cons () m)) 

(define (matrix-*-matrix m n) 
 (let ((cols (transpose n))) 
   (map (lambda (v) (matrix-*-vector cols v)) m)))  

(define (fold-right op initial sequence) 
 (if (null? sequence) 
     initial 
     (op (car sequence) 
         (fold-right op initial (cdr sequence))))) 


(define (fold-left op initial sequence) 
 (define (iter result rest) 
   (if (null? rest) 
       result 
       (iter (op result (car rest)) 
             (cdr rest)))) 
 (iter initial sequence)) 
