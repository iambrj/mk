#lang racket

(require "main.rkt")

(include "numbers.scm")

(define demolish-num
  (lambda (b e s)
    (cond
      [(equal? b '()) s]
      [else (demolish-num (rest b) (+ e 1) (+ s (* (expt 2 e) (first b))))])))

(define (factorial n f)
  (cond
    [(zero? n) f]
    [else (factorial (- n 1) (* n f))]))

(define (factorialo n f)
  (conde
    [(zeroo n) (== f (build-num 1))]
    [(<o (build-num 0) n)
     (fresh (n-1 f-1)
       (minuso n (build-num 1) n-1)
       (*o n f-1 f)
       (factorialo n-1 f-1))]))

(define (factorialo-out n f out)
  (conde
    [(zeroo n) (== f out)]
    [(fresh (n-1 f-1)
       (minuso n (build-num 1) n-1)
       (*o n f f-1)
       (factorialo-out n-1 f-1 out))]))

; Running it backwards is *extremely* slow (even for 1000 q 500)
; Make sure NO branches don't overlap!
; Can't assume other branches failed, no order gauranteed! Add extra constraints
; to be safe. cond != conde
(define (gcdo a b g)
  (conde
    [(zeroo b) (== g a)]
    [(zeroo a) (poso b) (== g b)]
    [(poso a)
     (poso b)
     (fresh (q r)
       (conde
         [(<o a b) (/o b a q r) (gcdo a r g)]
         [(<=o b a) (/o a b q r) (gcdo b r g)]))]))

; a * x + b * y = g
; returns (list x y g)
(define (xgcd a b)
  (cond
    [(zero? b) (list 1 0 a)]
    [else
      (let ([res (xgcd b (modulo a b))])
        (list (second res) (- (first res) (* (second res) (floor (/ a b)))) (third res)))]))

; How to get non-negative solutions?
; Interesting query,
; 1. Dynamic reordering
; 2. Static analysis
; 3. Bounds on number sizes
(define (xgcdo a b x y g)
  (conde
    [(zeroo b)
     (== g a)
     (== x (build-num 1))
     (== y (build-num 0))]
    [(poso b)
     (fresh (q r x1 t)
       (/o a b q r)
       (xgcdo b r x1 x g)
       (*o q x t)
       (minuso x1 t y))]))

(define (list-refo lst p x)
  (fresh (a d)
    (== `(,a . ,d) lst)
    (conde
      [(zeroo p) (== x a)]
      [(fresh (p-1)
         (minuso p (build-num 1) p-1)
         (list-refo d p-1 x))])))

(define (lengtho lst x)
  (conde
    [(== lst '()) (== x (build-num 0))]
    [(fresh (a d xs)
       (== `(,a . ,d) lst)
       (lengtho d xs)
       (pluso xs (build-num 1) x))]))

(define (bin-searcho cmp l u lst x)
  (conde 
    [(<o u l) (== #t #f)]
    [(== l u) (list-refo lst l x)]
    [(<o l u)
     (fresh (s m r)
      (pluso l u s)
      (/o s (build-num 2) m r)
      (conde
        [(list-refo lst m x)]
        [(fresh (mval m+1)
           (list-refo lst m mval)
           (pluso m (build-num 1) m+1)
           (conde
             [(cmp x mval)
              (bin-searcho cmp l m lst x)]
             [(bin-searcho cmp m+1 u lst x)]))]))]))

(define (bin-search-iter cmp l u lst x)
  (cond
    [(<= (length lst) l) #f]
    [(equal? l u) (equal? (list-ref lst l) x)]
    [else
      (let ([mid (quotient (+ l u) 2)])
        (cond
          [(equal? (list-ref lst mid) x) #t]
          [else
            (cond
              [(cmp x (list-ref lst mid)) (bin-search-iter cmp l mid lst x)]
              [(not (cmp x (list-ref lst mid))) (bin-search-iter cmp (+ 1 mid) u lst x)]
              [else #f])]))]))

(define (bin-search cmp lst x)
  (bin-search-iter cmp 0 (length lst) lst x))

(define (sqrt-floor n s e r)
  (fresh (m sum x)  ; _ would be useful here!
    (pluso s e sum)
    (/o sum (build-num 2) m x)
    (conde
      [(list-refo )])))

(provide (except-out (all-defined-out) appendo))
