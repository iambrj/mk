#lang racket

(require "mk.rkt")

(define successful (== #t #t))
(define unsuccessful (== #t #f))

(define caro
  (lambda (p a)
    (fresh (d)
      (conso a d p))))

(define cdro
  (lambda (p a)
    (fresh (d)
      (conso d a p))))

(define conso
  (lambda (x xs out)
    (== (cons x xs) out)))

(define nullo
  (lambda (l)
    (== '() l)))

(define zeroo
  (lambda (z)
    (== z 0)))

(define eqo
  (lambda (x y)
    (== x y)))

(define pairo
  (lambda (p)
    (fresh (x y)
      (conso x y p))))

(define listo
  (lambda (l)
    (conde
      [(nullo l) successful]
      [(pairo l)
       (fresh (d)
         (cdro l d)
         (listo d))]
      [unsuccessful])))

(define lolo
  (lambda (l)
    (conde
      [(nullo l) successful]
      [(fresh (a)
         (caro l a)
         (listo a))
       (fresh (d)
         (cdro l d)
         (lolo d))]
      [unsuccessful])))

(define twinso
  (lambda (l)
    (fresh (x)
      (== (list x x) l))))
; loto : list -> succeeds only if (x x)
(define loto
  (lambda (l)
    (conde
      [(nullo l) successful]
      [(fresh (x xs)
         (caro l x)
         (cdro l xs)
         (twinso x)
         (loto xs))]
      [unsuccessful])))

(define listofo
  (lambda (predo l)
    (conde
      [(nullo l) successful]
      [(fresh (x xs)
        (caro l x)
        (cdro l xs)
        (predo x)
        (listofo predo xs))]
      [unsuccessful])))

(define car-eq
  (lambda (l x)
    (caro l x)))

(define memo
  (lambda (x l out)
    (conde
      [(car-eq l x) (== out l)]
      [(fresh (fs)
         (cdro l fs)
         (memo x fs out))])))

(define rembero
  (lambda (x l out)
    (conde
      [(nullo l) (== '() out)]
      [(car-eq l x) (cdro l out)]
      [(fresh (f fs res)
         (conso f fs l)
         (rembero x fs res)
         (conso f res out))])))

(define surpriseo
  (lambda (s)
    (rembero s '(a b c) '(a b c))))

(define appendo
  (lambda (l1 l2 v)
    (conde
      [(nullo l1) (== v l2)]
      [(fresh (a d r)
         (== (cons a d) l1)
         (appendo d l2 r)
         (== v (cons a r)))])))

; miniKanren
; ==, conde, fresh
; '(1 2 3)
(define rappend
  (lambda (l1 l2 res)
    (conde
      [(== l1 '()) (== res l2)]
      [(fresh (a d val)
         (== `(,a . ,d) l2); a = first, d = rest
         (rappend d l2 val)
         (== res (cons a val)) ; cons 1 (2 3) = (1 2 3)
         )])))

((lambda (x) (list x (list (quote quote) x))) (quote (lambda (x) (list x (list (quote quote) x)))))
