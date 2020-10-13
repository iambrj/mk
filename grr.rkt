#lang racket

(require "mk.rkt")

(define nullo
  (lambda (x)
    (== x '())))

(define conso
  (lambda (x xs out)
    (== (cons x xs) out)))

(define caro
  (lambda (l out)
    (fresh (a)
      (conso out a l))))

(define cdro
  (lambda (l out)
    (fresh (a)
      (conso a out l))))

(define pairo
  (lambda (p)
    (fresh (x y)
      (conso x y p))))

(define listo
  (lambda (l)
    (conde
      [(nullo l)]
      [(pairo l)
       (fresh (a d)
         (conso a d l)
         (listo d))])))

(define membero
  (lambda (x l)
    (conde
      [(nullo l) (== #t #f)]
      [(fresh (a d)
         (conso a d l)
         (conde
           [(== x a)]
           [(membero x d)]))])))

(define appendo
  (lambda (l1 l2 l)
    (conde
      [(nullo l1) (== l2 l)]
      [(fresh (a d res)
         (conso a d l1)
         (appendo d l2 res)
         (conso a res l))])))

(define my-numbero
  (lambda (x)
    (== (number? x) #t)))
