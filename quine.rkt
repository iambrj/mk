#lang racket
(require "mk.rkt")

(define anyo
  (lambda (g)
    (conde
      (g)
      ((anyo g)))))

(define (foo x)
  (run x (q)
    (let
      ([nevero (anyo (== #t #f))])
      (conde
        [(== 1 q)]
        [nevero]
        [(conde
           ([== 2 q])
           (nevero)
           ([== 3 q]))]))))
