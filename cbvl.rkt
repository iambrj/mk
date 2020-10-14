#lang racket
(require "mk.rkt")

(define lookupo
  (lambda (x env t)
    (fresh (y v resto)
      (== `((,y . ,v) . ,resto) env)
      (conde
        [(== x y) (== v t)]
        [(=/= x y) (lookupo x resto t)]))))

(define eval-expo
  (lambda (expr env val)
    (conde
      [(== expr `(quote ,val))
         (absento 'closure val)
         (not-in-envo 'quote env)]
      [(fresh (x body)
         (== `(lambda (,x) ,body) expr)
         (symbolo x)
         (== `(closure ,x ,body ,env) val)
         (not-in-envo 'lambda env))]
      [(symbolo expr) (lookupo expr env val)]
      [(fresh (rator rand x body env2 a)
         (== `(,rator ,rand) expr)
         (eval-expo rator env `(closure ,x ,body ,env2))
         (eval-expo rand env a)
         (eval-expo body `((,x . ,a) . ,env2) val))]
      [(fresh (e*)
         (== `(list . ,e*) expr)
         (not-in-envo 'list env)
         (absento 'closure e*)
         (mapo (lambda (e v) (eval-expo e env v)) e* val))]; map takes GOAL not FUNCTION
         ;(list-helper e* env val))]; list
      ;[(fresh (v)
      ;   (== `(quote ,v) expr)
      ;   (not-in-envo 'quote env)
      ;   (absento 'closure v)
      ;   (== v val))]
      ))) ; quote 
(define list-helper
  (lambda (e* env v*)
    (conde
      [(== e* '()) (== v* '())]
      [(fresh (e e-rest v v-rest)
         (== `(,e . ,e-rest) e*)
         (== `(,v . ,v-rest) v*)
         (eval-expo e env v)
         (list-helper e-rest env v-rest))])))

(define mapo
  (lambda (g l v)
    (conde
      [(== l '()) (== v '())]
      [(fresh (h tail vh vtail)
         (== `(,h . ,tail) l)
         (== `(,vh . ,vtail) v)
         (g h vh); g is a _g_oal!
         (mapo g tail vtail))])))

; g l v = eval-expo l env v

(define not-in-envo
  (lambda (x env)
    (conde
      [(== env '())]
      [(fresh (y v resto)
         (== `((,y . ,v) . ,resto) env)
         (=/= x y)
         (not-in-envo x resto))])))

(run 1 (q)          
  (eval-expo        
    '((lambda (x) (list x (list (quote quote) x))) (quote (lambda (x) (list x (list (quote quote) x)))))
    '() q))         

; Set up Vim for Racket!
