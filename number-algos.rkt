(require "numbers.rkt")

(run 1 (g) (gcdo (build-num 20) (build-num 30) g))

; (run 1 (g) (gcdo (build-num 1000) g (build-num 500)))

(run 1 (q) (list-refo (list 'a 'b 'c) (build-num 2) q))

(run 1 (q) (list-refo (list 'a 'b 'c) (build-num 3) q))

