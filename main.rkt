#lang racket

;;
;; sudoku solver
;;

(module+ test
  (require rackunit))

#| (require "hello.rkt") |#

#| (provide hello-world) |#

(define (horizontal d x y)
  (list-ref d y))

(define (vertical d x y)
  (let loop ((i 0) (lst '()))
    (if (> i 8) 
        lst
        (loop (+ i 1)
              (append lst
                (list (list-ref
                        (list-ref d i) x)))))))

(define (square d x y)
  #f)

(define (lack lst)
  #f)

(define (product h v s)
  #f)

(module+ test
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.

  #| (check-equal? (+ 2 2) 4) |#
  (let ((data '((0 1 2 3 4 5 6 7 8)
                (1 2 3 4 5 6 7 8 9)
                (2 3 4 5 6 7 8 9 0)
                (3 4 5 6 7 8 9 0 1)
                (4 5 6 7 8 9 0 1 2)
                (5 6 7 8 9 0 1 2 3)
                (6 7 8 9 0 1 2 3 4)
                (7 8 9 0 1 2 3 4 5)
                (8 9 0 1 2 3 4 5 6))))

    (check-equal? (horizontal data 0 0)
                  '(0 1 2 3 4 5 6 7 8))
    (check-equal? (horizontal data 1 0)
                  '(0 1 2 3 4 5 6 7 8))
    (check-equal? (horizontal data 8 0)
                  '(0 1 2 3 4 5 6 7 8))

    (check-equal? (horizontal data 0 1)
                  '(1 2 3 4 5 6 7 8 9))
    (check-equal? (horizontal data 0 2)
                  '(2 3 4 5 6 7 8 9 0))
    (check-equal? (horizontal data 0 8)
                  '(8 9 0 1 2 3 4 5 6))

    (check-equal? (vertical data 0 0)
                  '(0 1 2 3 4 5 6 7 8))
    (check-equal? (vertical data 0 1)
                  '(0 1 2 3 4 5 6 7 8))
    (check-equal? (vertical data 0 8)
                  '(0 1 2 3 4 5 6 7 8))

    (check-equal? (vertical data 1 0)
                  '(1 2 3 4 5 6 7 8 9))
    (check-equal? (vertical data 2 0)
                  '(2 3 4 5 6 7 8 9 0))
    (check-equal? (vertical data 8 0)
                  '(8 9 0 1 2 3 4 5 6))




    )
)
(module+ main
  ;; (Optional) main submodule. Put code here if you need it to be executed when
  ;; this file is run using DrRacket or the `racket` executable.  The code here
  ;; does not run when this file is required by another module. Documentation:
  ;; http://docs.racket-lang.org/guide/Module_Syntax.html#%28part._main-and-test%29

  (require racket/cmdline)
  (define who (box "world"))
  (command-line
    #:program "my-program"
    #:once-each
    [("-n" "--name") name "Who to say hello to" (set-box! who name)]
    #:args ()
    (printf "hello ~a~n" (unbox who)))
)

