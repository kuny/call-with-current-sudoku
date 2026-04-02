#lang racket

;;
;; sudoku solver
;;

(module+ test
  (require rackunit))

#| (require "hello.rkt") |#

#| (provide hello-world) |#

(define (matrix-ref d x y)
  (list-ref
    (list-ref d y) x))

(define (horizontal d x y)
  (list-ref d y))

(define (vertical d x y)
  (let loop ((i 0) (lst '()))
    (if (> i 8) 
        lst
        (loop (+ i 1)
              (append lst
                      (list (matrix-ref d x i)))))))

(define (block-idxs x)
  (let ((a (quotient x 3)))
    (cond ((= a 0) '(0 1 2))
          ((= a 1) '(3 4 5))
          (else '(6 7 8)))))

(define (atom? x)
  (not (pair? x)))

(define (flatten x)
  (let rec ((x x) (acc '()))
    (cond ((null? x) acc)
          ((atom? x) (cons x acc))
          (else (rec
                  (car x)
                  (rec (cdr x) acc))))))

(define (map-product f xs yx)
  (map (lambda (x)
         (map (lambda (y)
                (f x y))
              yx))
         xs))

(define (square d x y)
  (define (matrix-ref+ a b)
    (matrix-ref d a b))
  (let ((xs (block-idxs x))
        (ys (block-idxs y)))
    (flatten (map-product matrix-ref+ xs ys))))

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

    (check-equal? (matrix-ref data 0 0) 0)
    (check-equal? (matrix-ref data 1 0) 1)
    (check-equal? (matrix-ref data 4 5) 9)
    (check-equal? (matrix-ref data 8 8) 6)

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

    (check-equal? (block-idxs 0) '(0 1 2))
    (check-equal? (block-idxs 1) '(0 1 2))
    (check-equal? (block-idxs 2) '(0 1 2))

    (check-equal? (block-idxs 3) '(3 4 5))
    (check-equal? (block-idxs 4) '(3 4 5))
    (check-equal? (block-idxs 5) '(3 4 5))

    (check-equal? (block-idxs 6) '(6 7 8))
    (check-equal? (block-idxs 7) '(6 7 8))
    (check-equal? (block-idxs 8) '(6 7 8))

    (check-equal? (square data 0 0) '(0 1 2 1 2 3 2 3 4))
    (check-equal? (square data 1 1) '(0 1 2 1 2 3 2 3 4))
    (check-equal? (square data 4 4) '(6 7 8 7 8 9 8 9 0))
    (check-equal? (square data 4 7) '(9 0 1 0 1 2 1 2 3))
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

