#lang racket

;;
;; sudoku solver
;;

(module+ test
  (require rackunit))

#| (require "hello.rkt") |#

#| (provide hello-world) |#

(define-syntax x-
  (syntax-rules ()
    [(x- x)
     (first x)]))

(define-syntax y-
  (syntax-rules ()
    [(y- x)
     (second x)]))

(define (matrix-ref d x y)
  (list-ref
    (list-ref d y) x))

(define (horizontal d xy)
  (let  ((x (x- xy))
         (y (y- xy)))
    (list-ref d y)))

(define (vertical d xy)
  (let loop ((i 0) (lst '()))
    (if (> i 8) 
        lst
        (loop (+ i 1)
              (append lst
                      (list (matrix-ref d (x- xy) i)))))))

(define (block-idxs x)
  (let ((a (quotient x 3)))
    (cond ((= a 0) '(0 1 2))
          ((= a 1) '(3 4 5))
          (else '(6 7 8)))))

(define (map-product f xs yx)
  (map (lambda (x)
         (map (lambda (y)
                (f x y))
              yx))
         xs))

(define (square d xy)
  (let* ((x (x- xy))
         (y (y- xy))
         (xs (block-idxs x))
         (ys (block-idxs y)))
    (flatten (map-product (lambda (a b)
                            (matrix-ref d a b))
                            xs ys))))

(define (include? lst x)
  (let loop ((l lst))
    (cond ((null? l) #f)
          ((equal? (car l) x) #t)
          (else
            (loop (cdr l))))))

(define (predict lst)
  (let loop ((xs '(1 2 3 4 5 6 7 8 9)) (ret '()))
    (cond ((null? xs) ret)
          ((not (include? lst (car xs)))
           (loop (cdr xs) (append ret
                                  (list (car xs)))))
          (else 
            (loop (cdr xs) ret)))))

(define (product seta setb setc)
  (define (product-internal x y)
    (let loop ((l x) (ret '()))
      (cond ((null? l) ret)
            ((include? y (car l))
             (loop (cdr l)
                   (append ret (list (car l)))))
            (else
              (loop (cdr l) ret)))))
  (let ((ab (product-internal seta setb))
        (ac (product-internal seta setc)))
    (product-internal ab ac)))

(define (replace-nth n nw lst)
  (cond ((null? lst) '())
        ((zero? n) (cons nw (cdr lst)))
        (else (cons (car lst)
                    (replace-nth (- n 1) nw (cdr lst))))))

(define (replace-xy xy nw d)
  (let ((x (x- xy))
        (y (y- xy)))
  (cond ((null? d) '())
        ((zero? y)
         (append (list (replace-nth x nw (car d)))
                 (cdr d)))
        (else
          (append (list (car d))
                  (replace-xy (list x (- y 1)) nw (cdr d)))))))

(define (find-empty y l)
  (let loop ((x l) (i 0) (ret '()))
    (cond ((null? x) ret)
          ((zero? (car x))
           (loop (cdr x)
                 (+ i 1)
                 (append ret (list (list i y)))))
          (else
            (loop (cdr x) (+ i 1) ret)))))


(define (find-empties d)
  (let loop ((x d) (i 0) (ret '()))
    (cond ((null? x) ret)
          (else
            (loop (cdr x)
                  (+ i 1)
                  (append ret
                          (find-empty i (car x))))))))

(define (complete? d)
  (let ((empties (find-empties d)))
    (if (null? empties) #t #f)))

(define (search-solution board xy)
  (let* ((hpredicts (predict (horizontal board xy)))
         (vpredicts (predict (vertical board xy)))
         (spredicts (predict (square board xy))))
    (product hpredicts vpredicts spredicts)))

(define (solve board)
  (define (solve-internal board xy)
    (let ((s (search-solution board xy)))
      (if (or (null? s)
              (> (length s) 1))
        board
        (replace-xy xy (car s) board))))
  (let loop ((empties (find-empties board)) (bd board))
    (cond ((null? empties) bd)
          (else
            (loop (cdr empties)
                  (solve-internal bd (car empties)))))))

(define prev '())

(define (solver board)
  (cond ((or (complete? board)
             (equal? prev board)) board)
        (else
          (set! prev board)
          (solver (solve board)))))

(define (result board)
  (let loop ((rows board))
    (if (null? rows) 
      (newline)
      (begin
        (displayln (car rows))
        (loop (cdr rows))))))

(module+ test

  (let ((data '((0 1 2 3 4 5 6 7 8)
                (1 2 3 4 5 6 7 8 9)
                (2 3 4 5 6 7 8 9 0)
                (3 4 5 6 7 8 9 0 1)
                (4 5 6 7 8 9 0 1 2)
                (5 6 7 8 9 0 1 2 3)
                (6 7 8 9 0 1 2 3 4)
                (7 8 9 0 1 2 3 4 5)
                (8 9 0 1 2 3 4 5 6))))

    (check-equal? (horizontal data '(0 0))
                  '(0 1 2 3 4 5 6 7 8))
    (check-equal? (horizontal data '(1 0))
                  '(0 1 2 3 4 5 6 7 8))
    (check-equal? (horizontal data '(8 0))
                  '(0 1 2 3 4 5 6 7 8))

    (check-equal? (horizontal data '(0 1))
                  '(1 2 3 4 5 6 7 8 9))
    (check-equal? (horizontal data '(0 2))
                  '(2 3 4 5 6 7 8 9 0))
    (check-equal? (horizontal data '(0 8))
                  '(8 9 0 1 2 3 4 5 6))

    (check-equal? (matrix-ref data 0 0) 0)
    (check-equal? (matrix-ref data 1 0) 1)
    (check-equal? (matrix-ref data 4 5) 9)
    (check-equal? (matrix-ref data 8 8) 6)

    (check-equal? (vertical data '(0 0))
                  '(0 1 2 3 4 5 6 7 8))
    (check-equal? (vertical data '(0 1))
                  '(0 1 2 3 4 5 6 7 8))
    (check-equal? (vertical data '(0 8))
                  '(0 1 2 3 4 5 6 7 8))

    (check-equal? (vertical data '(1 0))
                  '(1 2 3 4 5 6 7 8 9))
    (check-equal? (vertical data '(2 0))
                  '(2 3 4 5 6 7 8 9 0))
    (check-equal? (vertical data '(8 0))
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

    (check-equal? (square data '(0 0)) '(0 1 2 1 2 3 2 3 4))
    (check-equal? (square data '(1 1)) '(0 1 2 1 2 3 2 3 4))
    (check-equal? (square data '(4 4)) '(6 7 8 7 8 9 8 9 0))
    (check-equal? (square data '(4 7)) '(9 0 1 0 1 2 1 2 3))

    (check-equal? (include? '(1 2 3) 3) #t)
    (check-equal? (include? '(1 2 3) 4) #f)
    (check-equal? (include? (car data) 4) #t)
    (check-equal? (include? (car data) 9) #f)
    
    (check-equal? (predict '(0 1 2 3 4 5 6 7 8)) '(9))
    (check-equal? (predict '(0 1 2 0 4 5 6 7 8)) '(3 9))
    (check-equal? (predict '(0 1 2 0 4 0 6 7 8)) '(3 5 9))
    (check-equal? (predict '(0 0 2 1 4 6 0 7 8)) '(3 5 9))

    (check-equal? (product (predict '(0 1 2 3 4 5 6 7 8))
                           (predict '(0 1 2 3 4 5 6 7 8))
                           (predict '(0 1 2 3 4 5 6 7 8)))
                  '(9))
    (check-equal? (product (predict '(0 1 2 3 0 5 6 7 8))
                           (predict '(0 1 2 3 0 5 6 7 8))
                           (predict '(0 1 2 3 0 5 6 7 8)))
                  '(4 9))
    (check-equal? (product (predict '(0 1 2 3 0 5 6 0 8))
                           (predict '(0 1 2 3 0 5 6 0 8))
                           (predict '(0 1 2 3 0 5 6 0 8)))
                  '(4 7 9))
    (check-equal? (product (predict '(2 1 0 3 0 5 6 0 8))
                           (predict '(0 1 2 3 8 5 6 0 0))
                           (predict '(3 1 2 0 5 0 6 0 8)))
                  '(4 7 9))

    (check-equal? (replace-nth 8 9 '(1 2 3 4 5 6 7 8 0))
                  '(1 2 3 4 5 6 7 8 9))
    (check-equal? (replace-nth 4 5 '(1 2 3 4 0 6 7 8 9))
                  '(1 2 3 4 5 6 7 8 9))

    (check-equal? (replace-xy '(0 0) 9 data)
                  '((9 1 2 3 4 5 6 7 8)
                    (1 2 3 4 5 6 7 8 9)
                    (2 3 4 5 6 7 8 9 0)
                    (3 4 5 6 7 8 9 0 1)
                    (4 5 6 7 8 9 0 1 2)
                    (5 6 7 8 9 0 1 2 3)
                    (6 7 8 9 0 1 2 3 4)
                    (7 8 9 0 1 2 3 4 5)
                    (8 9 0 1 2 3 4 5 6)))
    (check-equal? (replace-xy '(7 3) 9 data)
                  '((0 1 2 3 4 5 6 7 8)
                    (1 2 3 4 5 6 7 8 9)
                    (2 3 4 5 6 7 8 9 0)
                    (3 4 5 6 7 8 9 9 1)
                    (4 5 6 7 8 9 0 1 2)
                    (5 6 7 8 9 0 1 2 3)
                    (6 7 8 9 0 1 2 3 4)
                    (7 8 9 0 1 2 3 4 5)
                    (8 9 0 1 2 3 4 5 6))))

  (let ((data '((0 0 0 4 0 6 0 0 0)
                (0 6 1 3 0 9 2 4 0)
                (0 5 2 0 0 0 9 6 0)
                (0 7 0 2 6 8 0 9 0)
                (1 2 0 7 0 3 0 8 6)
                (8 0 0 5 0 1 0 0 7)
                (6 0 0 9 0 5 0 0 2)
                (0 8 0 1 3 4 0 7 0)
                (0 4 3 0 0 0 8 1 0))))

    (check-equal? (find-empty 0 (list-ref data 0)) '((0 0) (1 0) (2 0) (4 0) (6 0) (7 0) (8 0)))
    (check-equal? (find-empties data) '((0 0) (1 0) (2 0) (4 0) (6 0) (7 0) (8 0)
                                  (0 1) (4 1) (8 1) (0 2)
                                  (3 2) (4 2) (5 2) (8 2)
                                  (0 3) (2 3) (6 3) (8 3)
                                  (2 4) (4 4) (6 4)
                                  (1 5) (2 5) (4 5) (6 5) (7 5)
                                  (1 6) (2 6) (4 6) (6 6) (7 6)
                                  (0 7) (2 7) (6 7) (8 7)
                                  (0 8) (3 8) (4 8) (5 8) (8 8)))

    (check-equal? (search-solution data '(0 0)) '(3 7 9)) 
    (check-equal? (search-solution data '(5 2)) '(7))

    (check-equal? (solver data)   
                  '((9 3 8 4 2 6 7 5 1)
                    (7 6 1 3 5 9 2 4 8)
                    (4 5 2 8 1 7 9 6 3)
                    (3 7 5 2 6 8 1 9 4)
                    (1 2 4 7 9 3 5 8 6)
                    (8 9 6 5 4 1 3 2 7)
                    (6 1 7 9 8 5 4 3 2)
                    (2 8 9 1 3 4 6 7 5)
                    (5 4 3 6 7 2 8 1 9)))
 
  )

)
(module+ main

  (let ((board '((0 0 0 4 0 6 0 0 0)
                 (0 6 1 3 0 9 2 4 0)
                 (0 5 2 0 0 0 9 6 0)
                 (0 7 0 2 6 8 0 9 0)
                 (1 2 0 7 0 3 0 8 6)
                 (8 0 0 5 0 1 0 0 7)
                 (6 0 0 9 0 5 0 0 2)
                 (0 8 0 1 3 4 0 7 0)
                 (0 4 3 0 0 0 8 1 0))))

#|
  (let ((board '((4 0 0 0 0 9 8 2 5)
                 (0 9 6 0 0 8 3 7 1)
                 (0 8 0 5 1 0 0 0 4)
                 (0 0 8 6 0 0 0 0 3)
                 (0 0 2 0 8 1 0 6 9)
                 (1 6 0 0 4 0 0 0 0)
                 (3 1 0 0 0 0 9 4 0)
                 (6 2 0 0 3 0 7 0 0)
                 (8 7 4 2 9 0 0 0 6))))
|#
    (result (solver board)))


)

