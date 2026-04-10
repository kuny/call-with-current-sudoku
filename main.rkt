#lang racket

;;
;; sudoku solver
;;

(module+ test
  (require rackunit))

(define-syntax coord-x
  (syntax-rules ()
    [(coord-x x)
     (first x)]))

(define-syntax coord-y
  (syntax-rules ()
    [(coord-y x)
     (second x)]))

(define (matrix-ref d x y)
  (list-ref
    (list-ref d y) x))

(define (horizontal d xy)
  (let  ((x (coord-x xy))
         (y (coord-y xy)))
    (list-ref d y)))

(define (vertical d xy)
  (let loop ((i 0) (lst '()))
    (if (> i 8)
        (reverse lst)
        (loop (+ i 1)
              (cons (matrix-ref d (coord-x xy) i) lst)))))

(define (index-set x)
  (let ((a (quotient x 3)))
    (cond ((= a 0) '(0 1 2))
          ((= a 1) '(3 4 5))
          (else '(6 7 8)))))

(define (cartesian-map f xs yx)
  (map (lambda (x)
         (map (lambda (y)
                (f x y))
              yx))
         xs))

(define (square d xy)
  (let* ((x (coord-x xy))
         (y (coord-y xy))
         (xs (index-set x))
         (ys (index-set y)))
    (flatten (cartesian-map (lambda (a b)
                              (matrix-ref d a b))
                            xs ys))))

(define (contains? lst x)
  (let loop ((l lst))
    (cond ((null? l) #f)
          ((equal? (car l) x) #t)
          (else
            (loop (cdr l))))))

(define (candidates lst)
  (let loop ((xs '(1 2 3 4 5 6 7 8 9)) (ret '()))
    (cond ((null? xs) (reverse ret))
          ((not (contains? lst (car xs)))
           (loop (cdr xs) (cons (car xs) ret)))
          (else
            (loop (cdr xs) ret)))))

(define (intersect seta setb setc)
  (define (intersect-pair x y)
    (let loop ((l x) (ret '()))
      (cond ((null? l) (reverse ret))
            ((contains? y (car l))
             (loop (cdr l)
                   (cons (car l) ret)))
            (else
              (loop (cdr l) ret)))))
  (let ((ab (intersect-pair seta setb))
        (ac (intersect-pair seta setc)))
    (intersect-pair ab ac)))

(define (replace-nth n nw lst)
  (cond ((null? lst) '())
        ((zero? n) (cons nw (cdr lst)))
        (else (cons (car lst)
                    (replace-nth (- n 1) nw (cdr lst))))))

(define (replace-xy xy nw d)
  (let ((x (coord-x xy))
        (y (coord-y xy)))
  (cond ((null? d) '())
        ((zero? y)
         (cons (replace-nth x nw (car d))
                 (cdr d)))
        (else
          (cons (car d)
                (replace-xy (list x (- y 1)) nw (cdr d)))))))

(define (find-empty y l)
  (let loop ((x l) (i 0) (ret '()))
    (cond ((null? x) (reverse ret))
          ((zero? (car x))
           (loop (cdr x)
                 (+ i 1)
                 (cons (list i y) ret)))
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
  (null? (find-empties d)))

(define (search-solution board xy)
  (let* ((h-candidates (candidates (horizontal board xy)))
         (v-candidates (candidates (vertical board xy)))
         (s-candidates (candidates (square board xy))))
    (intersect h-candidates v-candidates s-candidates)))

(define (fill-singletons board)
  (define (solve-internal board xy)
    (let ((cands (search-solution board xy)))
      (if (or (null? cands)
              (> (length cands) 1))
        board
        (replace-xy xy (car cands) board))))
  (let loop ((empties (find-empties board)) (board board))
    (cond ((null? empties) board)
          (else
            (loop (cdr empties)
                  (solve-internal board (car empties)))))))

;; 制約伝播を収束するまで繰り返す（純粋関数）
(define (propagate board)
  (let loop ((board board))
    (let ((next (fill-singletons board)))
      (if (equal? next board)
        board
        (loop next)))))

;; 候補数が最少の空きセルを返す（MRVヒューリスティック）
(define (find-best-empty board empties)
  (let loop ((rest empties) (best #f) (best-count 10))
    (if (null? rest)
      best
      (let* ((xy (car rest))
             (count (length (search-solution board xy))))
        (if (< count best-count)
          (loop (cdr rest) xy count)
          (loop (cdr rest) best best-count))))))

(define (solve board)
  ;; return: 解が見つかった瞬間に全再帰を脱出する継続
  (or (call/cc
        (lambda (return)
          ;; backtrack: board × fail → void
          ;;   fail は「この枝が失敗したとき呼ぶサンク」
          (define (backtrack board fail)
            (let ((propagated (propagate board)))
              (if (complete? propagated)
                (return propagated)           ; 解発見 → 即脱出
                (let* ((empties (find-empties propagated))
                       (best-xy (find-best-empty propagated empties)))
                  (if (not best-xy)
                    (fail)                    ; 空きなし・未完 → 失敗
                    (try-each (search-solution propagated best-xy)
                              best-xy propagated fail))))))
          ;; 候補を順に試す。尽きたら fail を呼ぶ
          (define (try-each cands xy board fail)
            (if (null? cands)
              (fail)
              (backtrack (replace-xy xy (car cands) board)
                         (lambda ()           ; この候補が失敗したら次へ
                           (try-each (cdr cands) xy board fail)))))
          (backtrack board (lambda () #f))    ; 初期失敗継続は #f を返す
          #f))
      board))

(define (display-board board)
  (let loop ((rows board))
    (if (null? rows)
      (newline)
      (begin
        (displayln (car rows))
        (loop (cdr rows))))))

(define (load-config)
  (call-with-input-file "./config.scm"
                        (lambda (in)
                          (read in))))

(define (show-boards boards (i 1))
  (cond ((null? boards) (newline))
        (else
          (displayln (string-append (number->string i) ". " (first (car boards))))
          (show-boards (cdr boards) (+ i 1)))))

(define (execute-solver filename)
  (let ((board
          (call-with-input-file filename
                                (lambda (in)
                                  (read in)))))
    (begin
      (display-board board)
      (display-board (solve board)))))

(define (->path x)
  (cdr (assq 'path x)))

(define (->note x)
  (cdr (assq 'note x)))


(define (config key cfg)
  (cdr (assq key cfg)))

(define (load-boards key cfg)
  (let ((path (->path (config key cfg))))
    (call-with-input-file path
                          (lambda (in)
                            (read in)))))

(define (valid? cfg key)
  (or (eq? key 'help)
      (assoc key cfg)))

(define (run-command cfg expr)
  (cond ((not (= (length expr) 2)) (undefined expr))
        (else
         (let ((boards (load-boards (car expr) cfg)))
           (cond ((equal? (second expr) 'list) (show-boards boards))
                 ((and (number? (second expr))
                       (<= (second expr) (length boards)))
                  (execute-solver (second (list-ref boards
                                                    (- (second expr) 1)))))
                 (else (undefined expr)))))))

(define (read-command)
  (display "🐢 ")
  (read))

(define (help cfg)
  (let loop ((x cfg))
    (if (null? x)
      (newline)
      (begin
        (displayln
          (format "~a ~a"
                  (car (car x))
                  (->note (cdr (car x)))))
        (loop (cdr x))))))

(define (dispatch-command cfg expr)
  (cond ((equal? expr '(help)) (help cfg))
        ((and (pair? expr)
              (equal? (car expr) '+))
         (displayln (apply + (cdr expr))))
        ((and (pair? expr)
              (valid? cfg (car expr)))
         (run-command cfg expr))
        (else
          (undefined expr))))

(define (undefined expr)
  (displayln (format "~s undefined" expr)))

(define (exit? expr)
  (equal? expr '(exit)))

(define (bye)
  (displayln "bye."))

(define (repl cfg)
  (let ([expr (read-command)])
    (cond [(exit? expr) (bye)]
          [else
            (dispatch-command cfg expr)
            (repl cfg)])))


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

    (check-equal? (index-set 0) '(0 1 2))
    (check-equal? (index-set 1) '(0 1 2))
    (check-equal? (index-set 2) '(0 1 2))

    (check-equal? (index-set 3) '(3 4 5))
    (check-equal? (index-set 4) '(3 4 5))
    (check-equal? (index-set 5) '(3 4 5))

    (check-equal? (index-set 6) '(6 7 8))
    (check-equal? (index-set 7) '(6 7 8))
    (check-equal? (index-set 8) '(6 7 8))

    (check-equal? (square data '(0 0)) '(0 1 2 1 2 3 2 3 4))
    (check-equal? (square data '(1 1)) '(0 1 2 1 2 3 2 3 4))
    (check-equal? (square data '(4 4)) '(6 7 8 7 8 9 8 9 0))
    (check-equal? (square data '(4 7)) '(9 0 1 0 1 2 1 2 3))

    (check-equal? (contains? '(1 2 3) 3) #t)
    (check-equal? (contains? '(1 2 3) 4) #f)
    (check-equal? (contains? (car data) 4) #t)
    (check-equal? (contains? (car data) 9) #f)

    (check-equal? (candidates '(0 1 2 3 4 5 6 7 8)) '(9))
    (check-equal? (candidates '(0 1 2 0 4 5 6 7 8)) '(3 9))
    (check-equal? (candidates '(0 1 2 0 4 0 6 7 8)) '(3 5 9))
    (check-equal? (candidates '(0 0 2 1 4 6 0 7 8)) '(3 5 9))

    (check-equal? (intersect (candidates '(0 1 2 3 4 5 6 7 8))
                             (candidates '(0 1 2 3 4 5 6 7 8))
                             (candidates '(0 1 2 3 4 5 6 7 8)))
                  '(9))
    (check-equal? (intersect (candidates '(0 1 2 3 0 5 6 7 8))
                             (candidates '(0 1 2 3 0 5 6 7 8))
                             (candidates '(0 1 2 3 0 5 6 7 8)))
                  '(4 9))
    (check-equal? (intersect (candidates '(0 1 2 3 0 5 6 0 8))
                             (candidates '(0 1 2 3 0 5 6 0 8))
                             (candidates '(0 1 2 3 0 5 6 0 8)))
                  '(4 7 9))
    (check-equal? (intersect (candidates '(2 1 0 3 0 5 6 0 8))
                             (candidates '(0 1 2 3 8 5 6 0 0))
                             (candidates '(3 1 2 0 5 0 6 0 8)))
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

    (check-equal? (solve data)
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
#|
  (check-equal? (load-config) '((daiso . ((path . "./boards/daiso/daiso.scm")))))
  (let ((cfg (load-config)))
    (check-equal? (assq 'daiso cfg) '(daiso . ((path . "./boards/daiso/daiso.scm"))))
    (check-equal? (cdr (assq 'daiso cfg)) '((path . "./boards/daiso/daiso.scm"))))
    #| (check-equal? (load-boards cfg 'daiso) "./boards/daiso/daiso.scm")) |#

|#
  (let ((cfg '((daiso . ((path . "./boards/daiso/daiso.scm"))))))
    (check-equal? (->path (cdr (assq 'daiso cfg))) "./boards/daiso/daiso.scm"))

)
(module+ main

  (repl (load-config))

)
