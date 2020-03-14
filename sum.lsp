#! /usr/bin/clisp
;sudo chmod +x sum.lsp
;////////////////////ouitput///////////////////

;(trace vid input obrabotka provznak output_full)

(defun output_full (l)
    (cond
        ((null (car l)) (print 0))
        ((null (car(cdr l)))  (res2 (vid (car l))))
        ;((= 1 (car(car(car(cdr l))))) (res2 (vid(car l) (output (cdr l )))))
        ;( (and  ((= 1 (car(car(car(car(cdr l))))))) (= 0 (car(cdr l))))) (res2(vid(car l))))
        (T (res (vid (car l)) '/\  (vid (car(cdr l)))))))
(defun output_2 (l)
        (res (vid (car l)) '/\ (vid (car(cdr l)))))
(defun output_1 (l)
        (res (vid (car(cdr(cdr l)))) '/\ (vid (car(cdr(cdr(cdr l)))))))


(defun vid (l)
      (cond
            ((null l) NIL)
            ((null (cdr l)) (type_const (car l)))
            (T (append (type_const (car l)) (provznak l)))))
(defun provznak (l)
        (cond
            ((> (car(car(cdr l))) 0) (cons '+ (vid (cdr l))))
            (T (cons '- (vid (cons (cons (- 0 (car(car(cdr l)))) (cdr(car(cdr l)))) (cdr(cdr l))))))))


(defun res (l1 d l2)
      (print l1) (princ d) (princ l2))

(defun res2 (l1)
        (print l1))


(defun type_const (l)
        (cond
              ((= 0 (car(cdr l))) (list (car l)))
              (T (type_x l))))
(defun type_x (l)
        (cond
              ((= 1 (car(cdr l)))
              (cond
                  ((= 1 (car l)) (list 'x))
                  ((= -1 (car l)) (list '- 'x))
                  (t (list (car l) 'x))))
        (T (type_cx l))))
(defun type_cx (l)
        (cond
          ((= 1 (car l)) (list 'x '^ (car(cdr l))))
          ((= -1 (car l)) (list '- 'x '^ (car(cdr l))))
          (t (list (car l) 'x '^ (car(cdr l))))))









;////////////obrabotka//////////////////////////////

(defun obrabotka (l)
    (obsdelete (list
           (provzero(result
                          (append
                              (operation (car l) (car(cdr(cdr(cdr l)))))
                              (operation (car(cdr l)) (car(cdr(cdr l)))))))
           (provzero(result
                              (operation (car(cdr l)) (car(cdr(cdr(cdr l))))))))))


(defun result (l) (varios l NIL NIL NIL))


(defun operation (l1 l2)
        (cond
              ((null l1) '())
              (T (append (provvir2 (car l1) l2)
                  (operation (cdr l1) l2)))))
(defun provvir2 (l1 l2)
      (cond
            ((null l2) '())
            (T (cons (prosum l1 (car l2))
                     (provvir2 l1 (cdr l2))))))
(defun prosum (l1 l2)
      (list (* (car l1) (car l2))
            (+ (car(cdr l1)) (car(cdr l2)))))


(defun provzero (l)
    (cond
        ((null l) NIL)
        ((= (car(car l)) 0) (provzero(result (cdr l))))
        (T (cons (car l) (provzero(result (cdr l)))))))
;///////////////varios

(defun getcar (l)
      (cond
            ((null l) '())
            (T (cons (car(car l)) (getcar (cdr l))))))
(defun getcdr (l)
      (cond
            ((null l) '())
            (T (cons (car(cdr(car l))) (getcdr (cdr l))))))
(defun obsdelete (l) (output_full l)
  (output_full (sokrash l (list (apply 'gcd (getcar (append (car l) (car(cdr l)))))
                  (apply 'min (getcdr (append (car l) (car(cdr l)))))))))


(defun sokrash (l d)
        (cond
              ((null l) '())
              (T (cons (provvir (car l) d)
                  (sokrash (cdr l) d)))))
(defun provvir (l d)
      (cond
            ((null l) '())
            (T (cons (prosedur (car l) d)
                     (provvir (cdr l) d)))))
(defun prosedur (l d)
      (list (/ (car l) (car d))
            (- (car(cdr l)) (car(cdr d)))))



(defun varios (l auxiliary support  control)
      (cond
            ((null l) (varios1 l auxiliary support  control))
            ((null auxiliary) (varios (cdr l) (car l) support control))
            ((= (car(cdr(car l))) (car(cdr auxiliary))) (varios (cdr l)
                (cons (+ (car auxiliary) (car(car l))) (cdr auxiliary)) support control))
            (T (varios (cdr l) auxiliary support (cons (car l) control)))))

(defun varios1 (l auxiliary support  control)
        (cond
            ((null control) (varios2 l auxiliary support  control))
            ((null auxiliary) (varios (cdr control) (car control) support NIL))
       (T (varios (cdr control) (car control) (cons auxiliary support) NIL))))

(defun varios2 (l auxiliary support  control)
        (cond
            ((null auxiliary) support)
            (T (cons auxiliary support))))



;////////////////////////INPUT//////////////////////////////

(defun input (l1 d l2)
        (list (preobraz (car (list (take l1) '/ (drop l1))))
              (preobraz (car(cdr(cdr (list (take l1) '/ (drop l1))))))
              (preobraz (car (list (take l2) '/ (drop l2))))
              (preobraz (car(cdr(cdr (list (take l2) '/ (drop l2))))))))

(defun proverkanull (l)
      (cond
            ((= 0 (car(car(car l)))) (output_1 l))
            ((= 0 (car(car(car(cdr(cdr l)))))) (output_2 l))
            (T (obrabotka l))))


(defun take (l)
        (cond
              ((null l) NIL)
              ((eq '/ (car l)) NIL)
              ((atom (car l)) (cons (car l) (take (cdr l))))
              (T (car l))))

(defun drop (l)
          (cond
                ((null l) NIL)
                ((eq '/ (car l))
          (cond
                ((atom (car(cdr l))) (cdr l))
                (T (car(cdr l)))))
                (T (drop (cdr l)))))

(defun preobraz (l)
    (cond
        ((null l) NIL)
        ((atom l) (list (types (list l))))
        ((atom (car l)) (preobraz (cons (list (car l)) (cdr l))))
        ((null (cdr l)) (list (types (car l))))
        ((eql '+ (car(cdr l))) (cons (types (car l)) (preobraz (cdr(cdr l)))))
        ((eql '- (car(cdr l))) (cons (types (car l)) (preobraz (cdr l))))
        (T (preobraz (cons (append (car l) (list (car(cdr l)))) (cdr(cdr l)))))))


(defun types (l)
      (cond
            ((null l) NIL)
            ((eql '+ (car l)) (types (cdr l)))
            ((null (cdr l)) (types1 l))
            ((null (cdr(cdr l))) (types2 l))
            ((null (cdr(cdr(cdr l)))) (types3 l))
            ((null (cdr(cdr(cdr(cdr l))))) (types4 l))
            ((null (cdr(cdr(cdr(cdr(cdr l)))))) (types5 l))
    (T NIL)))

(defun types1 (l)
        (cond
            ((numberp (car l)) (append l '(0)))  ; const
            (T '(1 1)))) ;x

(defun types2 (l)
        (cond
            ((and (eql '- (car l)) (numberp (car(cdr l))))   ; - const
            (list (- 0 (car(cdr l))) 0))
             ((numberp (car l)) (cons (car l) '(1)))     ; const x
             (T '(-1 1))))      ; - x

(defun types3 (l)
        (cond
            ((eql '- (car l)) (cons (- 0 (car(cdr l))) '(1)))  ; - const x
            (T (cons '1 (cdr(cdr l))))))   ; x ^ const

(defun types4 (l)
        (cond
            ((eql '- (car l)) (cons '-1 (cdr(cdr(cdr l)))))    ; - x ^ const
            (T (cons (car l) (cdr(cdr(cdr l))))))) ; const x ^ const

(defun types5 (l)
          (cons (- 0 (car(cdr l))) (cdr(cdr(cdr(cdr l)))))) ;- const x ^ const



(defun main (l1 d l2)
         (proverkanull (input l1 d l2)))

(print '(EXPRESION 1))
(main (print '((7 + 15 - 3 X) / (2 X ^ 3 + 3 x ^ 2))) (princ '+\ )
      (princ '((X ^ 5 - 3 x ^ 4 + 12 x ^ 3 - 100 x ^ 1 + 0) / (5 X ^ 8 - x))))
(print '(EXPRESION 2))
(main (print '((3 + x - 2 - x + 3 x) / (- 5 x + x ^ 2)))
      (princ '+\ ) (princ '((- x ^ 4 + 12 x ^ 10) / (- 4 x ^ 3))))
(print '(EXPRESION 3))
(main (print '((x + 1) / x)) (princ '+\ )
      (princ '((x - 1) / (x ^ 2))))
(print '(EXPRESION 4))
(main (print '(1 / 3)) (princ '+\ ) (princ '(-1 / 6)))
(print '(EXPRESION 5))
(main (print '((x ^ 2 - 2 x + 3) / (x - 4))) (princ '+\ )
      (princ '((x ^ 3) / (x ^ 2 - 5 x + 2))))
(print '(EXPRESION 6))
(main (print '(x / 3)) (princ '+\ )
      (princ '((-2 x) / 6)))
(print '(EXPRESION 7))
(main (print '(x / (3 x ^ 2))) (princ '+\ )
      (princ '((2 x) / (6 x ^ 2))))
(print '(EXPRESION 8))
(main (print '((x ^ 2 + 2 x) / 3)) (princ '+\ )
      (princ '((- x ^ 2 - 2 x + 1) / 3)))
(print '(EXPRESION 9))
(main (print '((2 x + 5) / (4 x ^ 2 + 10 x + 25))) (princ '+\ )
      (princ '((- 2 x + 5) / (4 x ^ 2 - 10 x + 25))))
(print '(EXPRESION 10))
(main (print '((x - 1) / (x + 1))) (princ '+\ )
      (princ '((x + 1) / (x - 1))))
(print '(EXPRESION 11))
(main (print '((x ^ 2 - x) / (x ^ 2 + x)))
      (princ '+\ ) (princ '((x + 1) / (x - 1))))
(print '(EXPRESION 12))
(main (print '((x ^ 2 + 2 x + 1) / (x + 1))) (princ '+\ )
      (princ '((2 - x - 3) / 1)))
(print '(EXPRESION 13))
(main (print '(1 / (x ^ 2 - 2 x + 2))) (princ '+\ )
      (princ '(1 / (x ^ 2 + 2 x + 2))))
(print '(EXPRESION 14))
(main (print '(3 / (x ^ 2 + 1))) (princ '+\ )
      (princ '((2 + 7 x - 5 - 7 x) / (x ^ 2 + 1))))
(print '(EXPRESION 15))
(main (print '((x + 1) / (x + 1))) (princ '+\ )
      (princ '((x - 1) / (x - 1))))
(print '(EXPRESION 16))
(main (print '(x ^ 2 + x / x ^ 2 - x)) (princ '+\ )
      (princ '(1 - x / x + 1)))
(print '(EXPRESION 17))
(main (print '(x ^ 2 + x / x + 1)) (princ '+\ )
      (princ '(1 / 1 )))
(print '(EXPRESION 18))
(main (print '(7 + 15 - 3 x / x)) (princ '+\ )
      (princ '(x / 5 x ^ 8)))
(print '(EXPRESION 19))
(main (print '(7 + 15 - 3 x / 1)) (princ '+\ )
      (princ '(2 / 2 )))
(print '(EXPRESION 20))
(main (print '(7 + 15 - 3 x / 1)) (princ '+\ )
      (princ '( x / 2  )))
(print '(EXPRESION 21))
(main (print '(15 - 3 x / 1)) (princ '+\ )
      (princ '( 0 / 2  )))
(print '(EXPRESION 22))
(main (print '(0 / 1)) (princ '+\ )
      (princ '( x / 2  )))

;(trace provzero vid res res2 type_const output input obrabotka sokrash obsdelete)
