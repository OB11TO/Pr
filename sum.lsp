#! /usr/bin/clisp
;sudo chmod +x sum.lsp
;////////////////////ouitput///////////////////


(defun output_full (l)
    (cond
        ((null (car l)) (print 0))
        ;((= 1 (car(cdr l)))  (res2 (vid (car l))))
        ;((= 1 (car(car(car(cdr l))))) (res2 (vid(car l) (output (cdr l )))))
        ;( (and  ((= 1 (car(car(car(car(cdr l))))))) (= 0 (car(cdr l))))) (res2(vid(car l))))
        (T (res (vid (car l)) '/\  (vid (car(cdr l)))))))
(defun output_2 (l)
        (res (vid (car l)) '/\ (vid (car(cdr l)))))
(defun output_1 (l)
        (res (vid (car(cdr(cdr l)))) '/\ (vid (car(cdr(cdr(cdr l)))))))
(defun output_3 (l) (print 0))

(defun vid (l)
      (cond
            ((null l) NIL)
            ((null (cdr l)) (type_const (car l)))
            (T (append (type_const (car l)) (provznak l)))))
(defun provznak (l)
        (cond
            ((> (car(car(cdr l))) 0) (cons '+ (vid (cdr l))))
            (T (cons '- (vid (cons (cons (- 0 (car(car(cdr l)))) (cdr(car(cdr l)))) (cdr(cdr l))))))))   ;(PROVZNAK '((2 2) (-15 0) (-39 1)))  (VID '((15 0) (-39 1)))



(defun res (l1 d l2)
      (print l1) (princ d) (princ l2))

(defun res2 (l1)
        (print l1))


(defun type_const (l)
        (cond
              ((= 0 (car(cdr l))) (list (car l)))  ;const
              (T (type_x l))))
(defun type_x (l)
        (cond
              ((= 1 (car(cdr l)))
              (cond
                  ((= 1 (car l)) (list 'x))  ;x
                  ((= -1 (car l)) (list '- 'x)); -x
                  (t (list (car l) 'x))))  ;cx
        (T (type_cx l))))
(defun type_cx (l)
        (cond
          ((= 1 (car l)) (list 'x '^ (car(cdr l))))  ;x^c
          ((= -1 (car l)) (list '- 'x '^ (car(cdr l)))) ;-x^1
          (t (list (car l) 'x '^ (car(cdr l))))))  ;cx^2








;////////////obrabotka//////////////////////////////

(defun obrabotka (l)
    (obsdelete (list
           (provzero(result
                          (append
                              (operation (car l) (car(cdr(cdr(cdr l)))))
                              (operation (car(cdr l)) (car(cdr(cdr l)))))))
           (provzero(result
                              (operation (car(cdr l)) (car(cdr(cdr(cdr l))))))))))
(defun obrabotka2 (l)
      (obsdelete (list(provzero (result
                                (operation(car l) (car(cdr(cdr l))))))
                 (result(provzero
                                (operation (car(cdr l)) (car(cdr(cdr(cdr l))))))))))

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

(defun result (l) (varios l NIL NIL NIL))

(defun varios (l first_l two_l  three_l)
      (cond
            ((null l) (varios1 l first_l two_l  three_l))
            ((null first_l) (varios (cdr l) (car l) two_l three_l))
            ((= (car(cdr(car l))) (car(cdr first_l))) (varios_oper l first_l two_l  three_l))
            (T (varios (cdr l) first_l two_l (cons (car l) three_l)))))

(defun varios_oper (l first_l two_l  three_l)
      (varios (cdr l) (cons (+ (car first_l) (car(car l))) (cdr first_l)) two_l three_l))

(defun varios1 (l first_l two_l  three_l)
        (cond
            ((null three_l) (varios2 l first_l two_l  three_l))
            ((null first_l) (varios (cdr three_l) (car three_l) two_l NIL))
            (T (varios (cdr three_l) (car three_l) (cons first_l two_l) NIL))))

(defun varios2 (l first_l two_l  three_l)
        (cond
            ((null first_l) two_l)
            (T (cons first_l two_l))))



;////////////////////////INPUT//////////////////////////////

(defun input (l1 d l2)
        (list (preobraz (car (list (take l1) '/ (drop l1))))
              (preobraz (car(cdr(cdr (list (take l1) '/ (drop l1))))))
              (preobraz (car (list (take l2) '/ (drop l2))))
              (preobraz (car(cdr(cdr (list (take l2) '/ (drop l2))))))))


(defun proverkanull (l)
        (cond
            ((and (= 0 (car(car(car l)))) (= 0 (car(car(car(cdr(cdr l))))))) (output_3 l))
            (T (proverkanull2 l))))

(defun proverkanull2 (l)
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
        ((atom l) (list (types (list l))))                                  ;(PREOBRAZ '(2)) (PREOBRAZ '((2)))
        ((atom (car l)) (preobraz (cons (list (car l)) (cdr l))))          ;(PREOBRAZ '(15 - 3 X))  Trace: (PREOBRAZ '((15) - 3 X))
        ((null (cdr l)) (list (types (car l))))
        ((eql '+ (car(cdr l))) (cons (types (car l)) (preobraz (cdr(cdr l)))))     ;(PREOBRAZ '(123 + 150 - 3)) (PREOBRAZ '((123) + 150 - 3))
        ((eql '- (car(cdr l))) (cons (types (car l)) (preobraz (cdr l))))
        (T (preobraz (cons (append (car l) (list (car(cdr l)))) (cdr(cdr l)))))))


(defun types (l)
      (cond
            ((null l) NIL)
            ((eql '+ (car l)) (types (cdr l)))  ;???
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

(defun main2 (l1 d l2)
        (proverkanull (input l1 d l2)))

(defun fil (l1 acc)
          (cond
                ((null l1) acc)
                ((equal '/ (car l1)) (list acc '/ (cdr l1)))
                (T (fil (cdr l1) (append acc (list (car l1)))))))

(defun main (l1 d l2)
         (proverkanull (input l1 d l2)))

;(trace main input proverkanull  provzero proverkanull2 take drop  preobraz types2 types types1 types3 types4 types5 output_full  output_1 output_2 output_3 vid res  provznak res2 type_const type_x type_cx obrabotka result  varios varios_oper varios1 varios2 prosedur provvir operation provvir2 prosum provzero getcar getcdr obsdelete sokrash)
(print '(EXPRESION 1))
(main  (fil '(10 x / 3 x) '() ) '+
        (fil '( 9 / 5 ) '() ))
(print '(EXPRESION 2))
(main2 (print '((690 + x ^ 8 - 14 x ^ 12) / (- 25  - x ^ 21 )))
      (princ '+\ ) (princ '((12 x ^ 10) / (-15 + X ^ 2))))
(print '(EXPRESION 3))
(main2 (print '(2 / 9)) (princ '+\ ) (princ '(-2 / 9)))
(print '(EXPRESION 4))
(main2 (print '((x ^ 1 + 3) / (x - 1))) (princ '+\ )
      (princ '((x ^ 1) / (x - 2))))
(print '(EXPRESION 5))
(main2 (print '(x ^ 2 / 6)) (princ '+\ )
      (princ '((-2 x ^ 2) / 12)))
(print '(EXPRESION 6))
(main2 (print '(1  / (9 x ))) (princ '+\ )
      (princ '((2 ) / (16 x))))
(print '(EXPRESION 7))
(main2 (print '((x ^ 2 + 2 x) / 3)) (princ '+\ )
      (princ '((- x ^ 2 - 2 x + 1) / 3)))
(print '(EXPRESION 8))
(main2 (print '((4 x ^ 2 + 25) / (8 x ^ 2 + 15))) (princ '+\ )
      (princ '((- 4 x ^ 2 + 25) / (8  x ^ 2 - 15))))
(print '(EXPRESION 9))
(main2 (print '((2 x - 1) / (x + 2))) (princ '+\ )
      (princ '((2 x + 1) / (x - 2))))
(print '(EXPRESION 10))
(main2 (print '( 0 / (4 x ^ 2 + 10 x + 25))) (princ '+\ )
      (princ '( 0 / (4 x ^ 2 - 10 x + 25))))
(print '(EXPRESION 11))
(main2 (print '(0 / 1)) (princ '+\ )
      (princ '( x / 2  )))
(print '(EXPRESION 12))
(main2 (print '(15 - 3 x / 1)) (princ '+\ )
      (princ '( 0 / 2  )))
(print '(EXPRESION 13))
(main2 (print '((123 + 150 - 3 ) / (2 X ^ 3 ))) (princ '+\ )
      (princ '((X ^ 5 - 3 x ^ 4 ) / ( X ^ 3 - 2))))
(print '(EXPRESION 14))
(main2 (print '(7 + 15 - 3 x / x)) (princ '+\ )
      (princ '(x / 5 x ^ 8)))
(print '(EXPRESION 15))
(main2 (print '(7 + 15 - 3 x / 1)) (princ '+\ )
      (princ '(2 / 2 )))


;(trace main input proverkanull  provzero proverkanull2 take drop  preobraz types2 types types1 types3 types4 types5)
;(trace output_full  output_1 output_2 output_3 vid res  provznak res2 type_const type_x type_cx)
;(trace output_full obrabotka result  varios varios_oper varios1 varios2 prosedur provvir operation provvir2 prosum provzero getcar getcdr obsdelete sokrash)

;(trace main input proverkanull  provzero proverkanull2 take drop  preobraz types2 types types1 types3 types4 types5 output_full  output_1 output_2 output_3 vid res  provznak res2 type_const type_x type_cx obrabotka result  varios varios_oper varios1 varios2 prosedur provvir operation provvir2 prosum provzero getcar getcdr obsdelete sokrash)
