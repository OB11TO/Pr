#! /usr/bin/clisp
;sudo chmod +x sum.lsp
;////////////////////ouitput///////////////////

; -- Преобразовать результат в естественный вид
(defun output (l)
    (cond
        ; expression = 0
        ((null (car l)) (print 0))
        (t (prin3 (math-view (car l)) '/\  (math-view (cadr l))))))



; -- Печать 3 элементов
(defun prin3 (x y z)
    (cond
        ((null (cdr x)) (print (car x)))
        (t (print x)))
    (princ y)
   (cond
        ((null (cdr z)) (princ (car z)))
        (t (princ z))))

; -- Преобразовать выражение из внутреннего представления
; -- к естественному виду
(defun math-view (l)

    (cond
        ((null l) nil)

        ((null (cdr l)) (math-summand (car l)))
        (t (append (math-summand (car l))
                (cond
                    ((> (caadr l) 0) (cons '+ (math-view (cdr l))))
                    (t (cons '- (math-view (cons (cons (- 0 (caadr l))
                                                       (cdadr l))
                                                 (cddr l))))))))))

;--; -- Преобразовать команду из внутреннего представления
; -- к естественному виду
(defun math-summand (l)
    (cond
        ; const
        ((= 0 (cadr l)) (list (car l)))
        ((= 1 (cadr l))
            (cond
                ; x
                ((= 1 (car l)) (list 'x))
                ; - x
                ((= -1 (car l)) (list '- 'x))
                ; const x
                (t (list (car l) 'x))))
        ; x ^ const
        ((= 1 (car l)) (list 'x '^ (cadr l)))
        ; - x ^ const
        ((= -1 (car l)) (list '- 'x '^ (cadr l)))
        ; const x ^ const
        (t (list (car l) 'x '^ (cadr l)))))









;////////////treatment//////////////////////////////

(defun treatment (l)
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


(defun varios (l auxiliary support control)
    (cond
          ((null l)
    (cond
          ((null control)
            (cond
               ((null auxiliary) support)
               (T (cons auxiliary support))))

          ((null auxiliary) (varios (cdr control) (car control) support NIL))
          (T (varios (cdr control) (car control) (cons auxiliary support) NIL))))

       ((null auxiliary) (varios (cdr l) (car l) support control))
            ((= (cadar l) (cadr auxiliary)) (varios (cdr l)
             (cons (+ (car auxiliary) (caar l)) (cdr auxiliary)) support control))

      (T (varios (cdr l) auxiliary support (cons (car l) control)))))


(defun getcar (l)
      (cond
            ((null l) '())
            (T (cons (car(car l)) (getcar (cdr l))))))

(defun getcdr (l)
      (cond
            ((null l) '())
            (T (cons (car(cdr(car l))) (getcdr (cdr l))))))

(defun obsdelete (l) (output l)
  (sokrash l (list (apply 'gcd (getcar (append (car l) (car(cdr l)))))
                  (apply 'min (getcdr (append (car l) (car(cdr l))))))))



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









;////////////////////////INPUT//////////////////////////////

(defun input (l1 n l2)
        (list (preobraz (car (list (take l1) '/ (drop l1))))
              (preobraz (car(cdr(cdr (list (take l1) '/ (drop l1))))))
              (preobraz (car (list (take l2) '/ (drop l2))))
              (preobraz (car(cdr(cdr (list (take l2) '/ (drop l2))))))))

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
                ((atom (cadr l)) (cdr l))
                (T (cadr l))))
                (T (drop (cdr l)))))

(defun preobraz (l)
    (cond
        ((null l) NIL)
        ((atom l) (list (types (list l))))
        ((atom (car l)) (preobraz (cons (list (car l)) (cdr l))))
        ((null (cdr l)) (list (types (car l))))
        ((eql '+ (cadr l)) (cons (types (car l)) (preobraz (cddr l))))
        ((eql '- (cadr l)) (cons (types (car l)) (preobraz (cdr l))))
        (T (preobraz (cons (append (car l) (list (cadr l))) (cddr l))))))


(defun types (l)
      (cond
            ((null l) NIL)
            ((eql '+ (car l)) (types (cdr l)))
            ((null (cdr l))
      (cond
            ((numberp (car l)) (append l '(0)))  ; const
            (T '(1 1)))) ; x
            ((null (cdr(cdr l)))
      (cond
            ((and (eql '- (car l)) (numberp (car(cdr l))))   ; - const
            (list (- 0 (car(cdr l))) 0))
             ((numberp (car l)) (cons (car l) '(1)))     ; const x
             (T '(-1 1))))      ; - x
             ((null (cdr(cdr(cdr l))))
      (cond
             ((eql '- (car l)) (cons (- 0 (car(cdr l))) '(1)))  ; - const x
             (T (cons '1 (cdr(cdr l))))))   ; x ^ const
             ((null (cdr(cdr(cdr(cdr l)))))
      (cond
             ((eql '- (car l)) (cons '-1 (cdr(cdr(cdr l)))))    ; - x ^ const
             (T (cons (car l) (cdr(cdr(cdr l))))))) ; const x ^ const
             ((null (cdr (cdr(cdr(cdr(cdr l))))))
             (cons (- 0 (car(cdr l))) (cdr(cdr(cdr(cdr l)))))) ; - const x ^ const
        (T  NIL)))




(defun expression (n) (expressions n)
    (cond
      ((= n 1)
            (main (print '((7 + 15 - 3 X) / (2 X ^ 3 + 3 x ^ 2))) (princ '+\ )
            (princ '((X ^ 5 - 3 x ^ 4 + 12 x ^ 3 - 100 x ^ 1 + 0) / (5 X ^ 8 - x)))))
      ((= n 2)
            (main (print '((3 + x - 2 - x + 3 x) / (- 5 x + x ^ 2)))
            (princ '+\ ) (princ '((- x ^ 4 + 12 x ^ 10) / (- 4 x ^ 3)))))
      ((= n 3)
              (main (print '((x + 1) / x)) (princ '+\ )
              (princ '((x - 1) / (x ^ 2)))))
      ((= n 5)
              (main (print '(1 / 3)) (princ '+\ ) (princ '(-1 / 6))))
      ((= n 6)
              (main (print '((x ^ 2 - 2 x + 3) / (x - 4))) (princ '+\ )
              (princ '((x ^ 3) / (x ^ 2 - 5 x + 2)))))
      ((= n 7)
              (main (print '(x / 3)) (princ '+\ ) (princ '((-2 x) / 6))))
      ((= n 8)
              (main (print '(x / (3 x ^ 2))) (princ '+\ )
              (princ '((2 x) / (6 x ^ 2)))))
      ((= n 9)
              (main (print '((x ^ 2 + 2 x) / 3)) (princ '+\ )
              (princ '((- x ^ 2 - 2 x + 1) / 3))))

      ((= n 10)
               (main (print '((2 x + 5) / (4 x ^ 2 + 10 x + 25))) (princ '+\ )
               (princ '((- 2 x + 5) / (4 x ^ 2 - 10 x + 25)))))
      ((= n 11)
               (main (print '((x - 1) / (x + 1))) (princ '+\ )
               (princ '((x + 1) / (x - 1)))))
      ((= n 12)
               (main (print '((x ^ 2 - x) / (x ^ 2 + x)))
               (princ '+\ ) (princ '((x + 1) / (x - 1)))))
      ((= n 13)
              (main (print '((x ^ 2 + 2 x + 1) / (x + 1))) (princ '+\ )
              (princ '((2 - x - 3) / 1))))
      ((= n 14)
              (main (print '(1 / (x ^ 2 - 2 x + 2))) (princ '+\ )
              (princ '(1 / (x ^ 2 + 2 x + 2)))))
      ((= n 15)
              (main (print '(3 / (x ^ 2 + 1))) (princ '+\ )
              (princ '((2 + 7 x - 5 - 7 x) / (x ^ 2 + 1)))))
      ((= n 16)
               (main (print '((x + 1) / (x + 1))) (princ '+\ )
               (princ '((x - 1) / (x - 1)))))
      ((= n 17)
               (main (print '(x ^ 2 + x / x ^ 2 - x)) (princ '+\ )
               (princ '(1 - x / x + 1))))
      (( = n 18)
               (main (print '(x ^ 2 + x / x + 1)) (princ '+\ )
               (princ '( 1 ))))
      (T "Error expression")))

(defun expressions (n)
    (print 'expression) (princ n))

(defun main (l1 n l2)
        (output (treatment (input l1 n l2))))

(expression 1)
(expression 2)
(expression 3)
(expression 4)
(expression 5)
(expression 6)
(expression 7)
(expression 8)
(expression 9)
(expression 10)
(expression 11)
(expression 12)
(expression 13)
(expression 14)
(expression 15)
(expression 16)
(expression 17)
(expression 18)
