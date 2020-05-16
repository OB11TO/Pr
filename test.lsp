(defun flatten (l)
(cond
((null l) NIL)
((atom (car l)) (cons (car l) (flatten (cdr l))))
((not (and (eql (length (car l)) '1) (not (atom (caar l))))) (append (list (flatten (car l))) (flatten (cdr l))))
(T (append (flatten (car l)) (flatten (cdr l))))))
