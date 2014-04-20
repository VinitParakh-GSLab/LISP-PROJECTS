;(critique
(defun convert-to-CNF (l)
  (let ((list_firstpass (removeallIF l)))
    (distributeANDoverOR (apply_associative (removeallNOT list_firstpass)))
    )
  )
;)

;(critique
;(defun distributeOR (l) 
 ; (distributeANDoverOR (apply_associative l))
;)
;)

;(critique
(defun distributeANDoverOR (l)
  (cond
   ((check-if-cnf l) l) 
   ((eql (car l) 'OR ) 
    (let ((cnf-l (append '(OR)(mapcar #'distributeANDoverOR (cdr l)))))
    (apply_associative (convert (car cnf-l) (cdr cnf-l)))))
   (t (apply_associative (append '(AND) (mapcar #'distributeANDoverOR (cdr l)))))
   )
)
;)

;(critique
(defun convert (m l)
  (cond 
   ((null l) nil)
   ((atom (car l)) (cons (car l) (convert (car l) (cdr l))))
   ((and (eql (caar l) 'AND) (null (cdr l))) (append (combine-terms m (cdr l)) (convert m (cddr l))))
   (t (append (combine-terms (cdar l) (cadr l)) (convert m (cddr l))))            
   )
)
;)

;(critique
(defun combine-terms (m l)
  (cond 
   ((atom m) (cons 'AND (combine-all-terms m l)))
   ((atom l) (cons 'AND (combine-all-terms l m)))
   (t 'nothandled)
   )
)
;)

;(critique
(defun combine-all-terms (m l)
  (cond 
   ((null l) nil)
   (t (cons (append '(OR) (list (car l)) (list m)) (combine-all-terms m (cdr l))))
   )
)
;)

;(critique
(defun check-if-cnf (l)
  (cond
   ((atom l) t)
   ((eql (car l) 'AND) (check-if-atomic-AND (cdr l)))
   ((eql (car l) 'OR) (check-if-atomic-OR (cdr l)))
   (t t)
   )
)
;)

;(critique
(defun check-if-atomic-OR (l)
(cond 
  ((null l) t)
  ((atom (car l)) (check-if-atomic-OR (cdr l)))
  ((eql (caar l) 'AND) nil)
  (t (check-if-atomic-OR (cdr l)))
)
)
;)

;(critique
(defun check-if-atomic-AND (l)
  (cond 
   ((null l) t)
   ((atom (car l)) (check-if-atomic-AND (cdr l)))
   ((eql (caar l) 'NOT) (check-if-atomic-AND (cdr l)))
   ((and (eql (caar l) 'OR ) (check-if-atomic-OR (cdar l))) (check-if-atomic-AND (cdr l)))
   (t nil)
 )
)
;)

;(critique
(defun apply_associative (l)
  (cond 
   ((atom l) l)
   ((and (eql (car l) 'OR) (allparamsOR (cdr l))) (apply_associative (cons 'OR (removenestedOR (cdr l)))))
   ((and (eql (car l) 'AND) (allparamsAND (cdr l))) (apply_associative (cons 'AND (removenestedAND (cdr l)))))
   (t (cons (car l) (mapcar #'apply_associative (cdr l))))
 ) 
)
;)

;(critique
(defun allparamsOR (l)
  (cond 
   ((null (car l)) nil)
   ((atom (car l)) nil)
   ((eql (caar l) 'OR) t)
   (t (allparamsOR (cdr l)))
   )
)
;)

;(critique
(defun allparamsAND (l)
  (cond 
   ((null (car l)) nil)
   ((atom (car l)) nil)
   ((eql (caar l) 'AND) t)
   (t (allparamsAND (cdr l)))
   )
)
;)

;(critique
(defun removenestedOR (l)
  (cond 
   ((null l) nil)
   ((atom (car l)) (append (list (car l)) (removenestedOR (cdr l))))
   ((eql (caar l) 'OR) (append (cdr (car l)) (removenestedOR (cdr l))))
   (t (append (list (car l)) (removenestedOR (cdr l))))
  )
)
;)

;(critique
(defun removenestedAND (l)
  (cond 
   ((null l) nil)
   ((atom (car l)) (append (list (car l)) (removenestedAND (cdr l))))
   ((eql (caar l) 'AND) (append (cdr (car l)) (removenestedAND (cdr l))))
   (t (append (list (car l)) (removenestedAND (cdr l))))
  )
)
;)

;(critique
(defun removeallIF (l)
         (cond 
          ((null l) l)
          ((atom l) l)
          ((eql (car l) 'IF) (removeIF (cadr l) (caddr l)))
          ((eql (car l) 'OR) (removeOR (cdr l)))
          ((eql (car l) 'AND) (removeAND (cdr l)))
          ((eql (car l) 'NOT) (removeNOT (cadr l)))
          (t (removeIFF (cadr l) (caddr l)))
          )
)
;)

;(critique
(defun removeallNOT (l)
  (cond
   ((atom l) l)
   ((and (eql (car l) 'NOT) (atom (cadr l))) l)
   ((and (eql (car l) 'NOT) (eql (caadr l) 'NOT)) (cadr (cadr l)))
   ((and (eql (car l) 'NOT) (eql (caadr l) 'OR)) (cons 'AND (mapcar #'removeNOT1 (cdr (cadr l)))))
   ((and (eql (car l)'NOT) (eql (caadr l) 'AND)) (cons 'OR (mapcar #'removeNOT1 (cdr (cadr l)))))
   (t (cons (car l) (mapcar #'removeallNOT (cdr l))))
   )
)
;)

;(critique
(defun removeNOT1 (l)
   (removeallNOT (list 'NOT l))
)
;)

;(critique
(defun removeIF (a b)
  (cond 
   ((and (atom a) (atom b)) (list 'OR (list 'NOT a) b))
   (t (list 'OR (list 'NOT  (removeallIF a)) (removeallIF b)))
   )
  )
;)
          
;(critique
(defun removeOR (a)
  (cons 'OR (mapcar #'removeallIF a))
  )
;)

;(critique
(defun removeAND (a)
  (cons 'AND (mapcar #'removeallIF a))
  )
;)

;(critique
(defun removeNOT (a)
  (cond 
   ((atom a) (list 'NOT a))
   (t (list 'NOT (removeallIF a)))
   )
  )
;)

;(critique
(defun removeIFF (a b)
  (list 'AND (removeallIF (list 'IF a b)) (removeallIF (list 'IF b a)))
  )
;)
