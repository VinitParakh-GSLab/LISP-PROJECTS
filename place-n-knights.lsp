;;(critique
(defun get-minimum-index (matrix)  
   (cond 
   ((= (length matrix) 1) (car matrix))
   ((<= (car matrix) (cadr matrix))
    (get-minimum-index (cons (car matrix) (cddr matrix)))
    )
   (t (get-minimum-index (cdr matrix)))
    )
  )
;;)

;;(critique
(defun place-knights (l)
  (cond 
   ((not (= (length (car l)) 2)) 'InvalidInput)
   ((null l) 'InvalidInput)
   (t (get-final-list (sort (n-knights l) #'<) (caar l) (cadr (car l))))
 )
)
;;)

;;(critique
(defun get-final-list (finalist row col)
  (cond 
   ((null finalist) nil)
   (t (let ((i (1+ (floor (car finalist) row)))
            (j (1+ (mod (car finalist) col))))
        (cons (list i j) (get-final-list (cdr finalist) row col)))
      )
   )
)
;;)


;;(critique
(defun n-knights (l)   
  (cond 
   ((= (* (car (car l)) (cadr (car l))) (length (cadr l)) ) nil)
   (t
    (let
        ((matrix (get-board-matrix (car l) (cadr l) 1 1)))
        (let ((index (position (get-minimum-index matrix) matrix) ))
        (let ((i (1+  (floor index (cadr (car l)))))
              (j (1+  (mod index (cadr (car l))))))
          (let ((finalist (cons index (n-knights (list (car l) (append (cadr l) (get-blockstate i j (car l) (cadr l))))))))
            finalist
            )
          )
        )
      )
    )
   )   
)
;;)

;;(critique
(defun check-if-usable (pos unusable)  
  (cond 
   ((null unusable) nil)
   ((equal pos (car unusable)) t)
   (t (check-if-usable pos (cdr unusable)))
  )
)
;;)


;;(critique
(defun get-board-matrix (size unusable i j) 
  (let
  ((count 0))   
    (cond    
    ((and (= i (1+ (car size)))(= j 1)) nil)  
    (t
     (cond ((not (null (check-if-usable (list i j) unusable)))(incf count 9))
           (t (incf count (1- (length (get-blockstate i j size unusable)))))
           )
     (cond ((< j (cadr size))(cons count (get-board-matrix size unusable i (1+ j))))
            (t (cons count (get-board-matrix size unusable (1+ i) 1))))
     )
    )
    )
  )
;;)

;;(critique
(defun get-blockstate (i j size unusable)
  (remove nil
          (list 
           (list i j)
           (cond ((and (<= (+ i 2) (car size)) (<= (1+ j) (cadr size))(null (check-if-usable (list (+ i 2) (1+ j)) unusable)))(list (+ i 2) (1+ j))) (t nil))
           (cond ((and (<= (+ i 2) (car size)) (>= (1- j) 1) (null (check-if-usable (list (+ i 2) (1- j)) unusable)))(list (+ i 2) (1- j)))(t nil))
           (cond ((and (>= (- i 2) 1)(<= (1+ j) (cadr size))(null (check-if-usable (list (- i 2) (1+ j)) unusable))) (list (- i 2) (1+ j)))(t nil))
           (cond ((and (>= (- i 2) 1)(>= (1- j) 1)(null (check-if-usable (list (- i 2) (1- j)) unusable)))(list (- i 2) (1- j)))(t nil))
           (cond ((and (<= (1+ i) (car size))(<= (+ j 2) (cadr size))(null (check-if-usable (list (1+ i) (+ j 2)) unusable)))(list (1+ i) (+ j 2)))(t nil))
           (cond ((and (>= (1- i) 1)(<= (+ j 2) (cadr size))(null (check-if-usable (list (1- i) (+ j 2)) unusable)))(list (1- i) (+ j 2)))(t nil))
           (cond ((and (<= (1+ i) (car size))(>= (- j 2) 1)(null (check-if-usable (list (1+ i) (- j 2)) unusable)))(list (1+ i) (- j 2)))(t nil))
           (cond ((and (>= (1- i) 1)(>= (- j 2) 1)(null (check-if-usable (list (1- i) (- j 2)) unusable)))(list (1- i) (- j 2)))(t nil))
           )
          )
  )
;;)

