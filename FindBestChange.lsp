;;;(critique
(defun find-best-change (amt coinsdenom)
  (cond
   ((zerop (car (remove-duplicates coinsdenom))) (print 'InvalidInput))
   ((null coinsdenom) (print 'InvalidInput))
   (t
    (let ((sortedcoins (sort (remove-duplicates coinsdenom) #'>)))  
      (calculate-change amt (car sortedcoins) sortedcoins 0 nil nil amt)
      )
    )
   )
)
;;;)

;;(critique
 (defun calculate-change (amt denom alldenom n currentstate solution totalamt)    
   (cond   
    ((null denom) (compare-change currentstate solution totalamt))
    ((>= (- amt denom) 0)
     (let 
         ((sol (calculate-change (- amt denom) denom alldenom (1+ n) currentstate solution totalamt)))
       (cond 
        ((and
          (null (cadr alldenom))
          (< amt denom)
          ) solution)
        (t
           (calculate-change amt (cadr alldenom) (cdr alldenom) 0 (cons (n-elts denom n) currentstate) sol totalamt)
         )
        )
       )
     )
     (t (calculate-change amt (cadr alldenom) (cdr alldenom) 0 (cons (n-elts denom n) currentstate) solution totalamt))
     )
    )
;; )

;;; (critique
  (defun compare-change (a b totalamt)
    (cond 
     ((null b) a)
     (t
      (let ((totala (calculate-total a))
            (totalb (calculate-total b))
            (coinsa (calculate-coins a))
            (coinsb (calculate-coins b)))
        (calculate-best-option a b totala totalb coinsa coinsb totalamt)
        )
      )
     )
    )
 ;;; )

 ;;;(critique
  (defun calculate-best-option (a b totala totalb coinsa coinsb totalamt)
    (cond 
     ((and 
       (= totala totalamt)
       (= totalb totalamt)
       )(cond
         ((< coinsa coinsb) a)
         (t b))
      )
     ((= totala totalamt) a)
     ((= totalb totalamt) b)
     ((= (- totalamt totala) (- totalamt totalb))
      (cond 
       ((< coinsa coinsb) a)
       (t b))
      )
     ((> (- totalamt totala) (- totalamt totalb)) b)
     (t a)
     )
    )
  ;;;)


 ;;;(critique
  (defun calculate-total (temp)
    (cond
     ((null temp) 0)
     (t
      (+ (* (car (car temp)) (cadr (car temp))) (calculate-total (cdr temp))))
     )
    ) 
 ;;; )

 ;;;(critique
  (defun calculate-coins (temp)
    (cond
     ((null temp) 0)
     (t
      (+ (car (car temp)) (calculate-coins (cdr temp)))
      )
     )
    )
 ;;; )

 ;;;(critique
  (defun check-if-member (mem list)
    (cond 
     ((null list) nil)
     ((eql mem (car list)) t)
     (t (check-if-member mem (cdr list))) 
     )
    )
 ;;; )
 
 ;;;(critique    
  (defun n-elts (elt n)
    (list n elt)
    )
  ;;;)







     
