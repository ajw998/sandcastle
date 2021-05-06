;; lovely-number.lisp

;; A lovely number is a number that contains fewer than n=3 instance of any distinct digits
;; For example 100 is lovely, because there are only 2 0s
;; 111 is considered not lovely, because there are 3 1s
(defun lovely-p (l &optional (m 3)) 
        (not (some #'(lambda (x) (>= (length x) m)) l)))

;; Count heuristic
;; Grouping characters
(defun group (l)
        (cond ((or (not (listp l)) (null l)) nil
                    (t (loop for x in (remove-duplicates l) collect (remove-if #'(lambda (j) (not (eql j x))) l))))))

(defun split-number (n)
        (coerce (write-to-string n) 'list))

(defun lovely-range (s l &optional (h #'group))
        "Given range s to l, return all the lovely numbers"
        (loop for x from s to l
                        when (lovely-p (funcall h (split-number x))) collect x))

(defun lovely (s l &optional (h #'group))
        (cond ((or (minusp s) (minusp l) (> s l)) nil
                    (t (length (lovely-range s l h))))))
