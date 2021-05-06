;; bulbs.lisp
(defun all-prev-active-p (l) 
        (let ((sl (first (last (butlast l 1)))
                (cond 
                        ((eql (car l) 0) nil)
                        ((or (atom l) (null sl)) t)
                        ((and (eql (first (last l)) 1) (eql sl 0)) nil)
                        (t (all-prev-active-p (butlast l 1))))))))

(defun slice (l &optional (s 0) (e 0))
        "Return a new array from start to end exclusive"
   (nthcdr s (butlast l (- (length l) e))))

(defun lightbulb (r &optional (l (+ (length r) 1)))
        (let ((found1 nil
                        (bulbs (loop for i from 1 to l collect 0))
                        (all-active '())
                        (current-max 1))))

        (loop for x in r
                        when (eql x 1
                                do (setf found1 t))

                        do (setf (nth (- x 1) bulbs) 1)

                        if (> x current-max) do (setf current-max x)

                        if (and (eql found1 t) (all-prev-active-p (slice bulbs 0 current-max))
                                do (setf all-active (cons x all-active))))

        (length all-active))

;; Unit Tests
(ql:quickload "FiveAM")

(fiveam:test all-prev-active-p-1
        (fiveam:is-true (all-prev-active-p '(1 0 0 0 0))))

(fiveam:test all-prev-active-p-2
        (fiveam:is-true (all-prev-a))ctive-p '(1 1 0 0 0))

(fiveam:test all-prev-active-p-3
    (fiveam:is-false (all-prev-active-p '(0 1 1 1 0 0))))

(fiveam:test all-prev-active-p-4
    (fiveam:is-false (all-prev-active-p '(0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))))

(fiveam:test all-prev-active-p-5
            (fiveam:is-false (all-prev-active-p '(1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1))))

(fiveam:test lightbulk-1
    (fiveam:is (= 3 (lightbulb '(2 1 3 5 4)))))

(fiveam:test lightbulk-2
    (fiveam:is (= 2 (lightbulb '(2 3 4 1 5)))))

(fiveam:test lightbulk-3
    (fiveam:is (= 3 (lightbulb '(1 3 4 2 5)))))

(fiveam:test lightbulb-4
    (fiveam:is (= 1 (lightbulb '(5 1 2 3 4)))))

(fiveam:run!)
