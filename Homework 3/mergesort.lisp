;; Step 1: Partition Function
(defun partition (lst)
  (if (<= (length lst) 1)
      (values lst nil)  ;; If the list has 0 or 1 item, it's already sorted
      (let* ((mid (floor (length lst) 2))
             (first-half (subseq lst 0 mid))
             (second-half (subseq lst mid)))
        (values first-half second-half))))

;; Step 2: Merge Function
(defun my-merge (left right)
  (cond ((null left) right)
        ((null right) left)
        ((< (car left) (car right)) 
         (cons (car left) (my-merge (cdr left) right)))
        (t 
         (cons (car right) (my-merge left (cdr right))))))

;; Step 3: Mergesort Function
(defun mergesort (lst)
  (if (or (null lst) (null (cdr lst))) 
      lst  ;; Base case: If the list has 0 or 1 element, it's already sorted
      (multiple-value-bind (left right) (partition lst)
        (my-merge (mergesort left) (mergesort right)))))
