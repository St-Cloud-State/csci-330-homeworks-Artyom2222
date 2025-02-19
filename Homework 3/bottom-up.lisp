;; Step 1: Partition the list into sorted pairs
(defun partition-into-pairs (lst)
  (if (null lst)
      nil
      (if (null (cdr lst))
          (list (list (car lst)))  ;; Single element remains
          (cons (sort (list (car lst) (cadr lst)) #'<)  ;; Sort the pair
                (partition-into-pairs (cddr lst))))))  ;; Recur with the rest

;; Step 2: Merge function (reused from previous problem)
(defun my-merge (left right)
  (cond ((null left) right)
        ((null right) left)
        ((< (car left) (car right)) 
         (cons (car left) (my-merge (cdr left) right)))
        (t 
         (cons (car right) (my-merge left (cdr right))))))

;; Step 3: Iteratively merge adjacent lists
(defun merge-pairs (lists)
  (if (null lists)
      nil
      (if (null (cdr lists))
          lists  ;; If there's only one list left, return it
          (cons (my-merge (car lists) (cadr lists))
                (merge-pairs (cddr lists))))))  ;; Recur with the rest

;; Step 4: Bottom-up Mergesort using the above steps
(defun bottom-up-mergesort (lst)
  (let ((sorted-pairs (partition-into-pairs lst)))
    (loop while (> (length sorted-pairs) 1) do
      (setf sorted-pairs (merge-pairs sorted-pairs)))
    (car sorted-pairs)))  ;; Return the final sorted list


 