;; Function to merge two sorted lists into one sorted list
(defun merge-lists (lst1 lst2)
  (cond
    ((null lst1) lst2)  ;; If lst1 is empty, return lst2
    ((null lst2) lst1)  ;; If lst2 is empty, return lst1
    ((<= (car lst1) (car lst2))  ;; Compare the first elements of each list
     (cons (car lst1) (merge-lists (cdr lst1) lst2)))  ;; Take lst1's first element and merge the rest
    (t  ;; Otherwise, take lst2's first element and merge the rest
     (cons (car lst2) (merge-lists lst1 (cdr lst2))))))

;; Function to partition a list into two nearly equal halves
(defun partition (lst left right)
  (cond
    ((null lst) (list left right))  ;; Base case: if lst is empty, return the two partitions
    ((null (cdr lst)) (list (cons (car lst) left) right))  ;; If there's only one element left, add it to left
    (t  ;; Otherwise, take two elements at a time: one for left and one for right
     (partition (cddr lst) (cons (car lst) left) (cons (cadr lst) right)))))

;; Recursive Mergesort function
(defun mergesort (lst)
  (if (or (null lst) (null (cdr lst)))  ;; Base case: if the list has 0 or 1 elements, it's already sorted
      lst
      (let* ((split (partition lst '() '()))  ;; Split the list into two halves
             (left (mergesort (car split)))  ;; Recursively sort the left half
             (right (mergesort (cadr split))))  ;; Recursively sort the right half
        (merge-lists left right))))  ;; Merge the sorted halves

