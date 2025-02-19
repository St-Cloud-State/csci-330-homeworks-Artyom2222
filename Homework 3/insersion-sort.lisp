(defun insert-sorted (x lst)
  (if (or (null lst) (< x (car lst)))  ;; Insert at correct position
      (cons x lst)
      (cons (car lst) (insert-sorted x (cdr lst)))))  ;; Keep checking

(defun insertion-sort (unsorted sorted)
  (if (null unsorted)  ;; Base case: Unsorted list is empty
      sorted
      (insertion-sort (cdr unsorted)  ;; Recursive call with remaining elements
                      (insert-sorted (car unsorted) sorted))))  ;; Insert first element
