;; Helper function to check if the input starts with a given prefix
(defun starts-with (input prefix)
  "Check if the input starts with the given prefix."
  (and (>= (length input) (length prefix))
       (string= (subseq input 0 (length prefix)) prefix)))

;; Parse non-terminal G
(defun parse-G (input)
  "Parse the non-terminal G."
  (cond
    ((starts-with input "x") (subseq input 1))
    ((starts-with input "y") (subseq input 1))
    ((starts-with input "z") (subseq input 1))
    ((starts-with input "w") (subseq input 1))
    (t nil)))

;; Parse non-terminal L
(defun parse-L (input)
  "Parse the non-terminal L."
  (if (starts-with input "s")
      (parse-L (subseq input 1))
      input))

;; Parse non-terminal S
(defun parse-S (input)
  "Parse the non-terminal S."
  (cond
    ((starts-with input "s") (subseq input 1))
    ((starts-with input "d")
     (let ((remaining (parse-L (subseq input 1))))
       (if (and remaining (starts-with remaining "b"))
           (subseq remaining 1)
           nil)))
    (t nil)))

;; Parse non-terminal E
(defun parse-E (input)
  "Parse the non-terminal E."
  (let ((remaining (parse-G input)))
    (if (and remaining (starts-with remaining "o"))
        (parse-E (subseq remaining 1))
        remaining)))

;; Parse non-terminal ES
(defun parse-ES (input)
  "Parse the non-terminal ES."
  (let ((remaining (parse-E input)))
    (if remaining
        (parse-S remaining)
        nil)))

;; Parse non-terminal I
(defun parse-I (input)
  "Parse the non-terminal I."
  (if (starts-with input "i")
      (let ((remaining (parse-ES (subseq input 1))))
        (if (and remaining (starts-with remaining "e"))
            (parse-S (subseq remaining 1))
            remaining))
      nil))

;; Test function to validate strings
(defun test-parser (input)
  "Test the parser with the given input."
  (if (parse-I input)
      (format t "~a is valid.~%" input)
      (format t "~a is invalid.~%" input)))

;; Test cases
(defun run-tests ()
  "Run test cases for valid and invalid strings."
  ;; Valid strings
  (test-parser "ixoyowdssbes")
  (test-parser "ixoys")
  (test-parser "iydsbs")
  (test-parser "izdssb")
  (test-parser "iwdsbs")
  (test-parser "ixoyowdssb")
  (test-parser "ixoyowdssbes")

  ;; Invalid strings
  (test-parser "ixoyowdssbe")
  (test-parser "ixoyowdssbex")
  (test-parser "ixoyowdssbse")
  (test-parser "ixoyowdssbeb")
  (test-parser "ixoyowdssbess")
  (test-parser "ixoyowdssbeds")
  (test-parser "ixoyowdssbese"))

;; Run the tests
(run-tests)