(defun append-zeros (string count)
  (cond ((< count 1) string)
	(t (append-zeros (string-concat string "0") (- count 1)))))

(defun char-pairs (string1 string2 &optional (result nil) (count (length string1)))
  (cond ((< count (length string1)) result)
	((> count 1) (char-pairs (subseq string1 1)
				 (subseq string2 1)
				 (append result
					 (list (list (elt string1 0) (elt string2 0))))
				 (- count 1)))
	((= count 1) (append result
			     (list (list (elt string1 0) (elt string2 0)))))
	))
  
(defun levenshtein (string1 string2)
  (if (< (length string1) (length string2))
      (setf string1 (append-zeros string1 (- (length string2) (length string1))))
      (setf string2 (append-zeros string2 (- (length string1) (length string2)))))
  (reduce #'+ (mapcar #'(lambda (node) (if (char-equal (first node) (second node))
					   0
					   1))
		      (char-pairs string1 string2))))

(defun setup (root word distance)
  (let ((var (find-if #'(lambda (element) (= (second element) distance))
	       (get root 'neighbours))))
       (if var 
	   (setup (first var) word (levenshtein (string (first var)) (string word)))
	   (setf (get root 'neighbours)
		 (append (get root 'neighbours)
			 (list (list word (levenshtein (string root) (string word)))))))
       ))

(defvar *words*)

(defun cons-BK-trees (file)
  (let ((*words* nil)
	(root-word 'book))
       (with-open-file (word-stream file :direction :input)
		       (do ((word (read-line word-stream nil) (read-line word-stream nil)))
			   ((not word))
			   (setup root-word (intern word) (levenshtein (string root-word)
								       word))))
       ))

(defun search-word (root word &optional (distance (levenshtein (string root)
							       (string word))))
  (let ((var (find-if #'(lambda (element) (= (second element) distance))
	       (get root 'neighbours))))
       (cond ((string-equal (string (first var)) (string word))
	      (print word)
	      (print var))
	     ((eq var nil) 'no-word)
	     (t (search-word (first var) word (levenshtein (string (first var)) (string word)))))
       ))
	   
			 