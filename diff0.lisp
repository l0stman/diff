(defun com-seq (f1 f2)
  "Find the indexes of the longest common subsequence between the
files f1 and f2 represented as arrays using simple recursion.  This
algorithm is O(mn) in time and in space."
  (labels ((iter (i j)
             (cond ((or (minusp i) (minusp j))
                    (values (list (cons 0 0))
                            0))
                   ((string= (aref f1 i) (aref f2 j))
                    (multiple-value-bind (seq len) (iter (1- i) (1- j))
                      (values (cons (cons (1+ i) (1+ j))
                                    seq)
                              (1+ len))))
                   (t (multiple-value-bind (seq1 len1) (iter (1- i) j)
                        (multiple-value-bind (seq2 len2) (iter i (1- j))
                          (if (< len1 len2)
                              (values seq2 len2)
                              (values seq1 len1))))))))
    (let ((len1 (length f1)) (len2 (length f2)))
     (nreverse
      (cons (cons (1+ len1) (1+ len2))
            (iter (1- len1) (1- len2)))))))

(defmacro aif (pred then &optional else)
  `(let ((it ,pred))
     (if it ,then ,else)))

(defun file->arr (file)
  (with-open-file (in file)
    (loop with arr = (make-array 0 :adjustable t :fill-pointer 0)
       do (aif (read-line in nil)
               (vector-push-extend it arr)
               (return arr)))))

(defun diff (file1 file2)
  "Compare files line by line."
  (let ((f1 (file->arr file1))
        (f2 (file->arr file2)))
    (flet ((print-lines (file from to prefix)
             (loop for i from (1- from) to (1- to)
                do (format t "~C ~A~%" prefix (aref file i))))
           (print-header (cmd from1 to1 from2 to2)
             (format t "~D~@[,~D~]~C~D~@[,~D~]~%"
                     from1
                     (when (< from1 to1) to1)
                     cmd
                     from2
                     (when (< from2 to2) to2))))
      (loop for ((pos1 . pos2) (next1 . next2)) on (com-seq f1 f2)
         when next1
         do
         (cond ((= (1+ pos1) next1)
                (when (< (1+ pos2) next2)
                  (print-header #\a pos1 pos1 (1+ pos2) (1- next2))
                  (print-lines f2 (1+ pos2) (1- next2) #\>)))
               ((= (1+ pos2) next2)
                (when (< (1+ pos1) next1)
                  (print-header #\d (1+ pos1) (1- next1) pos2 pos2)
                  (print-lines f1 (1+ pos1) (1- next1) #\<)))
               (t
                (print-header #\c (1+ pos1) (1- next1) (1+ pos2) (1- next2))
                (print-lines f1 (1+ pos1) (1- next1) #\<)
                (format t "---~%")
                (print-lines f2 (1+ pos2) (1- next2) #\>)))))))
