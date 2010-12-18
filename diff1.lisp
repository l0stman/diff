(defstruct line
  (serial 0 :type fixnum)
  (hash 0 :type fixnum))

(defstruct eqv
  (serial 0 :type fixnum)
  (lastp t :type boolean))

(defun assoc-eqv-class (f1 f2)
  "Return (VALUES EQVS P) where EQVS is the equivalence classes of
lines in F2 and P is an array such that for each line I of F1, P[I] is
equal to the serial of the first line in an equivalence class equal to
line I, or 0 otherwise.  All the elements of an equivalence class are
equal."
  (let ((lines (make-array (length f2))))
    (dotimes (i (length f2))
      (setf (aref lines i)
            (make-line :serial (1+ i) :hash (sxhash (aref f2 i)))))
    (setq lines (sort lines
                      (lambda (l1 l2)
                        (or (< (line-hash l1) (line-hash l2))
                            (and (= (line-hash l1) (line-hash l2))
                                 (< (line-serial l1) (line-serial l2)))))))
    (let ((eqvs (make-array (1+ (length f2)))))
      (setf (aref eqvs 0) (make-eqv))
      (dotimes (i (length f2))
        (setf (aref eqvs (1+ i))
              (make-eqv :serial (line-serial (aref lines i))
                        :lastp (or (= i (1- (length f2)))
                                   (/= (line-hash (aref lines i))
                                       (line-hash (aref lines (1+ i))))))))
      (flet ((bsearch (h)
               ;; Find the first element of the equivalence class whose
               ;; hash is H.
               (do ((min 0) (max (1- (length lines))))
                   ((> min max) 0)
                 (let* ((mid (ash (+ min max) -1))
                        (d (- h (line-hash (aref lines mid)))))
                   (cond ((and (eqv-lastp (aref eqvs mid))
                               (zerop d))
                          (return-from bsearch (1+ mid)))
                         ((<= d 0) (setq max (1- mid)))
                         (t (setq min (1+ mid))))))))
        (let ((P (make-array (length f1))))
          (dotimes (i (length f1) (values eqvs P))
            (setf (aref P i)
                  (bsearch (sxhash (aref f1 i))))))))))

(defmacro when-bind ((var expr) &body body)
  `(let ((,var ,expr))
     (when ,var ,@body)))

(defstruct cand
  (serial1 0 :type fixnum)
  (serial2 0 :type fixnum)
  (previous nil :type (or cand null)))

;;; A k-candidate is pair of serials I and J such that the lines
;;; number I of FILE1 and number J of FILE2 are equals, the longest
;;; common subsequence of the first I lines of FILE1 and the first J
;;; lines of FILE2 has K elements and no common subsequence of length
;;; K exists when either I or J is reduced.

(defun mergec (cands k i eqvs p)
  "Add to the array of candidates CANDS all the r-candidates such that
0 <= R <= K and return the index of the last filled element of CANDS
which is initially equal to K.  I is the current index in FILE1 and
EQVS is the equivalence classes of FILE2.  P is the index in EQVS of
the first element of a class of lines in FILE2 equivalent to line I of
FILE1."
  (declare (optimize speed))
  (declare ((simple-array cand) cands))
  (labels ((serial2 (s)
             (cand-serial2 (aref cands s)))
           (bsearch (j min max)
             ;; Find an element S such that SERIAL2(S) < J and
             ;; SERIAL2(S+1) > J.
             (declare (fixnum min max))
             (loop
                (if (> min max)
                    (return-from bsearch nil)
                    (let ((mid (ash (+ min max) -1)))
                      (cond ((and (< (serial2 mid) j)
                                  (> (serial2 (1+ mid)) j))
                             (return-from bsearch mid))
                            ((>= (serial2 mid) j) (setq max (1- mid)))
                            (t (setq min (1+ mid)))))))))
    (declare (inline serial2))
    (declare ((simple-array eqv) eqvs))
    (let ((r 0)
          (c (aref cands 0)))           ; last r-candidate found
      (loop with j
         do
         (setq j (eqv-serial (aref eqvs p)))
         (when-bind (s (bsearch j r k))
           (psetf (aref cands r) c
                  r              (1+ s)
                  c              (make-cand :serial1 i
                                            :serial2 j
                                            :previous (aref cands s)))
           (when (= s k)
             (psetf (aref cands (+ k 2)) (aref cands (1+ k)) ; move fence
                    k                    (1+ k))
             (return)))
         (if (eqv-lastp (aref eqvs p)) (return) (incf p)))
      (setf (aref cands r) c))
    k))

(defun k-candidates (f1 f2)
  "Return \(VALUES CANDS K) where CANDS is the array r-candidates
for 0 <= r <= K."
  (multiple-value-bind (eqvs P) (assoc-eqv-class f1 f2)
    (let* ((len1 (length f1))
           (len2 (length f2))
           (cands (make-array (+ 2 (min len1 len2))))
           (k 0))
      (setf (aref cands 0) (make-cand)
            (aref cands 1) (make-cand :serial1 (1+ len1) :serial2 (1+ len2)))
      (dotimes (i len1 (values cands k))
        (when (plusp (aref P i))
          (setq k (mergec cands k (1+ i) eqvs (aref P i))))))))

(defun com-seq (f1 f2)
  "Return the longest common subsequence between the files."
  (multiple-value-bind (cands k) (k-candidates f1 f2)
    (let ((seq (make-array (+ (length f1) 2) :element-type 'fixnum)))
      (do ((c (aref cands k) (cand-previous c)))
          ((null c) (setf (aref seq (1+ (length f1))) (1+ (length f2))))
        (setf (aref seq (cand-serial1 c)) (cand-serial2 c)))
      ;; Weed out jackpots.
      (flet ((make-pos (i) (cons i (aref seq i))))
        (do ((len (1+ (length f1)))
             (res (list (make-pos 0)))
             (i 1 (1+ i)))
            ((= i len) (nreverse (cons (make-pos len) res)))
          (when (and (plusp (aref seq i))
                     (string= (aref f1 (1- i))
                              (aref f2 (1- (aref seq i)))))
            (push (make-pos i) res)))))))

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
