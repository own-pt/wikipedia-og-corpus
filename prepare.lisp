
(ql:quickload '(:alexandria :cl-ppcre))

(defstruct doc id url title lines)

(defun getlines (xml-file)
  (with-open-file (in xml-file)
    (loop for line = (read-line in nil nil)
	  while line
	  collect line)))


(defun split (lines doc result)
  (let ((re "<doc id=\"([^\"]+)\" url=\"([^\"]+)\" title=\"([^\"]+)\">")
	(line (car lines)))
    (cond
      ((null lines) (reverse result))

      ((cl-ppcre:scan "</doc>" line)
       (setf (doc-lines doc)
	     (reverse (doc-lines doc)))
       (split (cdr lines) nil (cons doc result)))

      ((cl-ppcre:scan re line)
       (multiple-value-bind (s e gs ge)
	   (cl-ppcre:scan re line)
	 (declare (ignore s e))
	 (let ((my (make-doc :id (subseq line (aref gs 0) (aref ge 0))
			      :url (subseq line (aref gs 1) (aref ge 1))
			      :title (subseq line (aref gs 2) (aref ge 2))
			      :lines nil)))
	   (split (cdr lines) my result))))

      (t (push line (doc-lines doc))
	 (split (cdr lines) doc result)))))


(defun main (xml-file)
  (let ((docs (split (getlines xml-file) nil nil)))
    (dolist (d docs)
      (with-open-file (out (format nil "out/~a.txt" (doc-id d)) :direction :output :if-exists :supersede)
	(format out "~{~a~%~}" (doc-lines d))))))


(defun get-links (file-in file-out)
  (let ((input (alexandria:read-file-into-string file-in)))
    (with-open-file (out file-out :direction :output :if-exists :supersede)
      (cl-ppcre:do-scans (s e rs re "<a href=\"([^\"]+)\">([^<]+)</a>" input)
	(format out "~a,~a~%" 
		(subseq input (aref rs 0) (aref re 0))
		(subseq input (aref rs 1) (aref re 1)))))))

