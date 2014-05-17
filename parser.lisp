;;;; parser.lisp

(in-package :cl-ies)


(defun assert-data (data-type data expected-length expected-data)
  (assert (equal (length data) expected-length) () 
	  "Malformed ~a! ~a value(s) are present where ~a were expected.~%~%Expected data:~%~{~s~%~}~%Data found: ~S~%" 
	  data-type (length data) expected-length expected-data data))

(defun assert-ies (data expected-length elt)
  (assert-data "IES file" data expected-length (elt +iesna-lm-63-photometry-data+ elt)))

(defun assert-tilt (data expected-length elt)
  (assert-data "TILT data" data expected-length (elt +iesna-lm-63-tilt-data+ elt)))

(defun match-header (string)
  (ppcre:register-groups-bind (format)
      ("IESNA:?(.*)" (string-right-trim '(#\Space #\Return) string)) format))

(defun match-tilt (string)
  (ppcre:register-groups-bind (tilt)
      ("TILT=(.*)" (string-right-trim '(#\Space #\Return) string)) tilt))

(defun parse-tilt (stream &optional (file nil))
  (let* ((lamp-to-lum-geometry (car (get-line-values stream)))
	 (number-of-angles (car (get-line-values stream)))
	 (angles (get-line-values stream))
	 (multiplying-factors (get-line-values stream)))
    (assert-tilt angles number-of-angles 2)
    (assert-tilt multiplying-factors number-of-angles 3)
    (apply #'make-instance 
	   (cons 'tilt
		 (mapcan #'list 
			 `(:file :path ,@(remove-if-not #'keywordp (alexandria:flatten +iesna-lm-63-tilt-data+)))
			 (list
			  (if file (file-namestring (pathname file)) nil)
			  (if file (directory-namestring (pathname file)) nil)
			  lamp-to-lum-geometry
			  number-of-angles
			  angles
			  multiplying-factors))))))

(defun get-tilt-data (tilt stream)
  (let ((tilt-full-path (if (string= (directory-namestring tilt) "") 
			    (pathname (concatenate 'string 
						   (directory-namestring stream) 
						   tilt))
			    tilt)))
    (string-case:string-case (tilt 
			      :default (with-open-file (tilt-stream tilt-full-path)
					 (parse-tilt tilt-stream tilt-full-path)))
      ("INCLUDE" (parse-tilt stream))
      ("NONE" nil))))

(defun match-keyword (string)
  (ppcre:register-groups-bind (name value)
      ("\\[(.*?)]\\s*(.*)" (string-right-trim '(#\Space #\Return) string))
    (list (intern (remove-if-not #'alphanumericp name) "KEYWORD") value)))

(defun parse-keywords (stream)
  (loop with keyword-list
        for line = (read-line stream)
        for (key-raw value) = (match-keyword line)
        for key = key-raw then (if (eq key-raw :more) last-key key-raw)
        for last-key = key
        while key
        unless (member key '(:block :endblock))
        do
        (setf (getf keyword-list key)
              (format nil "~@[~a ~]~a" (getf keyword-list key) value))
        finally 
        (return (list :info keyword-list :tilt (get-tilt-data (match-tilt line) stream)))))

(defun parse-labels (stream first-line)
    (if (match-tilt first-line)
	(list :tilt (get-tilt-data (match-tilt first-line) stream))
	(loop
	   with info = (string-trim '(#\Space #\Return) first-line)
	   for line = (read-line stream)
	   until (match-tilt line)
	   do (setf info (concatenate 'string info " " (string-trim '(#\Space #\Return) line)))
	   finally (return (list :info info :tilt (get-tilt-data (match-tilt line) stream))))))

(defun parse-info (stream iesna first-line)
  (if (equal iesna 'lm-63-86)
      (parse-labels stream first-line)
      (parse-keywords stream)))

(defun read-trim-right (stream)
  (string-right-trim '(#\Return #\Space) (read-line stream)))

(defun read-ies-line (stream)
  (loop with line = ""
     for str = (read-trim-right stream)
     do (setf line (concatenate 'string line str))
     while (char= (peek-char nil stream nil #\Newline) #\Space)
     finally (return line)))

(defun get-line-values (stream) (mapcar #'read-from-string 
					(remove "" (ppcre:split "[\\s*\\,]" 
								(read-ies-line stream)) 
						:test #'string=)))

(defun parse-values (stream)
  (let* ((first-line (get-line-values stream))
	 (second-line (get-line-values stream))
	 (v-angles (get-line-values stream))
	 (h-angles (get-line-values stream))
	 (n-of-vertical-angles (elt first-line 3))
	 (n-of-horizontal-angles (elt first-line 4)))
    (assert-ies first-line (length (elt +iesna-lm-63-photometry-data+ 0)) 0)
    (assert-ies second-line (length (elt +iesna-lm-63-photometry-data+ 1)) 1)
    (assert-ies v-angles n-of-vertical-angles 2)
    (assert-ies h-angles n-of-horizontal-angles 3)
    (mapcan #'list
	    (remove-if-not #'keywordp (alexandria:flatten +iesna-lm-63-photometry-data+))
	    `(,@first-line
	      ,@second-line
	      ,v-angles
	      ,h-angles
	      ,(make-array (list n-of-horizontal-angles 
				 n-of-vertical-angles) 
			   :initial-contents 
			   (loop repeat n-of-horizontal-angles
			      collecting (get-line-values stream)))))))

(defun parse-ies (file)
  (with-open-file (stream (pathname file)) 
    (let* ((filepath (pathname file))
	   (first-line (read-line stream))
	   (header (match-header first-line))
	   (iesna (cond 
		    ((equal header nil) 'lm-63-86)
		    ((string= header "91") 'lm-63-91)
		    ((string= header "LM-63-1995") 'lm-63-95)
		    ((string= header "LM-63-2002") 'lm-63-02)
		    (t (warn "IES file is malformed or {IES Standard Revision}>2002 compliant and not yet supported! Assuming LM-63-2002.") 'lm-63-02))))
      (apply #'make-instance (append (list 'ies
					   :file (file-namestring filepath)
					   :path (directory-namestring filepath))
				     (parse-info stream iesna first-line)
				     (parse-values stream))))))
