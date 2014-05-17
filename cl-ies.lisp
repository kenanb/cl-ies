;;;; cl-ies.lisp

(in-package #:cl-ies)



;;; ERROR CONDITIONS

(defvar +expected-ies-data+ '(("lamp-to-luminaire geometry")
			      ("# of pairs of angles and multiplying factors")
			      ("angles")
			      ("multiplying factors")
			      ("# of lamps" "lumens per lamp" "candela multiplier" 
			       "# of vertical angles" "# of horizontal angles" 
			       "photometric type" "units type" "width" "length" "height")
			      ("ballast factor" 
			       "ballast-lamp photometric factor / future use" 
			       "input watts")
			      ("vertical angles")
			      ("horizontal angles")))

(defvar *only-standards-compliant* t)

(defun line-assertion (line expected-length expected-line)
  (assert (equal (length line) expected-length) () 
	  "Malformed IES file! ~a value(s) are present where ~a were expected.~%~%Expected data:~%~{~:@(~a~)~%~}~%Data found: ~S~%" 
	  (length line) expected-length expected-line line))

(defun noncompliance-reaction (class slot)
  (let ((message (format nil "IES ~a Standard requires ~a keyword." class slot)))
    (if *only-standards-compliant*
	(error message)
	(warn message))))


;;; MATCH KEYWORDS

(defun match-header (string)
  (ppcre:register-groups-bind (format)
      ("IESNA:?(.*)" (string-right-trim '(#\Space #\Return) string)) format))

(defun match-body-revised (string)
  (ppcre:register-groups-bind (name value)
      ("\\[(.+)]\\s*(.*)" (string-right-trim '(#\Space #\Return) string))
    (let ((key (remove-if-not #'alphanumericp name)))
      (list (intern key "CL-IES") (intern key "KEYWORD") value))))

(defun match-body-86 (string)
        (string-trim '(#\Space #\Return) string))

(defun match-tilt (string)
  (ppcre:register-groups-bind (tilt)
      ("TILT=(.*)" (string-right-trim '(#\Space #\Return) string)) tilt))


;;; PARSE KEYWORDS

(defmacro concat-key (getvar value)
  `(setf ,getvar (concatenate 'string ,getvar " " ,value)))

(defun match-keyword-initials (triplet class slots last)
  (assert slots)
  (let ((slot (first triplet))
	(key (second triplet))
	(value (third triplet)))
    (cond
      ((or (equal key :block) (equal key :endblock)) nil)
      ((equal key :more) (if (class-has-slot-p (car last) class) 
			     (concat-key (getf slots (cadr last)) value)
			     (concat-key (getf (getf slots :keys) (cadr last)) value)))
      ((and (class-has-slot-p slot class) (not (equal key :keys))) 
       (if (getf slots key)
	   (concat-key (getf slots key) value)
	   (setf slots (nconc slots (list key value)))))
      (t (if (getf (getf slots :keys) key)
	     (concat-key (getf (getf slots :keys) key) value)
	     (setf (getf slots :keys) (nconc (getf slots :keys) (list key value))))))))

(defun parse-body-revised (stream iesna)
  (loop 
     with slot-list = (list :keys (list))
     and last-slot = nil
     for line = (read-line stream)
     and match = (match-body-revised line)
     and key = (car match)
     do 
       (match-keyword-initials match iesna slot-list last-slot)
       (when (not (equal (cadr match) :more)) (setf last-slot match))
     while (char= (char line 0) #\[)
     finally 
       (nconc slot-list (list :tilt (parse-tilt (match-tilt line) stream)))
       (setf (getf slot-list :keys) (cddr (getf slot-list :keys)))
       (return slot-list)))

(defun parse-body-86 (stream first-line)
  (if (match-tilt first-line)
      (list :tilt (match-tilt first-line))
  (loop
     with slot-list = (list :other (match-body-86 first-line))
     for line = (read-line stream)
     until (match-tilt line)
     do (concat-key (getf slot-list :other) (match-body-86 line))
     finally (nconc slot-list (list :tilt (parse-tilt (match-tilt line) stream)))
       (return slot-list))))

(defun parse-body (stream iesna first-line)
  (if (equal iesna 'lm-63-86)
      (parse-body-86 stream first-line)
      (parse-body-revised stream iesna)))


;;; PARSE DATA

(defun read-right-trimmed-line (stream)
  (string-right-trim '(#\Return #\Space) (read-line stream)))

(defun read-ies-line (stream)
  (loop with line = ""
       for str = (read-right-trimmed-line stream)
       do (setf line (concatenate 'string line str))
       while (char= (peek-char nil stream nil #\Newline) #\Space)
       finally (return line)))

(defun get-line-values (stream) (mapcar #'read-from-string 
					(remove "" (ppcre:split "[\\s*\\,]" 
								(read-ies-line stream)) 
						:test #'string=)))


;;; PARSE TILT DATA

(defun read-tilt (stream &optional (file nil))
  (apply #'make-instance 
	 (list 'tilt
	       :file (if file (file-namestring (pathname file)) nil)
	       :path (if file (directory-namestring (pathname file)) nil)
	       :lamp->luminaire (car (get-line-values stream))
	       :number-of-pairs (car (get-line-values stream))
	       :angles (get-line-values stream)
	       :multiplying-factors (get-line-values stream))))


(defun parse-tilt (tilt stream)
  (let ((tilt-full-path (if (string= (directory-namestring tilt) "") 
			    (pathname (concatenate 'string 
						   (directory-namestring stream) 
						   tilt))
			    tilt)))
  (string-case:string-case (tilt 
			    :default (with-open-file (tilt-stream tilt-full-path)
				       (read-tilt tilt-stream tilt-full-path)))
			   ("INCLUDE" (read-tilt stream))
			   ("NONE" nil))))


;;; PARSE PHOTOMETRIC DATA

(defun parse-values (stream iesna)
  (let* ((line-1 (get-line-values stream))
	 (line-2 (get-line-values stream))
	 (line-3 (get-line-values stream))
	 (line-4 (get-line-values stream))
	 (n-of-vertical-angles (elt line-1 3))
	 (n-of-horizontal-angles (elt line-1 4)))
(line-assertion line-1 (length (elt +expected-ies-data+ 4)) (elt +expected-ies-data+ 4))
(line-assertion line-2 (length (elt +expected-ies-data+ 5)) (elt +expected-ies-data+ 5))
(line-assertion line-3 n-of-vertical-angles (elt +expected-ies-data+ 6))
(line-assertion line-4 n-of-horizontal-angles (elt +expected-ies-data+ 6))
  (mapcan #'list
	  (list :number-of-lamps
		:lumens-per-lamp
		:candela-multiplier
		:number-of-v-angles
		:number-of-h-angles
		:photometric-type
		:units-type
		:width
		:length
		:height
		:ballast-factor
		(case iesna 
		  ('lm-63-86 :b-lamp-p-factor)
		  ('lm-63-91 :b-lamp-p-factor)
		  ('lm-63-95 :future-use)
		  ('lm-63-02 :future-use)
		  (t :future-use))
		:input-watts
		:vertical-angles
		:horizontal-angles
		:candela-values)
	  (append line-1 
		  line-2 
		  (list line-3) 
		  (list line-4)
		  (list (make-array (list n-of-horizontal-angles 
				    n-of-vertical-angles) 
			      :initial-contents 
			      (loop repeat n-of-horizontal-angles
				 collecting (get-line-values stream))))))))


;;; PARSE IES FILE

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
      (apply #'make-instance (append (list iesna
					   :file (file-namestring filepath)
					   :path (directory-namestring filepath))
				     (parse-body stream iesna first-line)
				     (parse-values stream iesna))))))
