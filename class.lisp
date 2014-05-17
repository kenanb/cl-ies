;;;; class.lisp

(in-package :cl-ies)


(defun missing-keyword (class key)
  (let ((message (format nil "IES ~a Standard requires ~a keyword." class key)))
    (if *only-standards-compliant*
	(error message)
	(warn message))))

(defclass ies ()
  ((file
    :initarg :file
    :accessor ies-file
    :type string)
   (path
    :initarg :path
    :accessor ies-path
    :type string)
   (info
    :initarg :info
    :accessor info-of
    :type list)
   (tilt
    :initarg :tilt
    :accessor tilt-of
    :type tilt)
   (number-of-lamps
    :initarg :number-of-lamps
    :accessor number-of-lamps-of
    :type unsigned-byte)
   (lumens-per-lamp
    :initarg :lumens-per-lamp
    :accessor lumens-per-lamp-of
    :type (real -1 *))
   (candela-multiplier
    :initarg :candela-multiplier
    :accessor candela-multiplier-of
    :type real)
   (number-of-v-angles
    :initarg :number-of-v-angles
    :accessor number-of-v-angles-of
    :type unsigned-byte)
   (number-of-h-angles
    :initarg :number-of-h-angles
    :accessor number-of-h-angles-of
    :type unsigned-byte)
   (photometric-type
    :initarg :photometric-type
    :accessor photometric-type-of
    :type (integer 1 3))
   (units-type
    :initarg :units-type
    :accessor units-type-of
    :type (integer 1 2))
   (width
    :initarg :width
    :accessor width-of
    :type real)
   (length
    :initarg :length
    :accessor length-of
    :type real)
   (height
    :initarg :height
    :accessor height-of
    :type real)
   (ballast-factor
    :initarg :ballast-factor
    :accessor ballast-factor-of
    :type real)
   (future-use
    :initarg :future-use
    :accessor future-use-of)
   (input-watts
    :initarg :input-watts
    :accessor input-watts-of
    :type real)
   (vertical-angles
    :initarg :vertical-angles
    :accessor vertical-angles-of
    :type list)
   (horizontal-angles
    :initarg :horizontal-angles
    :accessor horizontal-angles-of
    :type list)
   (candela-values
    :initarg :candela-values
    :accessor candela-values-of
    :type array)))

(defclass lm-63-86 (ies) 
  ((info
    :type string)))

(defclass lm-63-91 (ies) ())

(defmethod initialize-instance :after ((instance lm-63-91) &key)
  (dolist (key '(:test :manufac))
  (unless (member key (info-of instance)) (missing-keyword "LM-63-1991" key))))

(defclass lm-63-95 (ies) ())
(defclass lm-63-02 (ies) ())

(defmethod initialize-instance :after ((instance lm-63-02) &key)
  (dolist (key '(:test :testlab :issuedate :manufac))
  (unless (member key (info-of instance)) (missing-keyword "LM-63-2002" key))))

(defclass tilt ()
  ((file
    :initarg :file
    :accessor tilt-file
    :type string)
   (path
    :initarg :path
    :accessor tilt-path
    :type string)
   (lamp-to-lum-geometry
    :initarg :lamp-to-lum-geometry
    :accessor lamp-to-lum-geometry-of
    :type (integer 1 3))
   (number-of-angles
    :initarg :number-of-angles
    :accessor number-of-angles-of
    :type unsigned-byte)
   (angles
    :initarg :angles
    :accessor angles-of
    :type list)
   (multiplying-factors
    :initarg :multiplying-factors
    :accessor multiplying-factors-of
    :type list)))
