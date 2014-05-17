;;;; class.lisp

(in-package :cl-ies)


(defun missing-keyword (class slot)
  (let ((message (format nil "IES ~a Standard requires ~a keyword." class slot)))
    (if *only-standards-compliant*
	(error message)
	(warn message))))

(defclass ies ()
  ((file
    :initarg :file
    :accessor ies-file)
   (path
    :initarg :path
    :accessor ies-path)
   (info
    :initarg :info
    :accessor info-of)
   (tilt
    :initarg :tilt
    :accessor tilt-of)
   (number-of-lamps
    :initarg :number-of-lamps
    :accessor number-of-lamps-of)
   (lumens-per-lamp
    :initarg :lumens-per-lamp
    :accessor lumens-per-lamp-of)
   (candela-multiplier
    :initarg :candela-multiplier
    :accessor candela-multiplier-of)
   (number-of-v-angles
    :initarg :number-of-v-angles
    :accessor number-of-v-angles-of)
   (number-of-h-angles
    :initarg :number-of-h-angles
    :accessor number-of-h-angles-of)
   (photometric-type
    :initarg :photometric-type
    :accessor photometric-type-of)
   (units-type
    :initarg :units-type
    :accessor units-type-of)
   (width
    :initarg :width
    :accessor width-of)
   (length
    :initarg :length
    :accessor length-of)
   (height
    :initarg :height
    :accessor height-of)
   (ballast-factor
    :initarg :ballast-factor
    :accessor ballast-factor-of)
   (future-use
    :initarg :future-use
    :accessor future-use-of)
   (input-watts
    :initarg :input-watts
    :accessor input-watts-of)
   (vertical-angles
    :initarg :vertical-angles
    :accessor vertical-angles-of)
   (horizontal-angles
    :initarg :horizontal-angles
    :accessor horizontal-angles-of)
   (candela-values
    :initarg :candela-values
    :accessor candela-values-of)))

(defclass tilt ()
  ((file
    :initarg :file
    :accessor tilt-file)
   (path
    :initarg :path
    :accessor tilt-path)
   (lamp-to-lum-geometry
    :initarg :lamp-to-lum-geometry
    :accessor lamp-to-lum-geometry-of)
   (number-of-angles
    :initarg :number-of-angles
    :accessor number-of-angles-of)
   (angles
    :initarg :angles
    :accessor angles-of)
   (multiplying-factors
    :initarg :multiplying-factors
    :accessor multiplying-factors-of)))
