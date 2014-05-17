;;;; class.lisp

(in-package #:cl-ies)


(defclass ies ()
  ((file
    :initarg :file
    :accessor ies-file)
   (path
    :initarg :path
    :accessor ies-path)
   (other
    :initarg :other
    :accessor information-of)
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

(defclass lm-63-revised (ies)
  ((test
    :initarg :test
    :accessor test-report-of)
   (testlab
    :initarg :testlab
    :accessor testing-laboratory-of)
   (manufac
    :initarg :manufac
    :accessor manufacturer-of)
   (lumcat
    :initarg :lumcat
    :accessor luminaire-catalog-of)
   (luminaire
    :initarg :luminaire
    :accessor luminaire-of)
   (lampcat
    :initarg :lampcat
    :accessor lamp-catalog-of)
   (lamp
    :initarg :lamp
    :accessor lamp-of)
   (ballastcat
    :initarg :ballastcat
    :accessor ballast-catalog-of)
   (ballast
    :initarg :ballast
    :accessor ballast-of)
   (maintcat
    :initarg :maintcat
    :accessor maintenance-category-of)
   (fixture
    :initarg :fixture
    :accessor fixture-of)
   (optic
    :initarg :optic
    :accessor optic-of)
   (align
    :initarg :align
    :accessor align-of)
   (watt
    :initarg :watt
    :accessor watt-of)
   (nearfield
    :initarg :nearfield
    :accessor near-field-of)
   (distribution
    :initarg :distribution
    :accessor distribution-of)
   (flasharea
    :initarg :flasharea
    :accessor flash-area-of)
   (colorconstant
    :initarg :colorconstant
    :accessor color-constant-of)
   (luminousgeometry
    :initarg :luminousgeometry
    :accessor luminous-geometry-of)
   (search
    :initarg :search
    :accessor search-keywords-of)
   (keys
    :initarg :keys
    :accessor user-defined-keywords-of)))

(defclass lm-63-86 (ies) 
  ((b-lamp-p-factor
    :initarg :b-lamp-p-factor
    :accessor b-lamp-p-factor-of)))

(defclass lm-63-91 (lm-63-revised) 
  ((date
    :initarg :date
    :accessor issue-date-of)
   (test
    :initform (noncompliance-reaction "LM-63-1991" "TEST (Test Report Number)"))
   (manufac
    :initform (noncompliance-reaction "LM-63-1991" "MANUFAC (Manufacturer)"))
   (b-lamp-p-factor
    :initarg :b-lamp-p-factor
    :accessor b-lamp-p-factor-of)))

(defclass lm-63-95 (lm-63-revised)
  ((date
    :initarg :date
    :accessor issue-date-of)
   (future-use
    :initarg :future-use
    :accessor future-use-of)))

(defclass lm-63-02 (lm-63-revised)
  ((issuedate
    :initarg :issuedate
    :accessor issue-date-of
    :initform (noncompliance-reaction "LM-63-2002" "ISSUEDATE (Issue Date)"))
   (test
    :initform (noncompliance-reaction "LM-63-2002" "TEST (Test Report Number)"))
   (testlab
    :initform (noncompliance-reaction "LM-63-2002" "TESTLAB (Testing Laboratory)"))
   (manufac
    :initform (noncompliance-reaction "LM-63-2002" "MANUFAC (Manufacturer)"))
   (lampposition
    :initarg :lampposition
    :accessor lamp-position-of)
   (future-use
    :initarg :future-use
    :accessor future-use-of)))

(defclass tilt ()
  ((file
    :initarg :file
    :initform nil
    :accessor tilt-file)
   (path
    :initarg :path
    :initform nil
    :accessor tilt-path)
   (lamp-to-luminaire-geometry
    :initarg :lamp->luminaire
    :accessor lamp->luminaire
    :initform nil)
   (number-of-pairs
    :initarg :number-of-pairs
    :initform nil
    :accessor number-of-pairs)
   (angles
    :initarg :angles
    :initform nil
    :accessor angles)
   (multiplying-factors
    :initarg :multiplying-factors
    :initform nil
    :accessor multiplying-factors)))

(loop for class in '(lm-63-86 lm-63-91 lm-63-95 lm-63-02)
     do (c2mop:finalize-inheritance (find-class class)))

(defun class-has-slot-p (slot-name class) 
  (when (find slot-name 
	      (c2mop:class-slots (find-class class)) 
	      :key #'c2mop:slot-definition-name) t))

(defun get-key (ies-instance key)
  "Returns the value of :KEY of IES-INSTANCE, where :KEY is a user-defined keyword."
  (assert (equal ies-instance 'lm-63-revised) () 
	  "IES file is type of ~S which does not support user defined keywords." 
	  ies-instance)
  (getf (user-defined-keywords-of ies-instance) key))

