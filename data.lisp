;;;; data.lisp

(in-package :cl-ies)


(defvar +iesna-lm-63-keyword-data+ '(:date ("ISSUE DATE")
				     :issuedate ("ISSUE DATE")
				     :test ("TEST REPORT")
				     :testlab ("TESTING LABORATORY")
				     :manufac ("MANUFACTURER")
				     :lumcat ("LUMINAIRE CATALOG")
				     :luminaire ("LUMINAIRE")
				     :lampcat ("LAMP CATALOG")
				     :lamp ("LAMP")
				     :ballastcat ("BALLAST CATALOG")
				     :ballast ("BALLAST")
				     :maintcat ("MAINTENANCE CATEGORY")
				     :fixture ("FIXTURE")
				     :optic ("OPTIC")
				     :align ("ALIGNMENT")
				     :watt ("WATT")
				     :nearfield ("NEAR FIELD")
				     :distribution ("DISTRIBUTION")
				     :flasharea ("FLASH AREA")
				     :colorconstant ("COLOR CONSTANT")
				     :luminousgeometry ("LUMINOUS GEOMETRY")
				     :lampposition ("LAMP POSITION")
				     :search ("SEARCH KEYWORDS")
				     :other ("OTHER")))

(defvar +iesna-lm-63-tilt-data+ '(((:lamp-to-lum-geometry . "LAMP-TO-LUMINAIRE GEOMETRY"))
				  ((:number-of-angles . "NUMBER OF ANGLES AND MULTIPLYING FACTORS"))
				  ((:angles . "ANGLES"))
				  ((:multiplying-factors . "MULTIPLYING FACTORS"))))

(defvar +iesna-lm-63-photometry-data+ '(((:number-of-lamps . "NUMBER OF LAMPS") 
					 (:lumens-per-lamp . "LUMENS PER LAMP") 
					 (:candela-multiplier . "CANDELA MULTIPLIER") 
					 (:number-of-v-angles . "NUMBER OF VERTICAL ANGLES") 
					 (:number-of-h-angles . "NUMBER OF HORIZONTAL ANGLES") 
					 (:photometric-type . "PHOTOMETRIC TYPE") 
					 (:units-type . "UNITS TYPE") 
					 (:width . "WIDTH") 
					 (:length . "LENGTH") 
					 (:height . "HEIGHT"))
					((:ballast-factor . "BALLAST FACTOR") 
					 (:future-use . "FUTURE USE") 
					 (:input-watts . "INPUT WATTS"))
					((:vertical-angles . "VERTICAL ANGLES"))
					((:horizontal-angles . "HORIZONTAL ANGLES"))
					((:candela-values . "CANDELA VALUES"))))