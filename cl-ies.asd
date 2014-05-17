;;;; cl-ies.asd

(asdf:defsystem #:cl-ies
  :serial t
  :depends-on (#:cl-ppcre #:alexandria #:string-case)
  :components ((:file "package")
	       (:file "data")
	       (:file "class")
	       (:file "parser")
               (:file "cl-ies")))

