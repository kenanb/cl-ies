;;;; cl-ies.asd

(asdf:defsystem #:cl-ies
  :serial t
  :depends-on (#:cl-ppcre #:closer-mop #:string-case)
  :components ((:file "package")
	       (:file "class")
               (:file "cl-ies")))

