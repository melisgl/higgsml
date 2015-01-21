;;;; -*- mode: Lisp -*-

(asdf:defsystem #:rumcajsz
  :author "Gábor Melis"
  :version "0.0"
  :licence "MIT"
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "config")
                             (:file "higgs-boson")
                             (:file "csv")
                             (:file "encoder")
                             (:file "bpn")
                             (:file "xgboost")
                             (:file "main"))))
  :depends-on (#:alexandria #:cl-csv #:external-program #:mgl)
  :serial t)

(defmethod asdf:perform ((o asdf:test-op)
                         (c (eql (asdf:find-system '#:rumcajsz))))
  (asdf:oos 'asdf:load-op '#:rumcajsz/test)
  (funcall (intern (symbol-name '#:test) (find-package '#:rumcajsz-test))))

(asdf:defsystem #:rumcajsz/test
  :author "Gabor Melis"
  :version "0.0"
  :licence "MIT"
  :components ((:module "test"
                :serial t
                :components ((:file "package")
                             (:file "test"))))
  :depends-on (#:rumcajsz)
  :serial t)
