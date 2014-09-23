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
  :depends-on (#:alexandria #:cl-csv #:external-program #:mgl-example)
  :serial t)

(defmethod asdf:perform ((o asdf:test-op)
                         (c (eql (asdf:find-system '#:rumcajsz))))
  (asdf:oos 'asdf:load-op '#:rumcajsz-test)
  (funcall (intern (symbol-name '#:test) (find-package '#:rumcajsz-test))))

(defmethod asdf:operation-done-p ((o asdf:test-op)
                                  (c (eql (asdf:find-system '#:rumcajsz))))
  (values nil))
