;;;; -*- mode: Lisp -*-

(asdf:defsystem #:rumcajsz-test
  :author "Gabor Melis"
  :version "0.0"
  :licence "MIT"
  :components ((:module "test"
                :serial t
                :components ((:file "package")
                             (:file "test"))))
  :depends-on (#:rumcajsz)
  :serial t)
