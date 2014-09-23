(in-package :higgs-boson)

(defun parse-config-line (string)
  (let ((position (position #\= string)))
    (assert position)
    (cons (subseq string 0 position)
          (let ((*read-eval* nil))
            (read-from-string (subseq string (1+ position)))))))

(defun parse-config (stream)
  (loop for line = (read-line stream nil nil)
        while line
        collect (parse-config-line line)))

(defun load-config ()
  (with-open-file (stream (asdf:system-relative-pathname :rumcajsz "SETTINGS"))
    (parse-config stream)))

(defparameter *config* (load-config))

(defun lookup-config (key)
  (or (cdr (assoc key *config* :test #'string=))
      (error "Unknown key in config: ~S" key)))

(defun lookup-config-path (key)
  (asdf:system-relative-pathname :rumcajsz (lookup-config key)))

(defparameter *data-dir* (lookup-config-path "DATADIR"))
(defparameter *model-dir* (lookup-config-path "MODELDIR"))
(defparameter *submission-dir* (lookup-config-path "SUBMISSIONDIR"))

(defparameter *training-file* (merge-pathnames "training.csv" *data-dir*))
(defparameter *test-file* (merge-pathnames "test.csv" *data-dir*))
