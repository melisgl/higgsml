(in-package :higgs-boson)

(defvar *training-examples*)

(defun training-examples ()
  (if (boundp '*training-examples*)
      *training-examples*
      (setq *training-examples*
            (if *training-file*
                (slurp-csv *training-file*)
                nil))))

(defvar *test-examples*)

(defun test-examples ()
  (if (boundp '*test-examples*)
      *test-examples*
      (setq *test-examples*
            (if *test-file*
                (slurp-csv *test-file*)
                nil))))


(defun map-csv-records (fn filename &key skip-header header-p-fn)
  (let ((i 0)
        (cl-csv:*default-external-format* :latin1)
        (filename (merge-pathnames (pathname filename)))
        (result ()))
    (handler-case
        (with-open-file (stream filename)
          (loop
            (let ((file-position (file-position stream))
                  (record (cl-csv:read-csv-row stream)))
              (when (zerop (mod i 10000))
                (mgl:log-msg "Read ~S csv records.~%" i))
              (incf i)
              (cond ((or (/= i 1)
                         (and (= i 1)
                              (eq skip-header :maybe)
                              (not (funcall header-p-fn record))))
                     (push (funcall fn record filename file-position)
                           result))
                    (t
                     (when header-p-fn
                       (assert (funcall header-p-fn record))))))))
      (end-of-file () nil))
    (reverse result)))

(defun slurp-csv (filename)
  (map-csv-records #'csv-record-to-example filename
                   :skip-header t :header-p-fn #'csv-header-p))

(defun parse-label (string)
  (let ((pos (position string *label-names* :test #'string=)))
    (assert pos)
    (elt *labels* pos)))

(defun parse-float (string)
  (let* ((*read-eval* nil)
         (*read-default-float-format* 'double-float)
         (x (read-from-string string)))
    (float x 0d0)))

(defun make-feature-vector ()
  (make-array *n-features* :element-type 'mgl-util:flt
              :initial-element (mgl-util:flt 0)))

(defun parse-feature-vector (features)
  (let ((v (make-feature-vector)))
    (loop for i upfrom 0
          for feature in features
          do (setf (aref v i) (parse-float feature)))
    (array-to-mat v :ctype *default-mat-ctype*)))

(defun csv-record-to-example (record filename file-position)
  (declare (ignore filename file-position))
  (let* ((n (length record))
         (event-id (parse-integer (first record))))
    (cond ((find-example-by-event-id event-id :errorp nil))
          ((= n (+ 1 *n-features*))
           (setf (gethash event-id *event-id-to-example*)
                 (make-example
                  :event-id event-id
                  :features (parse-feature-vector (subseq record 1)))))
          ((= n (+ 3 *n-features*))
           (setf (gethash event-id *event-id-to-example*)
                 (make-example
                  :event-id event-id
                  :features (parse-feature-vector
                             (subseq record 1 (+ 1 *n-features*)))
                  :weight (parse-float (elt record (+ 1 *n-features*)))
                  :label (parse-label (elt record (+ 2 *n-features*))))))
          (t
           (error "Unexpected number of columns.")))))

(defun csv-header-p (record)
  (let ((x (first record)))
    (and (stringp x)
         (plusp (length x))
         (alpha-char-p (aref x 0)))))

(defun write-example (example stream &key discard-label
                      (features-fn #'example-features))
  (format stream "~A" (example-event-id example))
  (let ((features (funcall features-fn example)))
    (dotimes (i (mat-size features))
      (format stream ",~A" (float (mref features i) 0.0))))
  (when (and (not discard-label) (example-label example))
    (format stream ",~A,~A" (float (example-weight example) 0.0)
            (string-downcase (symbol-name (example-label example)))))
  (terpri stream))

(defun write-training-csv-header (stream)
  (write-line "EventId,DER_mass_MMC,DER_mass_transverse_met_lep,DER_mass_vis,DER_pt_h,DER_deltaeta_jet_jet,DER_mass_jet_jet,DER_prodeta_jet_jet,DER_deltar_tau_lep,DER_pt_tot,DER_sum_pt,DER_pt_ratio_lep_tau,DER_met_phi_centrality,DER_lep_eta_centrality,PRI_tau_pt,PRI_tau_eta,PRI_tau_phi,PRI_lep_pt,PRI_lep_eta,PRI_lep_phi,PRI_met,PRI_met_phi,PRI_met_sumet,PRI_jet_num,PRI_jet_leading_pt,PRI_jet_leading_eta,PRI_jet_leading_phi,PRI_jet_subleading_pt,PRI_jet_subleading_eta,PRI_jet_subleading_phi,PRI_jet_all_pt,Weight,Label"
              stream))

(defun write-test-csv-header (stream)
  (write-line "EventId,DER_mass_MMC,DER_mass_transverse_met_lep,DER_mass_vis,DER_pt_h,DER_deltaeta_jet_jet,DER_mass_jet_jet,DER_prodeta_jet_jet,DER_deltar_tau_lep,DER_pt_tot,DER_sum_pt,DER_pt_ratio_lep_tau,DER_met_phi_centrality,DER_lep_eta_centrality,PRI_tau_pt,PRI_tau_eta,PRI_tau_phi,PRI_lep_pt,PRI_lep_eta,PRI_lep_phi,PRI_met,PRI_met_phi,PRI_met_sumet,PRI_jet_num,PRI_jet_leading_pt,PRI_jet_leading_eta,PRI_jet_leading_phi,PRI_jet_subleading_pt,PRI_jet_subleading_eta,PRI_jet_subleading_phi,PRI_jet_all_pt"
              stream))

(defun save-training (training training-file
                      &key (features-fn #'example-features))
  (with-open-file (training-stream training-file :direction :output
                                   :if-does-not-exist :create)
    (write-training-csv-header training-stream)
    (dolist (example training)
      (write-example example training-stream
                     :features-fn features-fn))))

(defun save-test (test test-file
                  &key (features-fn #'example-features))
  (with-open-file (test-stream test-file :direction :output
                               :if-does-not-exist :create)
    (write-test-csv-header test-stream)
    (dolist (example test)
      (write-example example test-stream :discard-label t
                     :features-fn features-fn))))
