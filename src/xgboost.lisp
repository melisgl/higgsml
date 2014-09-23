;;;; Unused in winning submission.

(in-package :rumcajsz)

(defun run-xgboost (training-file test-file output-file seed)
  (external-program:run "/bin/bash"
                        (list
                         (merge-pathnames
                          "../xgboost-scripts/run.sh" *model-dir*)
                         seed
                         training-file test-file output-file
                         (+ *n-encoded-features* 2))
                        :output *standard-output*
                        :error *error-output*))

(defun average-sigmoid-predictions (predictionss)
  (let ((r (make-hash-table)))
    (loop for predictions in predictionss
          do (loop for prediction in predictions
                   do (destructuring-bind (example . confidence) prediction
                        (push (mgl-util:sigmoid (mgl-util:flt confidence))
                              (gethash example r)))))
    (flet ((foo (confidences)
             (alexandria:mean confidences) ))
      (mapcar (lambda (prediction)
                (cons (car prediction)
                      (foo (cdr prediction))))
              (alexandria:hash-table-alist r)))))

(defun xgboost (training test &key (seed 1) (features-fn #'example-features))
  (let* ((name (format nil "xgboost-~X" (random (expt 2 64))))
         (output-file (merge-pathnames (format nil "~A-predictions" name)
                                       *model-dir*))
         (training-file (merge-pathnames (format nil "~A-training.csv" name)
                                         *model-dir*))
         (test-file (merge-pathnames (format nil "~A-test.csv" name)
                                     *model-dir*)))
    (unwind-protect
         (progn
           (save-training training training-file
                          :features-fn features-fn)
           (save-test test test-file
                      :features-fn features-fn)
           (run-xgboost training-file test-file output-file seed)
           (average-sigmoid-predictions (list
                                         (load-predictions output-file))))
      (ignore-errors (delete-file output-file))
      (ignore-errors (delete-file training-file))
      (ignore-errors (delete-file test-file)))))

(defun split-examples-by-predictions (examples predictions)
  (let ((h (make-hash-table))
        (training ())
        (test ()))
    (dolist (prediction predictions)
      (setf (gethash (car prediction) h) t))
    (map nil (lambda (example)
               (if (gethash example h)
                   (push example test)
                   (push example training)))
         examples)
    (values training test)))

;;; Mirror an existing bag.
(defun mirror-splits-with-xgboost (predictionss-to-mirror dir)
  (loop for i upfrom 0
        for seed upfrom 1
        for ps in predictionss-to-mirror
        do (multiple-value-bind (training test)
               (split-examples-by-predictions (training-examples) ps)
             (describe-examples "training" training)
             (describe-examples "test" test)
             (let ((filename
                     (merge-pathnames
                      (format nil "bag-~S-out-of-bag-predictions-xgboost" i)
                      dir)))
               (unless (probe-file filename)
                 (save-predictions
                  (xgboost training test :seed seed
                           :features-fn
                           (make-encoder
                            training
                            :transformers nil
                            :normalize nil
                            :missing-value -999d0))
                  filename)))
             (let ((filename
                     (merge-pathnames
                      (format nil "bag-~S-leaderboard-predictions-xgboost" i)
                      dir)))
               (unless (probe-file filename)
                 (save-predictions
                  (xgboost training (test-examples) :seed seed
                           :features-fn
                           (make-encoder
                            training
                            :transformers nil
                            :normalize nil
                            :missing-value -999d0))
                  filename))))))

#|

(defparameter *to-mirror*
  (load-bag (lambda (i)
              (merge-pathnames
               (format nil "final/bag-~S-out-of-bag-predictions-bpn" i)
               *model-dir*))))

(mirror-splits-with-xgboost *to-mirror*
                            (ensure-directories-exist
                             (merge-pathnames "xxx/" *model-dir*)))


|#
