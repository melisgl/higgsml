(in-package :rumcajsz)

(defun main ()
  (let ((args (rest sb-ext:*posix-argv*)))
    (unless (<= 1 (length args))
      (exit-with-error "Usage: ~A [command] [args...]~%"
                       (first sb-ext:*posix-argv*)))
    (let ((command (first args)))
      (cond ((string= command "train")
             (higgsml-train (rest args)))
            ((string= command "predict")
             (higgsml-predict (rest args)))
            (t
             (exit-with-error "Unknown command ~S must be one of ~
                              \"train\" and \"predict\".~%" command))))))

(defun higgsml-train (args)
  (unless (= 2 (length args))
    (exit-with-error
     "train command arguments: training.csv trained-model-dir~%"))
  (destructuring-bind (training-file trained-model-dir) args
    (let ((*training-file* training-file)
          (*test-file* nil)
          (trained-model-dir (osicat:pathname-as-directory trained-model-dir)))
      (when (uiop/filesystem:directory-exists-p trained-model-dir)
        (exit-with-error "Model directory ~S exists.~%"
                         (namestring trained-model-dir)))
      ;; Copy the entire training file to the model dir since we'll
      ;; need it at prediction time to reconstruct the normalization
      ;; of input.
      (alexandria:copy-file training-file
                            (ensure-directories-exist
                             (merge-pathnames "training.csv"
                                              trained-model-dir)))
      (train* (training-examples) trained-model-dir)
      (log-msg "Trained model written to ~S.~%"
               (namestring trained-model-dir))
      (log-msg "Success. Exiting ...~%"))))

(defun higgsml-predict (args)
  (unless (= 3 (length args))
    (exit-with-error "predict command arguments: ~
                     test.csv trained-model-dir submission.csv~%"))
  (destructuring-bind (test-file trained-model-dir submission-file) args
    (let* ((trained-model-dir (osicat:pathname-as-directory trained-model-dir))
           (*training-file* (merge-pathnames "training.csv" trained-model-dir))
           (*test-file* nil))
      (when (probe-file submission-file)
        (exit-with-error "Submission file ~S exists.~%"
                         (namestring submission-file)))
      ;; Force the original training examples to be loaded from the
      ;; file that was copied at training time to the model dir. If
      ;; this is not done, then the examples will be loaded from the
      ;; saved bag-*-training.csv files and we run into
      ;; text->float->text roundtrip issues and lose exact
      ;; reproducibility.
      (training-examples)
      (predict* test-file trained-model-dir submission-file)
      (log-msg "Submission file ~S written.~%" submission-file)
      (log-msg "Success. Exiting ...~%"))))

(defun train* (training-examples trained-model-dir)
  ;; Train a backprop network 35 times on 2 fold cross-validation
  ;; splits and average results. That's a bag of 70 neural networks.
  (let ((*experiment-random-seed* 1234)
        (mgl-example-util::*log-file*
          (merge-pathnames "rumcajsz.log" trained-model-dir)))
    (with-experiment ()
      (check-cuda)
      (run-cv-bagging #'train-4 :n-folds 2 :n-iterations 35
                      :save-dir trained-model-dir
                      :training training-examples
                      :test nil))))

(defun predict* (test-file trained-model-dir submission-file)
  (let* ((mgl-example-util::*log-file* "/dev/null")
         (bpn-predictions
           (with-cuda ()
             (check-cuda)
             (predict-batch-with-bpn-bag
              (lambda (i)
                (merge-pathnames (format nil "bag-~S-training.csv" i)
                                 trained-model-dir))
              (lambda (i)
                (merge-pathnames (format nil "bag-~S-model-bpn" i)
                                 trained-model-dir))
              (higgs-boson::slurp-csv test-file)
              :log t))))
    (generate-submission-by-ratio
     bpn-predictions 0.1520
     (merge-pathnames submission-file
                      *default-pathname-defaults*))))

(defun check-cuda ()
  (unless (cl-cuda:cuda-initialized-p)
    (exit-with-error "CUDA could not be initialized.")))

(defun exit-with-error (format &rest args)
  (apply #'format *error-output* format args)
  (sb-ext:exit :code 1))
