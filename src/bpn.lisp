(in-package :rumcajsz)

(defvar *class-weights* nil)

(defun higgs-class-weights ()
  (or *class-weights*
      (setq *class-weights*
            (flet ((sum-weights-for-label (label examples)
                     (loop for example in examples
                           when (eq label (example-label example))
                             sum (example-weight example))))
              (let ((n (length (training-examples))))
                (mapcar (lambda (label)
                          (/ n *n-labels*
                             (sum-weights-for-label label (training-examples))))
                        *labels*))))))

(defun predict-batch-with-bpn (bpn examples)
  (let ((lump (find-clump 'prediction bpn))
        (mgl-bp::*in-training-p* nil)
        (predictions (make-hash-table)))
    (let ((sampler (make-sampler examples :randomp nil :discard-label-p t)))
      (do-batches-for-model (samples (sampler bpn))
        (set-input samples bpn)
        (forward bpn)
        (loop for i below (length samples)
              for sample in samples
              do (let ((example (first sample)))
                   (setf (gethash example predictions)
                         (mref (nodes lump) i 0))))))
    (alexandria:hash-table-alist predictions)))

(defun predict-batch-with-bpn-bag (training-file-fn model-file-fn examples
                                   &key log)
  (average-predictions
   (loop for i upfrom 0
         for training-file = (funcall training-file-fn i)
         for model-file = (funcall model-file-fn i)
         while (and training-file (probe-file training-file))
         collect (progn
                   (when log
                     (log-msg "Predicting with network ~S~%" i))
                   (let* ((training (higgs-boson::slurp-csv training-file))
                          (bpn (make-higgs-bpn training)))
                     (load-state model-file bpn)
                     (let ((predictions (predict-batch-with-bpn bpn examples)))
                       #+nil
                       (save-predictions
                        predictions
                        (format nil "bag-~S-test-predictions-bpn" i))
                       predictions))))))

(defmacro setq* ((symbol special) value)
  `(progn
     (setq ,symbol ,value)
     (when ,special
       (setf (symbol-value ,special) ,symbol))))


;;;; Sampling, clamping, utilities

(defun split-examples-stratified-source (examples fold n-folds)
  (split-stratified examples fold n-folds
                    :key #'example-source
                    :weight #'example-weight))

(defun make-sampler (examples &key (n-epochs 1) (randomp t)
                     (max-n (* n-epochs (length examples)))
                     discard-label-p)
  (make-instance 'function-sampler
                 :max-n-samples max-n
                 :generator (let ((g (if randomp
                                         (make-random-generator examples)
                                         (make-sequence-generator examples))))
                              (lambda ()
                                (list (funcall g)
                                      :discard-label-p discard-label-p)))))

(defun sample-example-features (sample encoder)
  (funcall encoder (first sample)))

(defun clamp-features (samples mat encoder)
  (assert (= (length samples) (mat-dimension mat 0)))
  (map-concat #'copy! samples mat
              :key (lambda (sample)
                     (sample-example-features sample encoder))))

(defun clamp-labels (samples mat &key (fillp t) extra-example-weights)
  (assert (= (length samples) (mat-dimension mat 0)))
  (let ((class-weights (higgs-class-weights)))
    (when fillp
      (fill! 0 mat))
    (loop for sample in samples
          for row upfrom 0
          do (destructuring-bind (example &key discard-label-p) sample
               (unless discard-label-p
                 (let ((label-index (label-index example)))
                   (setf (mref mat row label-index)
                         (* (example-weight example)
                            (if label-index
                                (elt class-weights label-index)
                                1)
                            (gethash example extra-example-weights 1)))))))))


;;;; Logging

(defclass higgs-base-trainer ()
  ((training :initarg :training :reader training)
   (tests :initarg :tests :reader tests)))

(defun log-training-period (trainer learner)
  (declare (ignore learner))
  (* 20 (length (training trainer))))

(defun log-test-period (trainer learner)
  (declare (ignore learner))
  (* 20 (length (training trainer))))


;;;; BPN

(defclass higgs-bpn (bpn)
  ((encoder :initarg :encoder :reader encoder)
   (extra-example-weights :initform (make-hash-table)
                          :reader extra-example-weights)))

(defvar *stop* nil)

(defun maybe-stop ()
  (when *stop*
    (setq *stop* nil)
    (cerror "continue" "stopping")))

(defmethod set-input (samples (bpn higgs-bpn))
  (maybe-stop)
  (let* ((inputs (find-clump 'inputs bpn :errorp t))
         (error-node (find-clump 'prediction bpn :errorp t))
         (target (ensure-softmax-target-matrix error-node (length samples))))
    (clamp-features samples (nodes inputs) (encoder bpn))
    (clamp-labels samples target
                  :extra-example-weights (extra-example-weights bpn))))

(defun prediction-weight-p (lump)
  (let ((name (name lump)))
    (and (listp name)
         (= 2 (length name))
         (eq 'predictions (second name)))))


;;;; BPN training

(defclass higgs-bp-trainer (higgs-base-trainer) ())

(defun log-test-error (trainer learner)
  (let* ((*random-state* (make-random-state nil))
         (bpn (bpn learner))
         (tests (tests trainer))
         (training (training trainer))
         (training-predictions (predict-batch-with-bpn bpn training))
         (test-prediction-seqs (mapcar (lambda (test)
                                         (predict-batch-with-bpn bpn test))
                                       tests)))
    (when (<= (* 60 (length training)) (n-instances trainer))
      (let* ((extra-example-weights (extra-example-weights bpn))
             (n (length training-predictions))
             (segments '(((0.00 1) (0.80 1))
                         ((0.80 1) (0.82 2))
                         ((0.82 2) (0.88 2))
                         ((0.88 2) (0.90 1))
                         ((0.90 1) (1.00 1))))
             (sum-s 0)
             (sum-b 0)
             (sum-e-s 0)
             (sum-e-b 0))
        (log-msg "extra weight segments~S~%" segments)
        (clrhash extra-example-weights)
        (loop for prediction in (sort training-predictions #'< :key #'cdr)
              for i upfrom 0
              do (destructuring-bind (example . s-confidence) prediction
                   (declare (ignore s-confidence))
                   (let* ((ratio (/ i n))
                          (w (loop for ((x0 y0) (x1 y1)) in segments
                                   when (<= x0 ratio x1)
                                     do (return (+ y0 (* (- y1 y0)
                                                         (/ (- ratio x0)
                                                            (- x1 x0))))))))
                     (setf (gethash example extra-example-weights) w)
                     (cond ((eq :s (example-label example))
                            (incf sum-s (example-weight example))
                            (incf sum-e-s (* w (example-weight example))))
                           (t
                            (incf sum-b (example-weight example))
                            (incf sum-e-b (* w (example-weight example))))))))
        (let ((rs (/ sum-s sum-e-s))
              (rb (/ sum-b sum-e-b)))
          (maphash (lambda (example extra-weight)
                     (setf (gethash example extra-example-weights)
                           (* extra-weight
                              (if (eq :s (example-label example))
                                  rs
                                  rb))))
                   extra-example-weights))))
    (log-padded
     (append
      (monitor-bpn-results (make-sampler training :randomp nil
                                         :max-n 50000)
                           bpn
                           (make-cost-monitors
                            bpn :attributes '(:event "pred."
                                              :dataset "train")))
      (loop for test in tests
            for i upfrom 0
            nconc (monitor-bpn-results
                   (make-sampler test) (bpn learner)
                   (make-cost-monitors
                    (bpn learner)
                    :attributes `(:event "pred."
                                         :dataset
                                         ,(format nil "test-~D" i)))))))
    (log-thresholds training-predictions test-prediction-seqs))
  (log-msg "---------------------------------------------------~%"))


;;;; Code for the plain dropout backpropagation network with rectified
;;;; linear units (paper [3])

(defclass higgs-bpn-gd-trainer (higgs-bp-trainer segmented-gd-optimizer)
  ())

(defclass higgs-bpn-gd-segment-trainer (sgd-optimizer)
  ((n-instances-in-epoch :initarg :n-instances-in-epoch
                         :reader n-instances-in-epoch)
   (n-epochs-to-reach-final-momentum
    :initarg :n-epochs-to-reach-final-momentum
    :reader n-epochs-to-reach-final-momentum)
   (learning-rate-decay
    :initform 0.998
    :initarg :learning-rate-decay
    :accessor learning-rate-decay)))

(defmethod learning-rate ((trainer higgs-bpn-gd-segment-trainer))
  (* (expt (learning-rate-decay trainer)
           (/ (n-instances trainer)
              (n-instances-in-epoch trainer)))
     (- 1 (momentum trainer))
     (slot-value trainer 'learning-rate)))

(defmethod momentum ((trainer higgs-bpn-gd-segment-trainer))
  (let ((n-epochs-to-reach-final (n-epochs-to-reach-final-momentum trainer))
        (initial 0.5)
        (final 0.99)
        (epoch (/ (n-instances trainer) (n-instances-in-epoch trainer))))
    (if (< epoch n-epochs-to-reach-final)
        (let ((weight (/ epoch n-epochs-to-reach-final)))
          (+ (* initial (- 1 weight))
             (* final weight)))
        final)))

(defun make-grouped-segmenter (group-name-fn segmenter)
  (let ((group-name-to-trainer (make-hash-table :test #'equal)))
    (lambda (segment)
      (let ((group-name (funcall group-name-fn segment)))
        (or (gethash group-name group-name-to-trainer)
            (setf (gethash group-name group-name-to-trainer)
                  (funcall segmenter segment)))))))

(defun make-dwim-grouped-segmenter (segmenter)
  (make-grouped-segmenter #'weight-lump-target-name segmenter))

(defun weight-lump-target-name (lump)
  (let ((name (name lump)))
    (assert (listp name))
    (assert (= 2 (length name)))
    (if (eq (first name) :cloud)
        (second (second name))
        (second name))))

(defun train-higgs-bpn-gd (bpn training tests &key
                           (n-epochs 200) l2-upper-bound
                           learning-rate learning-rate-decay
                           (n-epochs-to-reach-final-momentum 500)
                           (batch-size 96))
  (setf (max-n-stripes bpn) batch-size)
  (flet ((make-trainer (lump)
           (let ((trainer (make-instance
                           'higgs-bpn-gd-segment-trainer
                           :n-instances-in-epoch (length training)
                           :n-epochs-to-reach-final-momentum
                           (min n-epochs-to-reach-final-momentum
                                (/ n-epochs 2))
                           :learning-rate learning-rate
                           :learning-rate-decay learning-rate-decay
                           :weight-decay (if (or (equal '(inputs f1)
                                                        (name lump))
                                                 (equal '(inputs g1)
                                                        (name lump)))
                                             0.00005
                                             0)
                           :weight-penalty (if (or (equal '(inputs f1)
                                                          (name lump))
                                                   (equal '(inputs g1)
                                                          (name lump)))
                                               0.000005
                                               0)
                           :batch-size batch-size)))
             (when l2-upper-bound
               (arrange-for-renormalizing-activations
                bpn trainer l2-upper-bound))
             (when (member (name lump) '((inputs f1))
                           :test #'name=)
               (push (let ((mask (make-sparse-column-mask (nodes lump) 10)))
                       (.*! mask (nodes lump))
                       (lambda ()
                         (.*! mask (nodes lump))))
                     (after-update-hook trainer)))
             trainer))
         (make-segmenter (fn)
           (if l2-upper-bound
               (make-dwim-grouped-segmenter fn)
               fn)))
    (let ((trainer (monitor-optimization-periodically
                    (make-instance 'higgs-bpn-gd-trainer
                                   :training training
                                   :tests tests
                                   :segmenter (make-segmenter #'make-trainer)) 
                    '((:fn log-test-error
                       :period log-test-period)
                      (:fn reset-optimization-monitors
                       :period log-training-period
                       :last-eval 0))))
          (learner (make-instance 'bp-learner :bpn bpn
                                  :monitors (make-cost-monitors
                                             bpn :attributes
                                             '(:event "train"
                                               :dataset "train")))))
      (unless (zerop n-epochs)
        (mgl:log-msg "Starting to train the whole BPN~%")
        (minimize trainer learner
                  :dataset (make-sampler training :n-epochs n-epochs))))))

;;; Return a matrix of the same shape as MAT that's zero everywhere,
;;; except in at most N randomly chosen positions in each column where
;;; it's one.
(defun make-sparse-column-mask (mat n)
  (let ((mask (make-mat (mat-dimensions mat) :ctype (mat-ctype mat))))
    (destructuring-bind (n-rows n-columns) (mat-dimensions mat)
      (loop for column below n-columns do
        (loop repeat n
              do (setf (mref mask (random n-rows) column) 1))))
    mask))

(defun init-bpn-weights (bpn &key stddev)
  (map-segments (lambda (weights)
                  (let ((*cuda-enabled* nil))
                    (gaussian-random! (nodes weights) :stddev stddev)))
                bpn))

(defun build-higgs-bpn (&key (group-size 3) (n 600) dropout)
  (build-fnn (:class 'higgs-bpn :max-n-stripes 96)
    (inputs (->input :dropout nil :size *n-encoded-features*))
    (f1-activations (->activation inputs :name 'f1 :size n))
    (f1* (->max-channel f1-activations :group-size group-size))
    (f1 (->dropout f1* :dropout dropout))
    (f2-activations (->activation f1 :name 'f2 :size n))
    (f2* (->max-channel f2-activations :group-size group-size))
    (f2 (->dropout f2* :dropout dropout))
    (f3-activations (->activation f2 :name 'f3 :size n))
    (f3* (->max-channel f3-activations :group-size group-size))
    (f3 (->dropout f3* :dropout dropout))
    (prediction (->softmax-xe-loss (->activation f3 :name 'prediction
                                                 :size *n-labels*)))))

(defun make-higgs-bpn (training)
  (let ((bpn (build-higgs-bpn :group-size 3 :n 600 :dropout 0.5)))
    (setf (slot-value bpn 'encoder)
          (make-encoder training :transformers (make-transformers)))
    bpn))

(defun train-higgs/4 (&key training tests quick-run-p bpn-var bpn-filename)
  (repeatably ()
    (let ((bpn nil))
      (setq* (bpn bpn-var) (make-higgs-bpn training))
      (init-bpn-weights bpn :stddev 0.01)
      (train-higgs-bpn-gd bpn training tests
                          :n-epochs (if quick-run-p 2 200)
                          :n-epochs-to-reach-final-momentum 100
                          :learning-rate 1
                          :learning-rate-decay (expt 0.998 15)
                          :l2-upper-bound nil)
      (when (and bpn-filename)
        (save-state bpn-filename bpn))
      bpn)))


(defvar *bpn/4*)

(defun run-quick-test ()
  (train-higgs/4 :training (training-examples) :tests (list (test-examples))
                 :quick-run-p t))


;;;; Cross-validation

(defun train-4 (&key training tests filename)
  (train-higgs/4 :training training :tests tests
                 :bpn-var '*bpn/4*
                 :bpn-filename filename))

(defun run-cv-bagging (fn &key (save-dir *model-dir*)
                       (training (training-examples))
                       (test (test-examples))
                       (n-folds 2)
                       n-iterations)
  #+nil
  (assert (or (not (uiop/filesystem:directory-exists-p save-dir))
              (endp (directory (merge-pathnames "*" save-dir)))))
  (ensure-directories-exist save-dir)
  (let* ((*experiment-random-seed* 1234)
         (bag-index 0))
    (repeatably ()
      (bag-cv training
              (lambda (fold out-of-bag in-bag)
                (log-msg "Starting bag ~S (fold ~S)~%" bag-index fold)
                (describe-examples "in-bag" in-bag)
                (describe-examples "out-of-bag" out-of-bag)
                (sb-ext:gc :full t)
                (let ((training-file (merge-pathnames
                                      (format nil "bag-~S-training.csv"
                                              bag-index)
                                      save-dir))
                      (model-file (merge-pathnames
                                   (format nil "bag-~S-model-bpn" bag-index)
                                   save-dir))
                      (out-of-bag-predictions-file
                        (merge-pathnames
                         (format nil "bag-~S-out-of-bag-predictions-bpn"
                                 bag-index)
                         save-dir))
                      (test-predictions-file
                        (merge-pathnames
                         (format nil "bag-~S-test-predictions-bpn" bag-index)
                         save-dir)))
                  (unless (probe-file model-file)
                    (save-training in-bag training-file :if-exists :supersede)
                    (let* ((bpn (let ((*experiment-random-seed*
                                        (+ *experiment-random-seed* bag-index)))
                                  (funcall fn :training in-bag
                                           :tests (list out-of-bag test)
                                           :filename model-file)))
                           (out-of-bag-predictions
                             (predict-batch-with-bpn bpn out-of-bag)))
                      (save-predictions
                       out-of-bag-predictions
                       out-of-bag-predictions-file)
                      (when test
                        (save-predictions
                         (predict-batch-with-bpn bpn test)
                         test-predictions-file))
                      (log-msg "Finished bag ~S~%" bag-index)
                      (let ((out-of-bag-predictions
                              (average-overlapping-predictions
                               (mapcar
                                #'load-predictions
                                (directory (merge-pathnames
                                            "bag-*-out-of-bag-predictions-bpn"
                                            save-dir)))))
                            (test-predictions
                              (average-overlapping-predictions
                               (mapcar
                                #'load-predictions
                                (directory (merge-pathnames
                                            "bag-*-test-predictions-bpn"
                                            save-dir))))))
                        (log-msg "Test results with ~S bags~%" (1+ bag-index))
                        (log-thresholds () (list out-of-bag-predictions
                                                 test-predictions)))))
                  (incf bag-index)
                  (values)))
              :n-folds n-folds
              :n n-iterations
              :split-fn #'split-examples-stratified-source
              :pass-fold t
              :random-state (make-random-state nil)))))

(defun load-bag (name-fn)
  ;; For the sake of reproducability, don't rely on ordering of files
  ;; being stable.
  (loop for i upfrom 0
        for filename = (funcall name-fn i)
        while (and filename (probe-file filename))
        collect (load-predictions filename)))

#|


(let ((*default-mat-ctype* :float))
  (run-quick-test))

(progn
  (clrhash higgs-boson::*event-id-to-example*)
  (makunbound 'higgs-boson::*training-examples*)
  (makunbound 'higgs-boson::*test-examples*)
  (makunbound 'higgs-boson::*opendata-examples*))

(let ((*experiment-random-seed* 1234)
      (*default-mat-ctype* :float))
  (repeatably ()
    (run-cv-bagging 'train-4 :n-folds 2
                    :save-dir (merge-pathnames "xxx/" *model-dir*))))

(setq *stop* t)

|#
