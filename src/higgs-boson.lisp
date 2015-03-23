(in-package :higgs-boson)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *label-names*
    ;; Signal vs background
    '("s" "b"))
  (defparameter *labels* '(:s :b))
  (defparameter *feature-names*
    '(:der-mass-mmc :der-mass-transverse-met-lep :der-mass-vis :der-pt-h
      :der-deltaeta-jet-jet :der-mass-jet-jet :der-prodeta-jet-jet
      :der-deltar-tau-lep :der-pt-tot :der-sum-pt :der-pt-ratio-lep-tau
      :der-met-phi-centrality :der-lep-eta-centrality
      :pri-tau-pt :pri-tau-eta :pri-tau-phi
      :pri-lep-pt :pri-lep-eta :pri-lep-phi
      :pri-met :pri-met-phi :pri-met-sumet :pri-jet-num
      :pri-jet-leading-pt :pri-jet-leading-eta :pri-jet-leading-phi
      :pri-jet-subleading-pt :pri-jet-subleading-eta :pri-jet-subleading-phi
      :pri-jet-all-pt))
  (defparameter *n-features* (length *feature-names*)))

(defparameter *feature-names-with-missing-values*
  '(:der-mass-mmc
    :der-deltaeta-jet-jet
    :der-mass-jet-jet
    :der-prodeta-jet-jet
    :der-lep-eta-centrality
    :pri-jet-leading-pt
    :pri-jet-leading-eta
    :pri-jet-leading-phi
    :pri-jet-subleading-pt
    :pri-jet-subleading-eta
    :pri-jet-subleading-phi))

(defun feature-index (name)
  (position name *feature-names*))

(defparameter *radian-feature-indices*
  (mapcar #'feature-index
          '(:pri-tau-phi
            :pri-lep-phi
            :pri-met-phi
            :pri-jet-leading-phi
            :pri-jet-subleading-phi)))

(deftype label () `(member ,@*labels*))

(defparameter *n-labels* (length *labels*))

(defstruct example
  (event-id nil :type fixnum)
  (label nil :type (or label null))
  (weight nil :type (or mgl-util:flt null))
  (features nil :type mat))

(defun index->label (index)
  (elt *labels* index))

(defmethod label-index ((label symbol))
  (position label *labels*))

(defmethod label-index ((example example))
  (label-index (example-label example)))


(defun label-weights (examples label)
  (loop for example in examples
        when (eq label (example-label example))
          sum (example-weight example)))

(defun describe-examples (name examples)
  (log-msg "~A set n: ~S S weights: ~,1F B weights: ~,1F~%"
           name (length examples)
           (label-weights examples :s) (label-weights examples :b)))

;;; http://higgsml.lal.in2p3.fr/files/2014/04/documentation_v1.5.pdf
;;;
;;; The error measure is not scale independent. If the some portion of
;;; the dataset is evaluated then SCALE can be set the 1/portion to
;;; compensate.
(defun approximate-median-significance (s b &key (scale 1) (b-reg 10))
  (let ((s (float (* scale s) 0d0))
        (b (float (* scale b) 0d0))
        (b-reg (float b-reg 0d0))
        (scale (float scale 0d0)))
    (declare (type double-float s b b-reg)
             (optimize speed))
    (values (mgl-mat::the!
             double-float
             (sqrt (max 0d0
                        (* 2 (- (* (+ s b b-reg)
                                   (mgl-mat::the!
                                    double-float
                                    (log (+ 1 (/ s (+ b b-reg))))))
                                s)))))
            (* scale s)
            (* scale b))))

(defun approximate-median-significance-for-s-set (s-examples scale &key
                                                  (b-reg 10))
  (let ((s 0d0)
        (b 0d0)
        (n-ss 0)
        (n-bs 0))
    (map nil
         (lambda (example)
           (let ((weight (example-weight example)))
             (if (eq :s (example-label example))
                 (progn
                   (incf s weight)
                   (incf n-ss))
                 (progn
                   (incf b weight)
                   (incf n-bs)))))
         s-examples)
    (values (approximate-median-significance s b :scale scale :b-reg b-reg)
            (* scale s) (* scale b)
            (if (zerop n-ss) 0 (/ b n-ss))
            (if (zerop n-bs) 0 (/ b n-bs)))))

(defun sum-weights (examples &key (key #'identity))
  (let ((sum 0))
    (map nil (lambda (example)
               (incf sum (example-weight (funcall key example))))
         examples)
    sum))

(defvar *training-weight-sum*)

(defun training-weights-sum ()
  (unless (boundp '*training-weight-sum*)
    (setq *training-weight-sum* (sum-weights (training-examples))))
  *training-weight-sum*)

(defun scaling-factor (examples &key (key #'identity))
  (let ((sum (sum-weights examples :key key)))
    (if (zerop sum)
        0
        (/ (training-weights-sum) sum))))

(defun find-best-threshold (pairs &key (b-reg 10) scale)
  (let* ((pairs (sort (coerce (copy-seq pairs) 'vector) #'> :key #'cdr))
         (s 0d0)
         (b 0d0)
         (scale (float (or scale (scaling-factor pairs :key #'car)) 0d0))
         (b-reg (float b-reg 0d0))
         (n 0)
         (n-ss 0)
         (n-bs 0)
         (best-threshold most-negative-double-float)
         (best-n 0)
         (best-ams (approximate-median-significance s b :scale scale))
         (best-s s)
         (best-b b)
         (best-n-ss 0)
         (best-n-bs 0)
         (prev-confidence (cdr (aref pairs 0)))
         (ams-auc 0)
         (batch-start 0))
    (loop
      for pair across pairs
      for i upfrom 0
      do (destructuring-bind (example . s-confidence) pair
           (when (/= prev-confidence s-confidence)
             (let ((ams (approximate-median-significance-df s b scale b-reg)))
               (incf ams-auc (* ams (- i batch-start)))
               (when (or (= best-threshold s-confidence) (< best-ams ams))
                 (setq best-threshold s-confidence
                       best-n n
                       best-ams ams
                       best-s s
                       best-b b
                       best-n-ss n-ss
                       best-n-bs n-bs)))
             (setq prev-confidence s-confidence)
             (setq batch-start i))
           (let ((weight (example-weight example)))
             (incf n)
             (if (eq :s (example-label example))
                 (progn
                   (incf s weight)
                   (incf n-ss))
                 (progn
                   (incf b weight)
                   (incf n-bs))))))
    (values best-threshold best-ams (* scale best-s) (* scale best-b)
            (float (/ best-n (length pairs)))
            (if (zerop best-n-ss) 0 (/ best-s best-n-ss))
            (if (zerop best-n-bs) 0 (/ best-b best-n-bs))
            (/ ams-auc n))))

(defun collect-ams (predictions &key (b-reg 10) use-confidence)
  (let ((amss ()))
    (map-ams (lambda (ratio confidence ams)
               (push (list (if use-confidence (- 1 confidence) ratio)
                           ams)
                     amss))
             predictions :b-reg b-reg)
    (reverse amss)))

(defun map-ams (fn pairs &key (b-reg 10) scale)
  (let* ((pairs (sort (coerce (shuffle pairs) 'vector) #'> :key #'cdr))
         (s 0d0)
         (b 0d0)
         (scale (float (or scale (scaling-factor pairs :key #'car)) 0d0))
         (b-reg (float b-reg 0d0))
         (n 0)
         (length (length pairs)))
    (loop for pair across pairs do
      (destructuring-bind (example . s-confidence) pair
        (let ((weight (example-weight example)))
          (if (eq :s (example-label example))
              (progn
                (incf s weight))
              (progn
                (incf b weight))))
        (let ((ams (approximate-median-significance-df s b scale b-reg)))
          (funcall fn (/ n length) s-confidence ams))
        (incf n)))))

;;; faster double float version
(defun approximate-median-significance-df (s b scale b-reg)
  (declare (type double-float s b scale b-reg))
  (locally
      (declare (optimize speed))
    (let ((s (* scale s))
          (b (* scale b)))
      (values (mgl-mat::the!
               double-float
               (sqrt (* 2 (max 0d0 (- (* (+ s b b-reg)
                                         (mgl-mat::the!
                                          double-float
                                          (log (+ 1 (/ s (+ b b-reg))))))
                                      s)))))
              s
              b))))

(defun threshold-confidence (pairs threshold)
  (let ((examples ()))
    (map nil (lambda (pair)
               (destructuring-bind (example . s-confidence) pair
                 (when (<= threshold s-confidence)
                   (push example examples))))
         pairs)
    (values examples (scaling-factor pairs :key #'car))))

(defun threshold-confidence-by-ratio (pairs ratio)
  (let ((pairs (sort (copy-seq pairs) #'> :key #'cdr))
        (n (length pairs)))
    (map 'list #'car (subseq pairs 0 (floor (* n ratio))))))

(defun log-thresholds (training-predictions test-prediction-seqs)
  (let ((training-predictions (shuffle training-predictions))
        (test-prediction-seqs (mapcar #'shuffle test-prediction-seqs)))
    (when (plusp (length training-predictions))
      (loop for b-reg in '(10) do
        (multiple-value-bind (best-threshold best-ams best-s best-b
                              best-ratio best-s-mean best-b-mean ams-auc)
            (find-best-threshold training-predictions :b-reg b-reg)
          (declare (ignore best-s-mean best-b-mean))
          (log-msg "~3D ams:#~5,3F ~5,3F S: ~3D B: ~6D T: ~6,4F R: ~6,4F~%"
                   b-reg best-ams ams-auc (round best-s) (round best-b)
                   best-threshold best-ratio))))
    (dolist (test-predictions test-prediction-seqs)
      (when (plusp (length test-predictions))
        (let ((test-scale (scaling-factor test-predictions :key #'car)))
          (dolist (b-reg (list 10))
            (loop
              for ratio in '(0.11 0.115
                             0.12 0.125 0.13 0.135 0.14 0.145 0.15 0.155
                             0.16 0.165 0.17)
              do (multiple-value-bind (ams s b)
                     (approximate-median-significance-for-s-set
                      (threshold-confidence-by-ratio test-predictions ratio)
                      test-scale
                      :b-reg b-reg)
                   (log-msg
                    "@15 ams: ~5,3F       S: ~3D B: ~6D           R: ~6,4F~%"
                    ams (round s) (round b) ratio)))
            (multiple-value-bind (best-threshold best-ams best-s best-b
                                  best-ratio best-s-mean best-b-mean
                                  ams-auc)
                (find-best-threshold test-predictions :scale test-scale
                                     :b-reg b-reg)
              (declare (ignore best-s-mean best-b-mean))
              (log-msg "idl ams: ~5,3F ~5,3F S: ~3D B: ~6D T: ~6,4F R: ~6,4F~%"
                       best-ams ams-auc (round best-s) (round best-b)
                       best-threshold best-ratio))))))))


;;;; Submission

(defun generate-submission-by-confidence (s-confidences threshold filename)
  (let ((pairs (sort (copy-seq s-confidences) #'< :key #'cdr)))
    (with-open-file (stream (ensure-directories-exist
                             (merge-pathnames filename *submission-dir*))
                            :direction :output
                            :if-does-not-exist :create)
      (format stream "EventId,RankOrder,Class~%")
      (let ((rank 1))
        (map nil (lambda (pair)
                   (destructuring-bind (example . s-confidence) pair
                     (format stream "~S,~S,~A~%" (example-event-id example)
                             rank (if (<= threshold s-confidence) "s" "b")))
                   (incf rank))
             pairs)))))

(defun generate-submission-by-ratio (s-confidences ratio filename)
  (let ((pairs (sort (copy-seq s-confidences) #'< :key #'cdr))
        (n (* (- 1 ratio) (length s-confidences))))
    (with-open-file (stream (ensure-directories-exist
                             (merge-pathnames filename *submission-dir*))
                            :direction :output
                            :if-does-not-exist :create)
      (format stream "EventId,RankOrder,Class~%")
      (let ((rank 1))
        (map nil (lambda (pair)
                   (destructuring-bind (example . s-confidence) pair
                     (declare (ignore s-confidence))
                     (format stream "~S,~S,~A~%" (example-event-id example)
                             rank (if (<= rank n) "b" "s")))
                   (incf rank))
             pairs)))))

(defun load-submission (filename)
  (with-open-file (stream filename)
    (map-csv-records #'csv-record-to-prediction filename
                     :skip-header t :header-p-fn #'csv-header-p)))

(defun csv-record-to-prediction (record filename file-position)
  (declare (ignore filename file-position))
  (let ((event-id (parse-integer (first record)))
        (rank (parse-integer (second record))))
    (cons (find-example-by-event-id event-id) rank)))

(defvar *event-id-to-example* (make-hash-table))

(defun find-example-by-event-id (event-id &key (errorp t))
  (or (gethash event-id *event-id-to-example*)
      (if errorp
          (error "Can't find event with id ~S." event-id)
          nil)))

(defun load-predictions (filename)
  (training-examples)
  (test-examples)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil nil)
          while line
          collect (with-input-from-string (stream line)
                    (cons (let ((event-id (read stream)))
                            (or (find-example-by-event-id event-id)
                                (error "Bad event id ~S in line ~S."
                                       event-id line)))
                          (read stream))))))

(defun save-predictions (predictions filename)
  (with-open-file (stream filename :direction :output
                          :if-does-not-exist :create)
    (dolist (prediction (sort (copy-seq predictions) #'<
			      :key (lambda (prediction)
				     (example-event-id (car prediction)))))
      (destructuring-bind (example . confidence) prediction
        (format stream "~S ~S~%" (example-event-id example) confidence)))))

(defun scale-predictions (scale predictions)
  (mapcar (lambda (prediction)
            (cons (car prediction) (* scale (cdr prediction))))
          predictions))

(defun average-predictions (predictionss)
  (let ((r (make-hash-table))
        (n (length predictionss)))
    (loop for predictions in predictionss
          do (loop for prediction in predictions
                   do (destructuring-bind (example . confidence) prediction
                        (incf (gethash example r 0) confidence))))
    (mapcar (lambda (prediction)
              (cons (car prediction) (/ (cdr prediction) n)))
            (alexandria:hash-table-alist r))))

(defun average-overlapping-predictions (predictionss)
  (let ((r (make-hash-table)))
    (loop for predictions in predictionss
          do (loop for prediction in predictions
                   do (destructuring-bind (example . confidence) prediction
                        (let ((entry (gethash example r (cons 0 0))))
                          (incf (car entry) confidence)
                          (incf (cdr entry))
                          (setf (gethash example r) entry)))))
    (mapcar (lambda (pair)
              (cons (car pair) (/ (cadr pair) (cddr pair))))
            (alexandria:hash-table-alist r))))

(defun predictions-for-examples (predictions examples)
  (let ((h (make-hash-table)))
    (map nil (lambda (example)
               (setf (gethash example h) t))
         examples)
    (loop for prediction in predictions
          when (gethash (car prediction) h)
            collect prediction)))

(defun prediction-examples (predictions)
  (mapcar #'car predictions))

(defun sum-weights-for-labels (examples &key key (prior 5))
  (let ((sum-s prior)
        (sum-b prior))
    (map nil (lambda (example)
               (let ((example (funcall key example)))
                 (if (eq :s (example-label example))
                     (incf sum-s (* 180 (example-weight example)))
                     (incf sum-b (* 0.3 (example-weight example))))))
         examples)
    (values sum-s sum-b)))


;;;; Simulation sources

(defun example-source (example)
  (let ((w (example-weight example)))
    (if (eq :s (example-label example))
        (cond ((< w 0.002)
               :s1)
              ((< w 0.004)
               :s2)
              (t
               :s3))
        (cond ((< w 0.19)
               :b1)
              ((< w 0.5)
               :b2)
              ((< w 1.6)
               :b3)
              ((< w 3.6)
               :b4)
              (t
               :b5)))))
#|

(defparameter *0584eccb-leaderboard-predictions*
  (average-overlapping-predictions
   (loop for i upfrom 0
         for filename = (merge-pathnames
                         (format nil "0584eccb/~
                                      bag-~S-leaderboard-predictions" i)
                         *model-dir*)
         while (probe-file filename)
         collect (load-predictions filename))))

(generate-submission-by-ratio *0584eccb-leaderboard-predictions*
                              0.1512 "bpn3-108.csv") => 3.78136



(defparameter *xg-leaderboard-predictions*
  (average-overlapping-predictions
   (loop for i upfrom 0
         for filename = (merge-pathnames
                         (format nil "6ea9554/bag-~S-leaderboard-predictions~
                                     -xgboost-360-0.15" i)
                         *model-dir*)
         while (probe-file filename)
         collect (load-predictions filename))))

(defparameter *leaderboard-ensemble*
  (average-overlapping-predictions
   (list
    *0584eccb-leaderboard-predictions*
    (scale-predictions 0.08 *xg-leaderboard-predictions*))))

(generate-submission-by-ratio *leaderboard-ensemble* 0.1512 "bpn3-108.csv") 
=> 3.78136
(generate-submission-by-ratio *leaderboard-ensemble* 0.1536 "bpn3-109.csv")
 => 3.77505

|#
