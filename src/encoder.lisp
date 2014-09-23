(in-package :rumcajsz)

(defun log-transform (x)
  (if (plusp x)
      (log (1+ x))
      (- (log (1+ (- x))))))

(defun encode-raw (feature-vector start x)
  (setf (aref feature-vector start) x)
  (1+ start))

(defun encode-normalized (feature-vector start x missingp stat)
  (unless missingp
    (let ((mean (running-stat-mean stat))
          (stddev (sqrt (running-stat-variance stat))))
      (unless (zerop stddev)
        (setf (aref feature-vector start) (/ (- x mean) stddev)))))
  (1+ start))

(defun encode-log (feature-vector start x missingp)
  (unless missingp
    (if (plusp x)
        (setf (aref feature-vector start) (log x))
        (warn "non-positive for log")))
  (1+ start))

(defun encode-extra-radian (feature-vector start raw)
  (let ((phi-1 (nth-value 2 (momentum-vector raw :tau)))
        (phi-2 (nth-value 2 (momentum-vector raw :lep)))
        (phi-3 (nth-value 2 (momentum-vector raw :met))))
    (setf (aref feature-vector start)
          (/ (- (min (abs (normalize-radian (- phi-1 phi-2)))
                     (abs (normalize-radian (- phi-1 phi-3)))
                     (abs (normalize-radian (- phi-2 phi-3))))
                0.79)
             0.55))
    (incf start)
    (setf (aref feature-vector start)
          (/ (- (min (abs (normalize-radian (- phi-2 phi-3)))
                     (abs (normalize-radian (- phi-1 phi-3))))
                0.95)
             0.74))
    (incf start)
    (setf (aref feature-vector start)
          (/ (- (min (abs (normalize-radian (- phi-1 phi-2)))
                     (abs (normalize-radian (- phi-1 phi-3))))
                1.37)
             0.86))
    (incf start)
    (setf (aref feature-vector start)
          (/ (- (abs (normalize-radian (- phi-2 phi-3)))
                1.735)
             1.02))
    (incf start)))

(defun momentum-vector (feature-vector particle)
  (ecase particle
    ((:met)
     (values (aref feature-vector (feature-index :pri-met))
             0
             (aref feature-vector (feature-index :pri-met-phi))))
    ((:tau)
     (values (aref feature-vector (feature-index :pri-tau-pt))
             (aref feature-vector (feature-index :pri-tau-eta))
             (aref feature-vector (feature-index :pri-tau-phi))))
    ((:lep)
     (values (aref feature-vector (feature-index :pri-lep-pt))
             (aref feature-vector (feature-index :pri-lep-eta))
             (aref feature-vector (feature-index :pri-lep-phi))))
    ((:jet)
     (values (aref feature-vector (feature-index :pri-jet-leading-pt))
             (aref feature-vector (feature-index :pri-jet-leading-eta))
             (aref feature-vector (feature-index :pri-jet-leading-phi))))
    ((:jet-2)
     (values (aref feature-vector (feature-index :pri-jet-subleading-pt))
             (aref feature-vector (feature-index :pri-jet-subleading-eta))
             (aref feature-vector (feature-index :pri-jet-subleading-phi))))))

(defun decoded-momentum-vector (feature-vector particle)
  (multiple-value-bind (pt eta phi) (momentum-vector feature-vector particle)
    (when (< -990 pt)
      (decode-momentum pt eta phi))))

(defun decode-momentum (pt eta phi)
  (values (* pt (cos phi))
          (* pt (sin phi))
          (* pt (sinh eta))))

(defun encode-momentum (x y z)
  (let ((pt (sqrt (+ (expt x 2) (expt y 2)))))
    (values pt
            (asinh (/ z pt))
            (asin (/ y pt)))))

#+nil
(multiple-value-call #'encode-momentum (decode-momentum 2 3 1.5d0))

(defun der-inv-mass (feature-vector particle-1 particle-2)
  (assert (not (eq particle-1 :met)))
  (assert (not (eq particle-2 :met)))
  (multiple-value-bind (pt-1 eta-1 phi-1)
      (momentum-vector feature-vector particle-1)
    (multiple-value-bind (pt-2 eta-2 phi-2)
        (momentum-vector feature-vector particle-2)
      (when (and (< -990 pt-1)
                 (< -990 pt-2))
        (log (1+ (inv-mass pt-1 eta-1 phi-1 pt-2 eta-2 phi-2)))))))

(defun der-tr-mass (feature-vector particle-1 particle-2)
  (multiple-value-bind (pt-1 eta-1 phi-1)
      (momentum-vector feature-vector particle-1)
    (multiple-value-bind (pt-2 eta-2 phi-2)
        (momentum-vector feature-vector particle-2)
      (when (and (< -990 pt-1)
                 (< -990 pt-2))
        (log (1+ (tr-mass pt-1 eta-1 phi-1 pt-2 eta-2 phi-2)))))))

(defun normalize-radian (r)
  (- (mod (+ r pi) (* 2 pi)) pi))

(defun encode-extra-moduli (feature-vector start f)
  (multiple-value-bind (tau-x tau-y tau-z) (decoded-momentum-vector f :tau)
    (multiple-value-bind (lep-x lep-y lep-z) (decoded-momentum-vector f :lep)
      (multiple-value-bind (jet-x jet-y jet-z) (decoded-momentum-vector f :jet)
        (multiple-value-bind (jet-2-x jet-2-y jet-2-z)
            (decoded-momentum-vector f :jet-2)
          (setf (aref feature-vector start)
                (let ((x (+ tau-x lep-x (or jet-x 0) (or jet-2-x 0)))
                      (y (+ tau-y lep-y (or jet-y 0) (or jet-2-y 0)))
                      (z (+ tau-z lep-z (or jet-z 0) (or jet-2-z 0))))
                  (multiple-value-bind (pt eta) (encode-momentum x y z)
                    (- (log (* pt (cosh eta)))
                       4.81))))
          (incf start)))))
  start)

(defun encode-extra-mass (feature-vector start raw)
  (progn
    (setf (aref feature-vector start)
          (let ((x (der-inv-mass raw :tau :jet)))
            (flt (if x (/ (- x 4.76) 0.66) 0))))
    (incf start)
    (setf (aref feature-vector start)
          (let ((x (der-inv-mass raw :tau :jet-2)))
            (flt (if x (/ (- x 4.74) 0.67) 0))))
    (incf start)
    (setf (aref feature-vector start)
          (let ((x (der-tr-mass raw :tau :lep)))
            (if x
                (flt (/ (log-transform (- x 4.13))
                        0.345))
                (flt 0))))
    (incf start)
    (setf (aref feature-vector start)
          (let ((x (der-tr-mass raw :tau :jet)))
            (if x
                (flt (/ (log-transform (- x 4.18))
                        0.509))
                (flt 0))))
    (incf start)
    (setf (aref feature-vector start)
          (let ((x (der-tr-mass raw :tau :jet-2)))
            (if x
                (flt (/ (log-transform (- x 4.05))
                        0.498))
                (flt 0))))
    (incf start)))

(defun inv-mass (pt-1 eta-1 phi-1 pt-2 eta-2 phi-2)
  (multiple-value-bind (x1 y1 z1)
      (decode-momentum pt-1 eta-1 phi-1)
    (multiple-value-bind (x2 y2 z2)
        (decode-momentum pt-2 eta-2 phi-2)
      (sqrt (- (expt (+ (sqrt (+ (expt x1 2)
                                 (expt y1 2)
                                 (expt z1 2)))
                        (sqrt (+ (expt x2 2)
                                 (expt y2 2)
                                 (expt z2 2))))
                     2)
               (expt (+ x1 x2)
                     2)
               (expt (+ y1 y2)
                     2)
               (expt (+ z1 z2)
                     2))))))

(defun tr-mass (pt-1 eta-1 phi-1 pt-2 eta-2 phi-2)
  (multiple-value-bind (x1 y1 z1)
      (decode-momentum pt-1 eta-1 phi-1)
    (declare (ignore z1))
    (multiple-value-bind (x2 y2 z2)
        (decode-momentum pt-2 eta-2 phi-2)
      (declare (ignore z2))
      (sqrt (max 0
                 (- (expt (+ (sqrt (+ (expt x1 2)
                                      (expt y1 2)))
                             (sqrt (+ (expt x2 2)
                                      (expt y2 2))))
                          2)
                    (expt (+ x1 x2)
                          2)
                    (expt (+ y1 y2)
                          2)))))))

(defun transform (feature-vector transformers feature-index)
  (if (< (aref feature-vector feature-index) -990)
      nil
      (let ((entry (find feature-index transformers :key #'car)))
        (if entry
            (funcall (cdr entry) feature-vector)
            (let ((x (aref feature-vector feature-index)))
              (if (< x -990)
                  nil
                  x))))))

(defun make-transformers ()
  (loop for feature-name
          in '(:pri-jet-leading-pt
               :pri-jet-subleading-pt
               :pri-tau-pt
               :pri-lep-pt
               :der-pt-h
               :pri-met
               :pri-met-sumet
               :der-pt-tot
               :der-sum-pt
               :der-mass-jet-jet
               :der-mass-vis
               :der-mass-mmc
               :der-mass-transverse-met-lep
               :pri-jet-all-pt)
        collect (let ((feature-index (feature-index feature-name)))
                  (cons feature-index
                        (lambda (feature-vector)
                          (log-transform (aref feature-vector
                                               feature-index)))))))

;;; Don't use the phi features directly (only via ENCODE-EXTRA-RADIAN)
;;; because they make results fluctuate way more.
(defparameter *encoded-feature-names*
  (set-difference *feature-names*
                  '(:pri-tau-phi
                    :pri-lep-phi
                    :pri-met-phi
                    :pri-jet-leading-phi
                    :pri-jet-subleading-phi)))

(defparameter *n-encoded-features*
  (+ (length *encoded-feature-names*)
     ;; see ENCODE-EXTRA-RADIAN
     4 
     ;; see ENCODE-EXTRA-MASS
     5
     ;; see ENCODE-EXTRA-MODULI
     1))

(defun make-encoder (examples &key transformers (normalize t)
                     (missing-value (flt 0)))
  (let ((transformed-stats (coerce (loop repeat *n-features*
                                         collect (make-instance 'running-stat))
                                   'vector))
        
        (n-missing (make-array *n-features* :initial-element 0))
        (class-weights (higgs-class-weights)))
    (map nil (lambda (example)
               (with-facets ((x ((example-features example) 'array
                                 :direction :input)))
                 (dotimes (i *n-features*)
                   (let ((e (transform x transformers i))
                         (w (* (if (eq :s (example-label example))
                                   (elt class-weights 0)
                                   (elt class-weights 1))
                               (example-weight example))))
                     (cond (e
                            (add-to-running-stat e (aref transformed-stats i)
                                                 :weight w))
                           (t
                            (incf (aref n-missing i))))))))
         examples)
    (loop for i upfrom 0
          for stat across transformed-stats
          do (format t "~S ts: ~A~%" i (aref transformed-stats i)))
    (let ((example-to-features (make-hash-table))
          (lock (sb-thread:make-mutex)))
      (lambda (example &key lisp-array-p)
        (sb-thread:with-mutex (lock)
          (or (gethash example example-to-features)
              (setf (gethash example example-to-features)
                    (let ((f (make-array *n-encoded-features*
                                         :element-type 'mgl-util:flt
                                         :initial-element missing-value))
                          (start 0)
                          (mgl-cube:*let-input-through-p* t))
                      (with-facets ((raw ((example-features example) 'array
                                          :direction :input)))
                        (loop for i below *n-features*
                              for name in *feature-names*
                              do (let* ((x (transform raw transformers i))
                                        (missingp (null x))
                                        (transformed-stat
                                          (aref transformed-stats i)))
                                   (when (member name *encoded-feature-names*)
                                     (if normalize
                                         (setq start
                                               (encode-normalized
                                                f start x missingp
                                                transformed-stat))
                                         (setq start
                                               (encode-raw f start
                                                           (aref raw i)))))))
                        (setq start (encode-extra-mass f start raw))
                        (setq start (encode-extra-moduli f start raw))
                        (setq start (encode-extra-radian f start raw)))
                      (assert (= (length f) start))
                      (if lisp-array-p
                          f
                          (array-to-mat f :ctype mgl-util:flt-ctype))))))))))
