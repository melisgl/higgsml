;;;; Not part of the solution, only used for exploration.

(in-package :higgs-boson)

(defparameter *histogram-options*
  "using (bin($1,binwidth)):($2) smooth freq with boxes")

(defun histogram (data-1 data-2 binwidth)
  (mgl-gnuplot:with-session (:geometry "2000x2000")
    (let (#+nil
          (mgl-gnuplot:*command-stream*
            (make-broadcast-stream mgl-gnuplot:*command-stream*
                                   *standard-output*)))
      (mgl-gnuplot:command (format nil "binwidth=~S" binwidth))
      (mgl-gnuplot:command "bin(x,width)=width*floor(x/width)")
      (mgl-gnuplot:command "set style fill transparent pattern 4")
      (mgl-gnuplot:command
       (make-instance 'mgl-gnuplot:plot
                      :mappings (list
                                 (make-instance 'mgl-gnuplot:data-mapping
                                                :options *histogram-options*
                                                :data data-1)
                                 (make-instance 'mgl-gnuplot:data-mapping
                                                :options *histogram-options*
                                                :data data-2)))))))

(defun plot-s-and-b (feature-index &key (transform #'identity)
                     (examples (training-examples)) (binwidth 0.1))
  (let ((s-data
          (loop for example in examples
                for f = (if (typep feature-index 'integer)
                            (mref (example-features example)
                                  feature-index)
                            (funcall feature-index example))
                when (and (eq :s (example-label example))
                          f
                          (< -990 f))
                  collect (list (funcall transform f)
                                (* 180 (example-weight example)))))
        (b-data
          (loop for example in examples
                for f = (if (typep feature-index 'integer)
                            (mref (example-features example)
                                  feature-index)
                            (funcall feature-index example))
                when (and (eq :b (example-label example))
                          f
                          (< -990 f))
                  collect (list (funcall transform f)
                                (* 0.3 (example-weight example))))))
    (print-data-stats (append s-data b-data))
    (print-data-stats s-data)
    (print-data-stats b-data)
    (histogram
     (normalize-plot-data s-data)
     (normalize-plot-data b-data)
     binwidth)))

(defun normalize-plot-data (data)
  (let ((sum (loop for e in data sum (second e))))
    (loop for e in data collect (list (first e) (/ (second e) sum)))))

(defun print-data-stats (data)
  (let ((stat (make-instance 'mgl-util:running-stat)))
    (dolist (e data)
      (mgl-util:add-to-running-stat (first e) stat :weight (second e)))
    (format t "mean: ~F stddev: ~F~%"
            (mgl-util:running-stat-mean stat)
            (sqrt (mgl-util:running-stat-variance stat)))))

(defun plot-ams (predictionss &key (b-reg 10) use-confidence)
  (mgl-gnuplot:with-session (:geometry "2000x2000")
    (let (#+nil
          (mgl-gnuplot:*command-stream*
            (make-broadcast-stream mgl-gnuplot:*command-stream*
                                   *standard-output*)))
      (mgl-gnuplot:command "set yrange [3.5:3.9]")
      (if use-confidence
          (mgl-gnuplot:command "set xrange [0:0.15]")
          (mgl-gnuplot:command "set xrange [0.08:0.22]"))
      (mgl-gnuplot:command
       (make-instance
        'mgl-gnuplot:plot
        :mappings (mapcar (lambda (predictions)
                            (make-instance 'mgl-gnuplot:data-mapping
                                           :options "ps 0.01 with lines notitle"
                                           :data (collect-ams
                                                  predictions
                                                  :b-reg b-reg
                                                  :use-confidence
                                                  use-confidence)))
                          predictionss))))))

(defun randomize-predictions (predictions stddev)
  (loop for prediction in predictions
        collect (cons (car prediction)
                      (+ (cdr prediction)
                         (* stddev (mgl-util:gaussian-random-1))))))

(defun average-ams (predictions stddev n &key (max-ratio 0.2))
  (let* ((v (make-array (length predictions) :initial-element 0))
         (length (length predictions))
         (scale (scaling-factor predictions :key #'car))
         (predictions (subseq (sort (copy-seq predictions) #'> :key #'cdr)
                              0 (floor (* max-ratio length)))))
    (loop repeat n
          do (let ((i 0))
               (map-ams (lambda (ratio confidence ams)
                          (declare (ignore ratio confidence))
                          (incf (aref v i) ams)
                          (incf i))
                        (randomize-predictions predictions stddev)
                        :scale scale)))
    (loop for x across v
          for i upfrom 0
          collect (list (/ i length) (/ x n)))))

(defun plot-smoothed-ams (predictionss &key (stddev 0.005) (n 200))
  (let ((smootheds (mapcar (lambda (predictions)
                             (average-ams predictions stddev n))
                           predictionss)))
    (mgl-gnuplot:with-session ()
      (mgl-gnuplot:command "set yrange [3.5:3.9]")
      (mgl-gnuplot:command "set xrange [0.08:0.22]")
      (mgl-gnuplot:command
       (make-instance
        'mgl-gnuplot:plot
        :mappings (mapcar (lambda (smoothed)
                            (make-instance 'mgl-gnuplot:data-mapping
                                           :options "ps 0.01 with lines notitle"
                                           :data smoothed))
                          smootheds))))))

(defun bootstrap-ams (predictions n use-confidence)
  (let* ((length 1000)
         (stddev-adjustment (sqrt (/ 450000 100000)))
         (v (make-array length :initial-element ())))
    (mgl-resample:bag
     (prediction-examples predictions)
     (lambda (in-bag)
       (let ((in-bag (subseq (shuffle in-bag) 0 100000)))
         (map-ams (lambda (ratio confidence ams)
                    (push ams
                          (aref v
                                (if use-confidence
                                    (floor (* (- 1 confidence)
                                              length))
                                    (floor (* ratio length))))))
                  (predictions-for-examples
                   predictions in-bag))))
     :n (* 2 n)
     :key #'example-source
     :weight #'example-weight)
    (loop for x across v
          for i upfrom 0
          collect (let ((mean (cl-num-utils.statistics:mean x))
                        (stddev (/ (sqrt
                                    (cl-num-utils.statistics:variance x))
                                   stddev-adjustment))
                        (min (loop for e in x minimize e))
                        (max (loop for e in x maximize e)))
                    (list (/ i length)
                          ;; min (- mean stddev) (+ mean stddev) max
                          (- mean stddev) (- mean stddev)
                          (+ mean stddev) (+ mean stddev)
                          (+ mean stddev))))))

(defun plot-bootstrap-ams (predictionss &key (n 50) use-confidence)
  (let ((smootheds (loop for i upfrom 0
                         for predictions in predictionss
                         collect (bootstrap-ams
                                  predictions n
                                  use-confidence))))
    (mgl-gnuplot:with-session ()
      (mgl-gnuplot:command "set yrange [3.0:4.0]")
      (if use-confidence
          (mgl-gnuplot:command "set xrange [0:0.15]")
          (mgl-gnuplot:command "set xrange [0.0:0.22]"))
      (mgl-gnuplot:command
       (make-instance
        'mgl-gnuplot:plot
        :mappings (mapcar (lambda (smoothed)
                            (make-instance
                             'mgl-gnuplot:data-mapping
                             :options "using 1:3:2:5:4 with candlesticks"
                             :data smoothed))
                          smootheds))))))
