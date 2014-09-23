;;;; Not part of the solution, only used for exploration.

(in-package :higgs-boson)

(defparameter *bpn-predictions*
  (average-overlapping-predictions
   (rumcajsz::load-bag
    (lambda (i)
      (merge-pathnames
       (format nil "final/bag-~S-out-of-bag-predictions-bpn" i)
       *model-dir*)))))

(defparameter *xg-predictions*
  (average-overlapping-predictions
   (rumcajsz::load-bag
    (lambda (i)
      (merge-pathnames
       (format nil "final/bag-~S-out-of-bag-predictions-xgboost" i)
       *model-dir*)))))

(plot-ams (list *bpn-predictions*
                *xg-predictions*
                (average-overlapping-predictions
                 (list *bpn-predictions*
                       (scale-predictions 0.08 *xg-predictions*)))))

(plot-smoothed-ams (list *bpn-predictions*
                         *xg-predictions*
                         (average-overlapping-predictions
                          (list *bpn-predictions*
                                (scale-predictions 0.08 *xg-predictions*))))
                   :stddev 0.003)


(defparameter *xg-predictions-1*
  (average-overlapping-predictions
   (rumcajsz::load-bag
    (lambda (i)
      (when (< i 8)
        (merge-pathnames
         (format nil "final/bag-~S-out-of-bag-predictions-xgboost" i)
         *model-dir*))))))

(defparameter *xg-predictions*
  (average-overlapping-predictions
   (rumcajsz::load-bag
    (lambda (i)
      (merge-pathnames
       (format nil "xxx/bag-~S-out-of-bag-predictions-xgboost" i)
       *model-dir*)))))

(plot-ams (list *xg-predictions-1*
                *xg-predictions*))

(plot-ams (list *bpn-predictions*
                (average-overlapping-predictions
                 (list *bpn-predictions*
                       (scale-predictions 0.08 *xg-predictions-1*)))
                (average-overlapping-predictions
                 (list *bpn-predictions*
                       (scale-predictions 0.08 *xg-predictions*)))))
