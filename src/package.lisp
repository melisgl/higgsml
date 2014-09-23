(defpackage :higgs-boson
  (:use :cl :mgl-mat :mgl-resample :mgl-example-util)
  (:export #:*data-dir*
           #:*model-dir*
           #:*submission-dir*
           #:*unlabeled-training-file*
           #:*training-file*
           #:*test-file*
           #:unlabeled-training-examples
           #:training-examples
           #:test-examples
           #:write-example
           #:write-training-csv-header
           #:write-test-csv-header
           #:save-training
           #:save-test
           #:load-submission
           ;;
           #:*labels*
           #:*n-labels*
           #:*n-features*
           #:*feature-names*
           #:feature-index
           #:*radian-feature-indices*
           #:index->label
           #:label-index
           #:*feature-names-with-missing-values*
           ;;
           #:example
           #:example-event-id
           #:example-weight
           #:example-label
           #:example-source
           #:example-source-fine-grained
           #:example-features
           #:encoded-example
           #:encoded-example-encoded-features
           ;;
           #:describe-examples
           #:scaling-factor
           #:approximate-median-significance
           #:approximate-median-significance-for-s-set
           #:find-best-threshold
           #:map-ams
           #:collect-ams
           #:threshold-confidence
           #:threshold-confidence-by-ratio
           #:log-thresholds
           ;;
           #:generate-submission-by-confidence
           #:generate-submission-by-ratio
           #:load-predictions
           #:save-predictions
           #:scale-predictions
           #:average-predictions
           #:average-overlapping-predictions
           #:predictions-for-examples
           #:prediction-examples
           #:sum-weights-for-labels))

(defpackage :rumcajsz
  (:use :cl :higgs-boson
        :mgl-mat :mgl-resample :mgl-util :mgl-train :mgl-bm :mgl-gd :mgl-bp
        :mgl-example-util)
  (:export #:main))
