;;; http://blog.quicklisp.org/2011/08/going-back-in-dist-time.html
(ql-dist:install-dist
 "http://beta.quicklisp.org/dist/quicklisp/2014-08-26/distinfo.txt"
 :replace t :prompt nil)

(load (merge-pathnames "../rumcajsz.asd" *load-pathname*))

(ql:quickload "rumcajsz")
