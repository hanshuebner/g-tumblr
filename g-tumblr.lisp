;; -*- Lisp -*-

(defpackage :g-tumblr
  (:use :cl)
  (:export #:main))

(in-package :g-tumblr)

(defun process-print-requests (pathname)
  (let ((file (dpof:parse-file pathname)))
    (dolist (job (dpof:jobs file))
      (format t "process ~A (quantity ~A)~%" (dpof:image-pathname job) (dpof:quantity job)))))

(defun volume-mounted (name)
  (format t "Volume ~A mounted~%" name)
  (alexandria:when-let (pathname (probe-file
                                  (merge-pathnames
                                   (make-pathname :name "AUTPRINT"
                                                  :type "MRK"
                                                  :directory `(:relative ,name "MISC"))
                                  #P"/Volumes/")))
    (process-print-requests pathname)))

(defun main ()
  (mount-notification:run :mount-handler 'volume-mounted))
