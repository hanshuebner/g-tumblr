;; -*- Lisp -*-

(defpackage :dpof
  (:use :cl)
  (:export #:revision
           #:date
           #:camera-type
           #:jobs
           #:id
           #:quantity
           #:parse-file
           #:image-pathname))

(in-package :dpof)

(defclass file ()
  ((revision :initarg :gen-rev
             :reader revision)
   (camera-type :initarg :gen-crt
                :reader camera-type)
   (date :initarg :gen-dtm
         :reader date)
   (jobs :reader jobs
         :accessor jobs%
         :initform nil)))

(defclass job ()
  ((id :initarg :prt-pid
       :reader id)
   (quantity :initarg :prt-qty
             :reader quantity)
   (image-pathname :initarg :image-pathname
                   :reader image-pathname)))

(defun parse-attribute (key value)
  (case key
    (:GEN-CRT
     (cl-ppcre:regex-replace "\"(.*)\".*" value "\\1"))
    (:GEN-DTM
     (apply #'local-time:encode-timestamp 0 (nreverse (mapcar #'parse-integer (cl-ppcre:split #\: value)))))
    ((:PRT-PID :PRT-QTY)
     (parse-integer value))
    (t
     value)))

(defun parse-file (pathname)
  (with-open-file (f pathname :external-format '(:character-encoding :ascii :line-termination :crlf))
    (let (file
          attributes)
      (loop
        (let ((line (or (read-line f nil)
                        (return))))
          (cond
            ((equal line "[JOB]")
             (if (null file)
                 (setf file (apply #'make-instance 'file (nreverse attributes)))
                 (push (apply #'make-instance 'job (nreverse attributes)) (jobs% file)))
             (setf attributes nil))
            ((cl-ppcre:register-groups-bind (key value) ("^([A-Z].*?)\\s*=\\s*(.*)$" line)
               (alexandria:when-let (key (find-symbol (cl-ppcre:regex-replace-all "\\s+" key "-") :keyword))
                 (push key attributes)
                 (push (parse-attribute key value) attributes)
                 t)))
            ((cl-ppcre:register-groups-bind (image-pathname) ("<IMG SRC\\s*=\\s*\"(.*)\">" line)
               (push :image-pathname attributes)
               (push (merge-pathnames (merge-pathnames image-pathname pathname)) attributes))))))
      (when attributes
        (push (apply #'make-instance 'job (nreverse attributes)) (jobs% file)))
      file)))
