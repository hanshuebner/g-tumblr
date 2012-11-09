;; -*- Lisp -*-

(defpackage :g-tumblr
  (:use :cl)
  (:export #:main))

(in-package :g-tumblr)

(defparameter +max-width+ 1200)

(defun get-image-orientation (pathname)
  (zpb-exif:parsed-exif-value :Orientation (zpb-exif:make-exif pathname)))

(defun normalize-image (input-pathname output-pathname)
  (cl-gd:with-image-from-file (original-image input-pathname)
    (let ((original-width (cl-gd:image-width original-image))
          (original-height (cl-gd:image-height original-image)))
      (alexandria:if-let (rotation-angle (ecase (get-image-orientation input-pathname)
                                           (:rotated-90 90)
                                           (:rotated-270 270)
                                           (:normal nil)))
        (let* ((resizep (< +max-width+ original-height))
               (width (min +max-width+ original-height))
               (height (floor (* original-width (/ width original-height)))))
          (cl-gd:with-image (rotated-image original-height original-width t)
            (cl-gd:copy-image original-image rotated-image 0 0 (floor original-height 2) (floor original-width 2) original-width original-height
                              :rotate t :angle rotation-angle)
            (cl-gd:with-image (rescaled-image width height t)
              (cl-gd:copy-image rotated-image rescaled-image 0 0 0 0 original-height original-width
                                :dest-width width :dest-height height :resample resizep :resize resizep)
              (cl-gd:write-image-to-file output-pathname :image rescaled-image :if-exists :supersede))))
        (let* ((resizep (< +max-width+ original-width))
               (width (min +max-width+ original-width))
               (height (* original-height (/ width original-width))))
          (cl-gd:with-image (rescaled-image width height t)
            (cl-gd:copy-image original-image rescaled-image 0 0 0 0 original-width original-height
                              :dest-width width :dest-height height :resample resizep :resize resizep)
            (cl-gd:write-image-to-file output-pathname :image rescaled-image :if-exists :supersede))))
      output-pathname)))

(defparameter *get-access-token-endpoint* "http://www.tumblr.com/oauth/access_token")

(defun configure (consumer-key consumer-secret username password)
  (let* ((consumer-token (cl-oauth:make-consumer-token :key consumer-key :secret consumer-secret))
         (access-token (cl-oauth:obtain-access-token *get-access-token-endpoint* nil :consumer-token consumer-token :xauth-username username :xauth-password password)))
    (with-open-file (f "~/.g-tumblr-config.lisp" :direction :output :if-exists :new-version)
      (write (list :consumer-key (cl-oauth:token-key consumer-token)
                   :consumer-secret (cl-oauth:token-secret consumer-token)
                   :access-key (cl-oauth:token-key access-token)
                   :access-secret (cl-oauth:token-secret access-token))
             :stream f))))

(defun post-image (pathname)
  (format t "pathname: ~A~%" pathname)
  (temporary-file:with-open-temporary-file (rescaled-image-pathname :template (format nil "temporary-files:%.~A" (pathname-type pathname)))
    (normalize-image pathname rescaled-image-pathname)))

(defun process-print-requests (pathname)
  (let ((file (dpof:parse-file pathname)))
    (dolist (job (dpof:jobs file))
      (format t "process ~A (quantity ~A)~%" (dpof:image-pathname job) (dpof:quantity job))
      (post-image (dpof:image-pathname job)))))

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
