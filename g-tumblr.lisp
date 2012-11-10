;; -*- Lisp -*-

(defpackage :g-tumblr
  (:use :cl)
  (:export #:main))

(in-package :g-tumblr)

(defparameter *max-width* 1200)
(defparameter *config-pathname* "~/.g-tumblr-config.lisp")

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
        (let* ((resizep (< *max-width* original-height))
               (width (min *max-width* original-height))
               (height (floor (* original-width (/ width original-height)))))
          (cl-gd:with-image (rotated-image original-height original-width t)
            (cl-gd:copy-image original-image rotated-image 0 0 (floor original-height 2) (floor original-width 2) original-width original-height
                              :rotate t :angle rotation-angle)
            (cl-gd:with-image (rescaled-image width height t)
              (cl-gd:copy-image rotated-image rescaled-image 0 0 0 0 original-height original-width
                                :dest-width width :dest-height height :resample resizep :resize resizep)
              (cl-gd:write-image-to-file output-pathname :image rescaled-image :if-exists :supersede))))
        (let* ((resizep (< *max-width* original-width))
               (width (min *max-width* original-width))
               (height (* original-height (/ width original-width))))
          (cl-gd:with-image (rescaled-image width height t)
            (cl-gd:copy-image original-image rescaled-image 0 0 0 0 original-width original-height
                              :dest-width width :dest-height height :resample resizep :resize resizep)
            (cl-gd:write-image-to-file output-pathname :image rescaled-image :if-exists :supersede))))
      output-pathname)))

(defparameter *get-request-token-endpoint* "http://www.tumblr.com/oauth/request_token")
(defparameter *get-access-token-endpoint* "http://www.tumblr.com/oauth/access_token")
(defparameter *authorize-endpoint* "http://www.tumblr.com/oauth/authorize")
(defparameter *hunchentoot-port* 10721 "Port to use for Hunchentoot as callback destination on localhost")
(defvar *queue* nil "Queue to send received oauth_verifier over")

(defmacro with-temporary-webserver ((acceptor) &body body)
  `(unwind-protect
        (progn
          (hunchentoot:start ,acceptor)
          ,@body)
     (hunchentoot:stop ,acceptor)))

(defmacro with-temporary-queue ((queue) &body body)
  `(unwind-protect
        (progn
          (setf ,queue (lparallel.queue:make-queue))
          ,@body)
     (setf ,queue nil)))

(defmacro with-temporary-kernel ((count) &body body)
  `(let ((lparallel:*kernel* (lparallel:make-kernel ,count)))
     (unwind-protect
          (progn
            ,@body)
       (lparallel:end-kernel :wait nil))))

(hunchentoot:define-easy-handler (callback :uri "/") (oauth_verifier)
  (lparallel.queue:push-queue oauth_verifier *queue*)
  "Your access token has been generated, you can close this window.")

;; According to tumblr support, xAuth permission is granted only to
;; mass-deployed applications.  For single user applications like this
;; one, you are supposed to generate an access token using the
;; standard OAuth flow, which is implemented by the below or by
;; https://gist.github.com/2603387

(defun configure (blog-name consumer-key consumer-secret)
  (let* ((consumer-token (cl-oauth:make-consumer-token :key consumer-key :secret consumer-secret))
         (callback-uri (format nil "http://localhost:~A/" *hunchentoot-port*))
         (request-token (cl-oauth:obtain-request-token *get-request-token-endpoint* consumer-token
                                                       :callback-uri callback-uri))
         (auth-uri (cl-oauth:make-authorization-uri *authorize-endpoint* request-token))
         (acceptor (make-instance 'hunchentoot:easy-acceptor :port *hunchentoot-port*)))
    (with-temporary-kernel (2)
      (with-temporary-queue (*queue*)
        (with-temporary-webserver (acceptor)
          (inferior-shell:run (format nil "open ~A" (puri:uri auth-uri)))
          (format t "Please authorize the access on this URL:~%~A~%" (puri:uri auth-uri))
          (lparallel:future
            (format t "If the redirection in the browser failed, paste the value of the oauth_verifier parameter from the URL here:~%")
            (lparallel.queue:push-queue (read-line) *queue*))
          (let ((oauth-verifier (lparallel.queue:pop-queue *queue*)))
            (cl-oauth:authorize-request-token request-token)
            (setf (cl-oauth:request-token-verification-code request-token) oauth-verifier)
            (let ((access-token (cl-oauth:obtain-access-token *get-access-token-endpoint* request-token)))
              (with-open-file (f *config-pathname* :direction :output :if-exists :new-version)
                (write (list :blog-name blog-name
                             :consumer-key (cl-oauth:token-key consumer-token)
                             :consumer-secret (cl-oauth:token-secret consumer-token)
                             :access-key (cl-oauth:token-key access-token)
                             :access-secret (cl-oauth:token-secret access-token))
                       :stream f)))))))))

(defun read-configuration ()
  (with-open-file (f *config-pathname*)
    (destructuring-bind (&key blog-name consumer-key consumer-secret access-key access-secret) (read f)
      (list :blog-name blog-name
            :access-token (make-instance 'cl-oauth:access-token
                                         :consumer (make-instance 'cl-oauth:consumer-token
                                                                  :key consumer-key
                                                                  :secret consumer-secret)
                                         :key access-key
                                         :secret access-secret)))))

(defun url-encode-octets (octets)
  "URL-encode a string."
  (with-output-to-string (s)
    (loop for octet across octets
          for c = (code-char octet)     ; ugh
          do (cond
               ((or (char<= #\0 c #\9)
                    (char<= #\a c #\z)
                    (char<= #\A c #\Z)
                    (find c "$-_.!*'()," :test #'char=))
                (write-char c s))
               (t (format s "%~2,'0x" octet))))))

(defun post-image (pathname)
  (format t "pathname: ~A~%" pathname)
  (temporary-file:with-open-temporary-file (rescaled-image-pathname :template (format nil "temporary-files:%.~A" (pathname-type pathname)))
    (normalize-image pathname rescaled-image-pathname)
    (let ((drakma:*text-content-types* '(("text" . nil) ("application" . "json"))))
      (destructuring-bind (&key blog-name access-token) (read-configuration)
        (cl-oauth:access-protected-resource (format nil "https://api.tumblr.com/v2/blog/~A.tumblr.com/post" blog-name)
                                            access-token
                                            :request-method :post
                                            :user-parameters `(("type" . "text")
                                                               ("body" . "he")
                                                               ;; status: with data[0] parameters, 401
                                                               ("data[0]" . ,(url-encode-octets (alexandria:read-file-into-byte-vector rescaled-image-pathname)))))))))

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
