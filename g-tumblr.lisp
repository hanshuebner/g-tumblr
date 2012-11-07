;; -*- Lisp -*-

;; Use print requests in DPOF format on SD-Card to upload selected
;; pictures to tumblr

;; DPOF summary: http://panasonic.jp/dc/dpof_110/white_e.htm

(defpackage :g-tumblr
  (:use :cl))

(in-package :g-tumblr)

(eval-when (:load-toplevel :execute)
  (ccl:open-shared-library "/System/Library/Frameworks/DiskArbitration.framework/DiskArbitration"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ccl:use-interface-dir :cocoa)
  (load "ccl:cocoa-ide;lib;cf-utils"))

(defun process-dpof-file (pathname)
  )

(defun volume-mounted (name)
  (format t "Volume ~A mounted~%" name)
  (alexandria:when-let (pathname (probe-file
                                  (merge-pathnames
                                   (make-pathname :name "AUTPRINT"
                                                  :type "MRK"
                                                  :directory `(:relative ,name "MISC"))
                                  #P"/Volumes/")))
    (process-print-requests pathname)))

(defun volume-unmounted (name)
  (format t "Volume ~A umounted~%" name))

(ccl:defcallback disk-appeared (:address disk-ref :address context :void)
  (declare (ignore context))
  (let* ((dict (ccl:external-call "DADiskCopyDescription" :address disk-ref
                                  #>CFDictionaryRef)))
    (ccl:with-cfstring (key "DAVolumeName")
      (let ((name (#_CFDictionaryGetValue dict key)))
        (unless (ccl:%null-ptr-p name)
          (volume-mounted (ccl:%get-cfstring name)))))
    (#_CFRelease dict)))

(ccl:defcallback disk-disappeared (:address disk-ref :address context :void)
  (declare (ignore context))
  (let* ((dict (ccl:external-call "DADiskCopyDescription" :address disk-ref
                                  #>CFDictionaryRef)))
    (ccl:with-cfstring (key "DAVolumeName")
      (let ((name (#_CFDictionaryGetValue dict key)))
        (unless (ccl:%null-ptr-p name)
          (volume-unmounted (ccl:%get-cfstring name)))))
    (#_CFRelease dict)))

(defun disk-watcher ()
  (let ((session (ccl:external-call "DASessionCreate"
                                    :address ccl:+null-ptr+ :address)))
    (ccl:external-call "DARegisterDiskAppearedCallback"
                       :address session :address ccl:+null-ptr+
                       :address disk-appeared :address ccl:+null-ptr+)
    (ccl:external-call "DARegisterDiskDisappearedCallback"
                       :address session :address ccl:+null-ptr+
                       :address disk-disappeared :address ccl:+null-ptr+)
    (ccl:external-call "DASessionScheduleWithRunLoop"
                       :address session
                       :address (#_CFRunLoopGetCurrent)
                       :address #&kCFRunLoopDefaultMode)
    ;; this blocks
    (#_CFRunLoopRun)
    (#_CFRelease session)))

(defparameter *disk-watcher-run-loop* nil)

(defun run-disk-watcher ()
  (ccl:process-run-function "disk watcher"
                            (lambda ()
                              (setq *disk-watcher-run-loop* 
                                    (#_CFRunLoopGetCurrent))
                              (disk-watcher)
                              (setq *disk-watcher-run-loop* nil))))

(defun stop-disk-watcher ()
  (#_CFRunLoopStop *disk-watcher-run-loop*))
