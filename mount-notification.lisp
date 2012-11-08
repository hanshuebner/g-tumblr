;; -*- Lisp -*-

;; This is mostly verbatim from http://trac.clozure.com/ccl/wiki/Cocoa/DiskInsertions

(defpackage :mount-notification
  (:use :cl)
  (:export #:stop
           #:run))

(in-package :mount-notification)

(eval-when (:load-toplevel :execute)
  (ccl:open-shared-library "/System/Library/Frameworks/DiskArbitration.framework/DiskArbitration"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ccl:use-interface-dir :cocoa)
  (load "ccl:cocoa-ide;lib;cf-utils"))

(defvar *mount-handler* nil)
(defvar *unmount-handler* nil)

(ccl:defcallback disk-appeared (:address disk-ref :address context :void)
  (declare (ignore context))
  (let* ((dict (ccl:external-call "DADiskCopyDescription" :address disk-ref
                                  #>CFDictionaryRef)))
    (ccl:with-cfstring (key "DAVolumeName")
      (let ((name (#_CFDictionaryGetValue dict key)))
        (when (and *mount-handler*
                   (not (ccl:%null-ptr-p name)))
          (funcall *mount-handler* (ccl:%get-cfstring name)))))
    (#_CFRelease dict)))

(ccl:defcallback disk-disappeared (:address disk-ref :address context :void)
  (declare (ignore context))
  (let* ((dict (ccl:external-call "DADiskCopyDescription" :address disk-ref
                                  #>CFDictionaryRef)))
    (ccl:with-cfstring (key "DAVolumeName")
      (let ((name (#_CFDictionaryGetValue dict key)))
        (when (and *unmount-handler*
                   (not (ccl:%null-ptr-p name)))
          (funcall *unmount-handler* (ccl:%get-cfstring name)))))
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

(defun run (&key mount-handler unmount-handler)
  (setf *mount-handler* mount-handler
        *unmount-handler* unmount-handler)
  (ccl:process-run-function "disk watcher"
                            (lambda ()
                              (setf *disk-watcher-run-loop* 
                                    (#_CFRunLoopGetCurrent))
                              (disk-watcher)
                              (setf *disk-watcher-run-loop* nil))))

(defun stop ()
  (when *disk-watcher-run-loop*
    (#_CFRunLoopStop *disk-watcher-run-loop*)))
