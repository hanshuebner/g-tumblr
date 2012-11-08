(defsystem :g-tumblr
    :description "Camera to tumblr uploader"
  :serial t
  :depends-on (:alexandria
               :cl-ppcre
               :local-time
               :cl-oauth)
  :components ((:file "dpof")
               (:file "mount-notification")
               (:file "g-tumblr")))
