(defsystem :g-tumblr
    :description "Camera to tumblr uploader"
  :serial t
  :depends-on (:alexandria
               :cl-ppcre
               :local-time
               :temporary-file
               :zpb-exif
               :hunchentoot
               :lparallel
               :inferior-shell
               :yason
               :cl-gd
               :cl-oauth)
  :components ((:file "dpof")
               (:file "mount-notification")
               (:file "g-tumblr")))
