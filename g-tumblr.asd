(defsystem :g-tumblr
    :description "Camera to tumblr uploader"
  :serial t
  :depends-on (:alexandria
               :cl-ppcre)
  :components ((:file "emerald")))
