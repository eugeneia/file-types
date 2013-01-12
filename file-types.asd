;;;; System definition for file-types.

(defpackage file-types-asd
  (:use :cl :asdf))

(in-package :file-types-asd)

(defsystem file-types
  :components ((:file "package")
               (:file "types" :depends-on ("package"))
               (:file "names" :depends-on ("package"))
               (:file "access" :depends-on ("package" "types" "names"))))
