;;;; List of known default/special names.

(in-package :file-types)


;;; File name list

(defparameter *file-name-list*
  '(;; Conventions
    ("README" "txt")
    ("Makefile" "mk")
    ;; Configuration files
    (".emacs" "el")))