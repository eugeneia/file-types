;;;; Access the file type hash table.

(in-package :file-types)


(defvar *file-type-hash* nil
  "Type to property map.")

(defvar *file-name-hash* nil
  "Special file name to type map.")


;;; Loading type and name dictionaries.

(defun reset-types/names ()
  (setf *file-type-hash* (make-hash-table :test #'equalp)
        *file-name-hash* (make-hash-table :test #'equalp)))

(defun load-types/names (&key type-dictionary name-dictionary)
  (loop for (types . properties) in type-dictionary do
       (dolist (type types)
         (setf (getf (gethash type *file-type-hash*) :tags)
               (append (getf (gethash type *file-type-hash*) :tags)
                       (getf properties :tags)))
         (when #1=(getf properties :mime)
               (setf (getf (gethash type *file-type-hash*) :mime)
                     #1#))))
  (loop for (name type) in name-dictionary do
       (setf (gethash name *file-name-hash*) type)))


;;; Initialize dictionaries with defaults.

(eval-when (:load-toplevel :execute)
  (unless (or *file-type-hash* *file-name-hash*)
    (reset-types/names)
    (load-types/names *file-type-list* *file-name-list*)))

;;; Access functions

(defun file-name (file)
  "Return type for FILE or NIL."
  (gethash (pathname-name file) *file-name-hash*))

(defun file-property (file property)
  "Return property of FILE or NIL."
  (let ((type (pathname-type file))
	(name (pathname-name file)))
    (or (and type (getf (gethash type *file-type-hash*)
			property))
	(and name (getf (gethash (file-name name) *file-type-hash*)
			property))
	(case property
	  (:tags '(:binary))
	  (:mime '("application" "octet-stream"))))))

(defun file-tags (file &optional tag)
  "→ _tags_

   *Arguments and Values:*

   _file_—a _pathname designator_.

   _tag_—a _keyword_.

   _tags_—a _list_ of _keywords_ which describe properties of a given
   _file_.

   *Description:*

   {file-tags} returns _tags_ for _file_. If _tag_ is given, {file-tags}
   acts as a predicate that tests if _tag_ is associated with _file_. In
   that case {file-tags} will return {nil} unless _tag_ would be present
   in _tags_."
  (let ((tag-list (file-property file :tags)))
    (if tag
	(when (member tag tag-list)
	  tag-list)
	tag-list)))

(defun file-mime (file)
  "→ _mime-type_

   *Arguments and Values:*

   _file_—a _pathname designator_.

   _mime-type_—a two-element _list_ containing both MIME type parts as
   _strings_ or {nil}.

   *Description:*

   {file-mime} returns the _mime-type_ of _file_ or {nil} if one could
   not be determined."
  (file-property file :mime))
