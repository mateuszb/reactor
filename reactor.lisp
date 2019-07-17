(in-package :reactor)

(defvar *reactor*)

(defstruct reactor
  (handle #+linux (make-epoll) #+os-macosx (make-kqueue)))

(defmacro with-reactor ((reactor) &body body)
  `(let ((*reactor* ,reactor))
     ,@body))

(defun wait-for-events ()
  #+linux
  (with-slots (handle) *reactor*
    (epoll-events handle))
  #+os-macosx
  (with-slots (handle) *reactor*
    (kqueue-events handle)))
