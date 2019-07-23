(in-package :reactor)

(defvar *reactor*)

(defstruct reactor
  (handle #+linux (make-epoll) #+os-macosx (make-kqueue)))

(defmacro with-reactor ((reactor) &body body)
  `(let ((*reactor* ,reactor))
     ,@body))

(defun wait-for-events (&optional (max-events 128) (timeout -1))
  #+linux
  (with-slots (handle) *reactor*
    (epoll-events handle max-events timeout))
  #+os-macosx
  (with-slots (handle) *reactor*
    (kqueue-events handle)))
