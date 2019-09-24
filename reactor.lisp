(in-package :reactor)

(defvar *reactor*)

(defstruct reactor
  (handle #+linux (make-epoll)
	  #+os-macosx (make-kqueue)
	  #+freebsd (make-kqueue)))

(defmacro with-reactor ((reactor) &body body)
  `(let ((*reactor* ,reactor))
     ,@body))

(defun wait-for-events (&optional (max-events 128) (timeout -1))
  (with-slots (handle) *reactor*
    (let ((evts))
      #+linux
      (setf evts (epoll-events handle max-events timeout))
      #+os-macosx
      (setf evts (kqueue-events handle))
      #+freebsd
      (setf evts (kqueue-events handle))

      (mapcar #'platform->universal-event evts))))

(defgeneric handle-key (handle))

(defun reactor-add (reactor handle filters flags)
  (let ((translated-filters (filters->enums filters))
	(translated-flags (flags->enums flags)))
    #+linux
    (epoll-add
     (reactor-handle reactor)
     handle
     translated-filters
     translated-flags)
    #+freebsd
    (kqueue-add
     (reactor-handle reactor)
     handle
     translated-filter translated-flags)
    #+os-macosx
    (kqueue-add
     (reactor-handle reactor)
     handle
     translated-filter translated-flags)))

(defun reactor-remove (reactor handle filters)
  (let ((translated-filters (filters->enums filters)))
    #+linux
    (epoll-del
     (reactor-handle reactor)
     handle)
    #+os-macosx
    (kqueue-del
     (reactor-handle reactor)
     handle
     translated-filters)
    #+freebsd
    (kqueue-del
     (reactor-handle reactor)
     handle
     translated-filters)))

(defun reactor-disable (reactor handle filters flags)
  (let ((translated-filters (filters->enums filters))
	(translated-flags (flags->enums flags)))
    #+linux
    (epoll-mod
     (reactor-handle reactor)
     handle
     translated-filters
     translated-flags)
    #+os-macosx
    (kqueue-del
     (reactor-handle reactor)
     handle translated-filter)
    #+freebsd
    (kqueue-del
     (reactor-handle reactor)
     handle translated-filter)))

(defun reactor-modify (reactor handle filters flags)
  (let ((translated-filters (filters->enums filters))
	(translated-flags (flags->enums flags)))
    (format t "reactor-modify: translated flags=~a~%" translated-flags)
    #+linux
    (epoll-mod
     (reactor-handle reactor)
     handle
     translated-filters
     translated-flags)
    #+os-macosx
    (kqueue-add
     (reactor-handle reactor)
     handle
     translated-filter
     translated-flags)
    #+freebsd
    (kqueue-add
     (reactor-handle reactor)
     handle
     translated-filter
     translated-flags)))

(defun close-reactor (reactor)
  (sb-posix:close
   (reactor-handle
    reactor)))
