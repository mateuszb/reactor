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

(defun reactor-add (reactor socket filter flags)
  (let ((translated-filter (filter->enum filter)))
    #+linux
    (epoll-add (reactor-handle reactor) (socket-fd socket) translated-filter flags)
    #+freebsd
    (kqueue-add (reactor-handle reactor) (socket-fd socket) translated-filter flags)
    #+os-macosx
    (kqueue-add (reactor-handle reactor) (socket-fd socket) translated-filter flags)))

(defun reactor-remove (reactor socket filter)
  (let ((translated-filter (filter->enum filter)))
    #+linux
    (epoll-del (reactor-handle reactor) (socket-fd socket) translated-filter)
    #+os-macosx
    (kqueue-del (reactor-handle reactor) (socket-fd socket) translated-filter)
    #+freebsd
    (kqueue-del (reactor-handle reactor) (socket-fd socket) translated-filter)))

(defun reactor-modify (reactor socket filter flags)
  (let ((translated-filter (filter->enum event-filter)))
    #+linux
    (epoll-mod (reactor-handle reactor) (socket-fd socket) translated-filter flags)
    #+os-macosx
    (kqueue-add (reactor-handle reactor) (socket-fd socket) translated-filter flags)
    #+freebsd
    (kqueue-add (reactor-handle reactor) (socket-fd socket) translated-filter flags)))

;; potentialy not needed anymore
(defun reactor-remove-socket (socket)
  (let ((sd (socket-fd socket)))
    (with-slots ((tab socktab) (r reactor)) *dispatcher*
      (multiple-value-bind (ctx existsp) (gethash sd tab)
	(declare (ignore ctx))
	(when existsp
	  (remhash sd tab)
	  (let ((err (reactor-del (reactor-handle r) sd)))
	    (when (= err -1)
	      #+debug
	      (format t "error: ~a~%" (socket::errno))
	      (error "error when deleting socket"))
	    err))))))

(defun close-reactor (reactor)
  (sb-posix:close
   (reactor-handle
    reactor)))
