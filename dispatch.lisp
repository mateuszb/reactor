(in-package :reactor.dispatch)

(defvar *dispatcher*)

(defstruct dispatcher
  (handle #+linux (make-epoll) #+os-macosx (make-kqueue))
  (socktab (make-hash-table :synchronized t)))

(defstruct context
  rx-handler
  rx-evts
  tx-handler
  tx-evts)

(defmacro with-dispatcher ((dispatcher) &body body)
  `(let ((*dispatcher* ,dispatcher))
     ,@body))

(defun on-read (socket rxfun &optional (replace-evts nil))
  (let ((sd (sb-bsd-sockets:socket-file-descriptor socket)))
    #+linux
    (with-slots (handle socktab) *dispatcher*
      (multiple-value-bind (ctx existsp) (gethash sd socktab)
	(if existsp
	    (with-slots (rx-handler rx-evts) ctx
	      (setf rx-handler rxfun
		    rx-evts (if replace-evts (list +EPOLLIN+ +EPOLLET+) 
				(union rx-evts (list +EPOLLIN+ +EPOLLET+))))
	      (epoll-mod handle sd rx-evts))
	    (progn
	      (let ((ctx (make-context :rx-handler rxfun :rx-evts (list +EPOLLIN+ +EPOLLET+))))
		(let ((ret (epoll-add handle sd (list +EPOLLIN+ +EPOLLET+))))
		  (if (zerop ret)
		      (setf (gethash sd socktab) ctx)
		      nil)))))))))

(defun on-write (sd txfun)
  #+linux
  (with-slots (handle socktab) *dispatcher*
    (case (epoll-add handle sd (list +EPOLLOUT+))
      (0 (setf (gethash sd socktab nil)
	       (make-context)))))
  )

(defun wait-for-events ()
  #+linux
  (with-slots (handle) *dispatcher*
    (epoll-events handle))
  #+os-macosx
  (with-slots (handle) dispatcher
    (kqueue-events handle)))
