(in-package :reactor.dispatch)

(defvar *dispatcher*)

(defstruct dispatcher
  (socktab (make-hash-table))
  (reactor #+linux (make-reactor)))

(defstruct context
  socket
  rx-handler
  rx-evts
  tx-handler
  tx-evts)

(defmacro with-dispatcher ((dispatcher) &body body)
  `(let ((*dispatcher* ,dispatcher))
     (let ((reactor:*reactor* (slot-value *dispatcher* 'reactor)))
       ,@body)))

(defun on-read (socket rxfun &optional (replace-evts nil))
  (let ((sd (socket-fd socket)))
    #+linux
    (with-slots ((tab socktab) (r reactor)) *dispatcher*
      (multiple-value-bind (ctx existsp) (gethash sd tab)
	(if existsp
	    (with-slots (rx-handler rx-evts tx-evts) ctx
	      (setf rx-handler rxfun
		    rx-evts (if replace-evts (list +EPOLLIN+ +EPOLLET+)
				(union rx-evts (list +EPOLLIN+ +EPOLLET+))))
	      (when (= -1 (epoll-mod (reactor-handle r) sd (union tx-evts rx-evts)))
		(error "epoll-mod error")))
	    (let ((ctx (make-context :socket socket
				     :rx-handler rxfun
				     :rx-evts (list +EPOLLIN+ +EPOLLET+))))
	      (let ((ret (epoll-add (reactor-handle r) sd (list +EPOLLIN+ +EPOLLET+))))
		(when (= ret -1)
		  (error "epoll-add error"))
		(if (zerop ret)
		    (setf (gethash sd tab) ctx)
		    nil))))))))

(defun on-write (socket txfun &optional (replace-evts nil))
  (let ((sd (socket-fd socket)))
    #+linux
    (with-slots ((tab socktab) (r reactor)) *dispatcher*
      (multiple-value-bind (ctx existsp) (gethash sd tab)
	(if existsp
	    (with-slots (tx-handler tx-evts rx-evts) ctx
	      (setf tx-handler txfun
		    tx-evts (if replace-evts (list +EPOLLOUT+ +EPOLLET+)
				(union tx-evts (list +EPOLLOUT+ +EPOLLET+))))
	      (when (= (epoll-mod (reactor-handle r) sd (union rx-evts tx-evts)) -1)
		(error "epoll-mod write error")))
	    (progn
	      (let ((ctx (make-context :socket socket
				       :rx-handler txfun
				       :rx-evts (list +EPOLLOUT+ +EPOLLET+))))
		(let ((ret (epoll-add (reactor-handle r) sd (list +EPOLLOUT+ +EPOLLET+))))
		  (when (= ret -1)
		    (error "epoll-add write error"))
		  (if (zerop ret)
		      (setf (gethash sd tab) ctx)
		      nil)))))))))

(defun del-write (socket)
  (let ((sd (socket-fd socket)))
    #+linux
    (with-slots ((tab socktab) (r reactor)) *dispatcher*
      (multiple-value-bind (ctx existsp) (gethash sd tab)
	(when existsp
	  (with-slots (tx-evts rx-evts) ctx
	    (setf tx-evts (remove +EPOLLOUT+ tx-evts))
	    (when (= (epoll-mod (reactor-handle r) sd (union rx-evts tx-evts)) -1)
	      (error "epoll-mod del write error"))))))))

(defun rem-socket (socket)
  (let ((sd (socket-fd socket)))
    #+linux
    (with-slots ((tab socktab) (r reactor)) *dispatcher*
      (multiple-value-bind (ctx existsp) (gethash sd tab)
	(declare (ignore ctx))
	(when existsp
	  (let ((err (epoll-del (reactor-handle r) sd)))
	    (when (= err -1)
	      (format t "error: ~a~%" (socket::errno))
	      (error "epoll-del rem socket error"))
	    (remhash sd tab)
	    err))))))

(defun dispatch-events (evts)
  (format t "dispatching events: ~a~%" evts)
  (loop for evt in evts
     with tab = (slot-value *dispatcher* 'socktab)
     do
       (format t "dispatching event ~a~%" evt)
       (multiple-value-bind (ctx existsp) (gethash (getf evt :fd) tab)
	 (when existsp
	   (with-slots (rx-handler tx-handler) ctx
	     (when (and rx-handler (eq (getf evt :filter) :in))
	       (funcall rx-handler ctx evt))
	     (when (and tx-handler (eq (getf evt :filter) :out))
	       (funcall tx-handler ctx evt)))))))
