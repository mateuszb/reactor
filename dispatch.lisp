(in-package :reactor.dispatch)

(defvar *dispatcher*)

(defstruct dispatcher
  (socket->ctx-tab (make-hash-table))
  (reactor #+linux (make-reactor)))

(defstruct context
  rx-handler
  rx-evts
  tx-handler
  tx-evts)

(defmacro with-dispatcher ((dispatcher) &body body)
  `(let ((*dispatcher* ,dispatcher))
     (let ((reactor:*reactor* (slot-value *dispatcher* 'reactor)))
       ,@body)))

(defun on-read (socket rxfun &optional (replace-evts nil))
  (let ((sd (sb-bsd-sockets:socket-file-descriptor socket)))
    #+linux
    (with-slots ((tab socket->ctx-tab) (r reactor)) *dispatcher*
      (multiple-value-bind (ctx existsp) (gethash sd tab)
	(if existsp
	    (with-slots (rx-handler rx-evts tx-evts) ctx
	      (setf rx-handler rxfun
		    rx-evts (if replace-evts (list +EPOLLIN+ +EPOLLET+)
				(union rx-evts (list +EPOLLIN+ +EPOLLET+))))
	      (epoll-mod (reactor-handle r) sd (union tx-evts rx-evts)))
	    (let ((ctx (make-context :rx-handler rxfun :rx-evts (list +EPOLLIN+ +EPOLLET+))))
	      (let ((ret (epoll-add (reactor-handle r) sd (list +EPOLLIN+ +EPOLLET+))))
		(if (zerop ret)
		    (setf (gethash sd tab) ctx)
		    nil))))))))

(defun on-write (socket txfun &optional (replace-evts nil))
  (let ((sd (sb-bsd-sockets:socket-file-descriptor socket)))
    #+linux
    (with-slots ((tab socket->ctx-tab) (r reactor)) *dispatcher*
      (multiple-value-bind (ctx existsp) (gethash sd tab)
	(if existsp
	    (with-slots (tx-handler tx-evts rx-evts) ctx
	      (setf tx-handler txfun
		    tx-evts (if replace-evts (list +EPOLLOUT+ +EPOLLET+)
				(union tx-evts (list +EPOLLOUT+ +EPOLLET+))))
	      (epoll-mod (reactor-handle r) sd (union rx-evts tx-evts)))
	    (progn
	      (let ((ctx (make-context :rx-handler txfun :rx-evts (list +EPOLLOUT+ +EPOLLET+))))
		(let ((ret (epoll-add (reactor-handle r) sd (list +EPOLLOUT+ +EPOLLET+))))
		  (if (zerop ret)
		      (setf (gethash sd tab) ctx)
		      nil)))))))))


(defun dispatch-events (evts)
  (loop for evt in evts
     with tab = (slot-value *dispatcher* 'socket->ctx-tab)
     do
       (multiple-value-bind (ctx existsp) (gethash (getf evt :fd) tab)
	 (when existsp
	   (with-slots (rx-handler tx-handler) ctx
	     (when rx-handler
	       (funcall rx-handler))
	     (when tx-handler
	       (funcall tx-handler)))))))
