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
  tx-evts
  disconnect-handler)

(defmacro with-dispatcher ((dispatcher) &body body)
  `(let ((*dispatcher* ,dispatcher))
     (let ((reactor:*reactor* (slot-value *dispatcher* 'reactor)))
       ,@body)))

(defun on-disconnect (socket handler)
  (let ((sd (socket-fd socket)))
    #+linux
    (with-slots ((tab socktab) (r reactor)) *dispatcher*
      (multiple-value-bind (ctx existsp) (gethash sd tab)
	(if existsp
	    (with-slots (disconnect-handler) ctx
	      (setf disconnect-handler handler))
	    (let ((ctx (make-context :socket socket :disconnect-handler handler)))
	      (setf (gethash sd tab) ctx)))))))

(defun on-read (socket rxfun)
  (let ((sd (socket-fd socket)))
    #+linux
    (with-slots ((tab socktab) (r reactor)) *dispatcher*
      (multiple-value-bind (ctx existsp) (gethash sd tab)
	(if existsp
	    (with-slots (rx-handler rx-evts tx-evts) ctx
	      (setf rx-handler rxfun
		    rx-evts (read-event-mask))

	      (when (= -1 (epoll-mod (reactor-handle r) sd (union tx-evts rx-evts)))
		(error "epoll-mod error")))
	    (let ((ctx (make-context :socket socket
				     :rx-handler rxfun
				     :rx-evts (read-event-mask))))
	      (let ((ret (epoll-add (reactor-handle r) sd (read-event-mask))))
		(when (= ret -1)
		  (error "epoll-add error"))
		(if (zerop ret)
		    (setf (gethash sd tab) ctx)
		    nil))))))))

(defun on-write (socket txfun)
  (let ((sd (socket-fd socket)))
    #+linux
    (with-slots ((tab socktab) (r reactor)) *dispatcher*
      (multiple-value-bind (ctx existsp) (gethash sd tab)
	(if existsp
	    (with-slots (tx-handler tx-evts rx-evts) ctx
	      (setf tx-handler txfun
		    tx-evts (write-event-mask))
	      (when (= (epoll-mod (reactor-handle r) sd (union rx-evts tx-evts)) -1)
		(error "epoll-mod write error")))
	    (progn
	      (let ((ctx (make-context :socket socket
				       :tx-handler txfun
				       :tx-evts (write-event-mask))))
		(let ((ret (epoll-add (reactor-handle r) sd (write-event-mask))))
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
	      #+debug
	      (format t "error: ~a~%" (socket::errno))
	      (error "epoll-del rem socket error"))
	    (remhash sd tab)
	    err))))))

(defun dispatch-events (evts)
  (loop for evt in evts
     with tab = (slot-value *dispatcher* 'socktab)
     with failed = '()
     do
       #+debug
       (format t "dispatching event ~a~%" evt)
       (multiple-value-bind (ctx existsp) (gethash (getf evt :fd) tab)
	 (if (member :+EPOLLRDHUP+ (getf evt :flags))
	     (push (cons ctx evt) failed)
	     (when existsp
	       (with-slots (rx-handler tx-handler) ctx
		 (when (and rx-handler (eq (getf evt :filter) :in))
		   (funcall rx-handler ctx evt))
		 (when (and tx-handler (eq (getf evt :filter) :out))
		   (funcall tx-handler ctx evt))))))
     finally
       (mapcar (lambda (x)
		 (let ((ctx (first x))
		       (evt (second x)))
		   (with-slots (disconnect-handler) ctx
		     (when disconnect-handler
		       (funcall disconnect-handler ctx evt)))))
	       failed)))
