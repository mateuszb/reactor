(in-package :reactor.dispatch)

(defvar *dispatcher*)

(defstruct dispatcher
  (socktab (make-hash-table :synchronized t))
  (reactor (make-reactor)))

(defstruct context
  socket
  rx-handler
  rx-evts
  tx-handler
  tx-evts
  disconnect-handler
  data)

(defmacro with-dispatcher ((dispatcher) &body body)
  `(let ((*dispatcher* ,dispatcher))
     (let ((reactor:*reactor* (dispatcher-reactor *dispatcher*)))
       ,@body)))

(defun on-disconnect (socket handler)
  (let ((sd (socket-fd socket)))
    (with-slots ((tab socktab) (r reactor)) *dispatcher*
      (multiple-value-bind (ctx existsp) (gethash sd tab)
	(if existsp
	    (progn
	      (update r ctx :in (read-event-flags) handler)
	      (update r ctx :out (write-event-flags) handler))
	    (let ((ctx (make-context :socket socket
				     :disconnect-handler handler
				     :rx-evts (read-event-flags))))
	      (add r ctx :in (read-event-flags) handler)
	      (add r ctx :out (write-event-flags) handler)
	      (setf (gethash sd tab) ctx)))))))

(defun close-all-descriptors ()
  (loop for s being the hash-key in (dispatcher-socktab *dispatcher*)
     using (hash-value ctx)
     do
       (ignore-errors
	 (rem-socket s)
	 (disconnect s))))

(defun close-dispatcher (dispatcher)
  (with-dispatcher (dispatcher)
    (close-all-descriptors)
    (close-reactor
     (dispatcher-reactor dispatcher))))

(defun socket-context (socket)
  (gethash socket (dispatcher-socktab *dispatcher*)))

(defun (setf socket-context) (context socket)
  (setf (gethash socket (dispatcher-socktab *dispatcher*)) context))

(defun update (reactor ctx filter flags handler &optional newdata)
  (with-slots (rx-handler rx-evts tx-handler tx-evts data socket) ctx
    (when newdata (setf data newdata))
    (case filter
      (:in
       (setf rx-handler handler rx-flags flags)
       (reactor-modify reactor socket :in flags))
      (:out
       (setf tx-handler handler tx-flags flags)
       (reactor-modify reactor socket :out flags)))))

(defun add (reactor ctx filter flags handler &optional newdata)
  (with-slots (rx-handler rx-evts tx-handler tx-evts data socket) ctx
    (when newdata (setf data newdata))
    (case filter
      (:in
       (setf rx-handler handler rx-evts flags)
       (reactor-add reactor socket :in flags))
      (:out
       (setf tx-handler handler tx-evts flags)
       (reactor-add reactor socket :out flags)))))

(defun on-read (socket rxfun &optional context-data)
  (let ((sd (socket-fd socket)))
    (with-slots ((tab socktab) (r reactor)) *dispatcher*
      (multiple-value-bind (ctx existsp) (gethash sd tab)
	(if existsp
	    (update r ctx :in (read-event-flags) rxfun context-data)
	    (let ((ctx (make-context :socket socket
				     :data context-data
				     :rx-handler rxfun
				     :rx-evts (read-event-flags))))
	      (add r ctx :in (read-event-flags) rxfun context-data)
	      (setf (gethash sd tab) ctx)))))))

(defun on-write (socket txfun &optional context-data)
  (let ((sd (socket-fd socket)))
    (with-slots ((tab socktab) (r reactor)) *dispatcher*
      (multiple-value-bind (ctx existsp) (gethash sd tab)
	(if existsp
	    (update r ctx :out (write-event-flags) txfun context-data)
	    (let ((ctx (make-context :socket socket
				     :data context-data
				     :tx-handler txfun
				     :tx-evts (write-event-flags))))
	      (add r ctx :out (write-event-flags) txfun context-data)
	      (setf (gethash sd tab) ctx)))))))

(defun del-write (socket)
  (let ((sd (socket-fd socket)))
    (with-slots ((tab socktab) (r reactor)) *dispatcher*
      (multiple-value-bind (ctx existsp) (gethash sd tab)
	(when existsp
	  (with-slots (tx-evts rx-evts) ctx
	    (setf tx-evts (remove :out tx-evts))
	    (reactor-remove r socket :out)))))))

(defun del-read (socket)
  (let ((sd (socket-fd socket)))
    (with-slots ((tab socktab) (r reactor)) *dispatcher*
      (multiple-value-bind (ctx existsp) (gethash sd tab)
	(when existsp
	  (with-slots (rx-evts) ctx
	    (setf rx-evts (remove :in rx-evts))
	    (reactor-remove r socket :in)))))))

(defun rem-socket (socket)
  (let ((sd (socket-fd socket)))
    (with-slots ((tab socktab) (r reactor)) *dispatcher*
      (multiple-value-bind (ctx existsp) (gethash sd tab)
	(declare (ignore ctx))
	(when existsp
	  (reactor-remove-socket socket)
	  (remhash sd tab))))))


(defun dispatch-events (evts)
  (labels
      ((errorp (x) (member x '(:error)))
       (eofp (x) (member x '(:eof)))
       (is-error-p (flags) (some #'errorp flags))
       (is-eof-p (flags) (some #'eofp flags)))
    (loop for evt in evts
       with tab = (slot-value *dispatcher* 'socktab)
       with failed = '()
       do
	 (format t "dispatching event ~s~%" evt)
	 (multiple-value-bind (ctx existsp) (gethash (getf evt :fd) tab)
	   (when (or (is-eof-p (getf evt :flags))
		     (is-error-p (getf evt :flags)))
	     (pushnew (list (getf evt :fd) ctx evt) failed :test #'= :key #'car))
	   (when existsp
	     (with-slots (rx-handler tx-handler) ctx
	       (when (and rx-handler (eq (getf evt :filter) :in))
		 (funcall rx-handler ctx evt))
	       (when (and tx-handler (eq (getf evt :filter) :out))
		 (funcall tx-handler ctx evt)))))
       finally
	 (format t "failed: ~a~%" failed)
	 (mapcar (lambda (x)
		   (let ((ctx (second x))
			 (evt (third x)))
		     (with-slots (disconnect-handler) ctx
		       (when disconnect-handler
			 (funcall disconnect-handler ctx evt)))))
		 failed))))
