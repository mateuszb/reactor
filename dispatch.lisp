(in-package :reactor)

(defvar *dispatcher*)

(defclass context ()
  ((handle :initarg :handle :accessor context-handle)
   (filters :initarg :filters :accessor filters)
   (flags :initarg :flags :accessor flags)
   (handlers :initform nil :initarg :handlers :accessor handlers)
   (data :initform nil :accessor context-data :initarg :data)))

(defmethod print-object ((obj context) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "handle=~a, handlers=~a, data=~a"
	    (context-handle obj)
	    (handlers obj)
	    (context-data obj))))

(defun make-context (handle filters flags &optional handlers data)
  (make-instance
   'context :handle handle
   :filters filters
   :flags flags
   :handlers handlers
   :data data))

(defstruct dispatcher
  (handle-tab (make-hash-table :synchronized t :test 'equal))
  (reactor (make-reactor)))

(defmacro with-dispatcher ((dispatcher) &body body)
  `(let ((*dispatcher* ,dispatcher))
     (let ((*reactor* (dispatcher-reactor *dispatcher*)))
       ,@body)))

(defun close-all-descriptors ()
  (loop for s being the hash-key in (dispatcher-handle-tab *dispatcher*)
     using (hash-value ctx)
     do
       (ignore-errors
	 (rem-handle s)
	 (disconnect s))))

(defun close-dispatcher (dispatcher)
  (with-dispatcher (dispatcher)
    (close-all-descriptors)
    (close-reactor
     (dispatcher-reactor dispatcher))))

(defun get-context (handle)
  (gethash
   handle
   (dispatcher-handle-tab *dispatcher*)))

(defun (setf get-context) (ctx handle)
  (setf (gethash handle (dispatcher-handle-tab *dispatcher*)) ctx))

(defun add (reactor handle filters flags handlers &optional newdata)
  ;; the logic is as follows:
  ;; - check if the socket is in the reactor set or not
  ;;   if it is in the set, fetch its context.
  ;;   otherwise, make a new context and fill it out
  (assert handle)
  (multiple-value-bind (ctx foundp) (get-context (handle-key handle))
    (unless foundp
      (assert filters)
      (setf ctx
	    (make-context handle
			  filters
			  flags
			  (mapcar #'cons filters handlers)
			  newdata))
      (setf (get-context (handle-key handle)) ctx)
      (reactor-add reactor (handle-key handle) filters flags))

    ;; check if the requested filters and flags are already associated
    ;; with this context
    (let* ((filters-present (filters ctx))
	   (flags-present (flags ctx))
	   (new-filters (set-difference filters filters-present))
	   (new-flags (set-difference flags flags-present)))

      (cond
	;; if no new filters and flags are present, we only have to
	;; check if the handler has changed
	((and (null new-filters) (null new-flags))
	 (let ((pairs (pairlis filters handlers)))
	   (loop for existing-handler on (handlers ctx)
	      when (assoc (caar existing-handler) pairs)
	      do
		(rplaca existing-handler (assoc (caar existing-handler) pairs)))))

	;; if there are only new flags present, we update the flags
	((and (null new-filters) new-flags)
	 ;; only update flags
	 (setf (flags ctx) (union flags-present new-flags))
	 (reactor-modify reactor (handle-key handle) (filters ctx) (flags ctx)))

	;; filter is not present but the handle is in the set, we need
	;; to add a new filter
	((and foundp (or new-filters new-flags))
	 (let* ((pairs (mapcar #'cons filters handlers)))
	   (dolist (h pairs)
	     (unless (member (car h) (filters ctx))
	       (push h (handlers ctx)))))
	 (setf (filters ctx) (union filters-present new-filters))
	 (setf (flags ctx) (union flags-present new-flags))

	 (reactor-modify reactor (handle-key handle) (filters ctx) (flags ctx)))

	((and (not foundp)) ;; TODO: is this correct?
	 (reactor-add reactor (handle-key handle) (filters ctx) (flags ctx)))))))

(defun del (reactor handle del-filters del-flags)
  (multiple-value-bind (ctx foundp) (get-context (handle-key handle))
    (when foundp
      (with-slots (handlers flags filters) ctx
	(let* ((matching-filters (intersection del-filters filters))
	       (matching-flags (intersection del-flags flags)))
	  (when matching-filters
	    (let ((new-filters (set-difference filters matching-filters))
		  (new-flags (set-difference flags matching-flags)))
	      (cond
		;; new filter list is empty. remove the handle from
		;; the reactor set
		((null new-filters)
		 (reactor-remove reactor (handle-key handle) filters)
		 (remhash (handle-key handle) (dispatcher-handle-tab *dispatcher*)))

		;; new filter list is not empty. update the handle in
		;; the reactor set
		(new-filters
		 ;; we're removing the descriptor from the
		 ;; matching-filters set
		 (reactor-modify reactor (handle-key handle) new-filters new-flags)
		 (setf flags new-flags)
		 (setf filters new-filters)

		 (dolist (f matching-filters)
		   (setf handlers (delete f handlers :key #'car))))))))))))

(defun on-disconnect (handle handler)
  (let ((reactor (dispatcher-reactor *dispatcher*)))
    (add reactor handle nil '(:eof :error) handler)))

(defun on-read (handle rxfun &optional context-data)
  (let ((reactor (dispatcher-reactor *dispatcher*)))
    (add reactor handle '(:in) '(:edge) (list rxfun) context-data)))

(defun on-write (handle txfun &optional context-data)
  (let ((reactor (dispatcher-reactor *dispatcher*)))
    (add reactor handle '(:out) '(:edge) (list txfun) context-data)))

(defun del-write (handle)
  (let ((reactor (dispatcher-reactor *dispatcher*)))
    (del reactor handle '(:out) nil)))

(defun del-read (handle)
  (let ((reactor (dispatcher-reactor *dispatcher*)))
    (del reactor handle '(:in) nil)))

(defun rem-handle (handle)
  (let ((reactor (dispatcher-reactor *dispatcher*)))
    (del reactor handle '(:in :out) nil)))

(defun dispatch-events (evts)
  (labels
      ((errorp (x) (member x '(:error)))
       (eofp (x) (member x '(:eof)))
       (is-error-p (flags) (some #'errorp flags))
       (is-eof-p (flags) (some #'eofp flags)))
    (loop for evt in evts
       with failed = '()
       do
	 (multiple-value-bind (ctx existsp) (get-context (getf evt :handle))
	   (let ((flags (getf evt :flags)))
	     (when (or (is-eof-p flags) (is-error-p flags))
	       (pushnew (list (getf evt :handle) ctx evt) failed :test #'= :key #'car)))
	   (when existsp
	     (let ((matching-filters (intersection
				      (list (getf evt :filter))
				      (filters ctx))))
	       (dolist (mf matching-filters)
		 (funcall (cdr (assoc mf (handlers ctx))) ctx evt)))))
       finally
	 (dolist (elm failed)
	   (let ((ctx (second elm))
		 (evt (third elm)))
	     (unless ctx
	       (with-slots (handlers) ctx
		 (when (cdr (assoc :eof handlers))
		   (funcall (cdr (assoc :eof handlers)) ctx evt)))))))))
