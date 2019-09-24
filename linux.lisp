(in-package :reactor.linux)

(defcfun (epoll-create1 "epoll_create1") :int
  (flags :int))

(defcfun (epoll-ctl "epoll_ctl") :int
  (epollfd :int)
  (op :int)
  (fd :int)
  (event (:pointer (:struct epoll-event))))

(defcfun (epoll-wait "epoll_wait") :int
  (epollfd :int)
  (event (:pointer (:struct epoll-event)))
  (maxevts :int)
  (timeout :int))

(defun make-epoll ()
  (epoll-create1 0))

(defun epoll-add (epollfd fd evt-filters evt-flags)
  (with-foreign-object (evt '(:struct epoll-event))
    (with-foreign-slots ((events data) evt (:struct epoll-event))
      (let ((evt-mask (union evt-filters evt-flags)))
	(setf events evt-mask data fd)
	(format t "EPOLL-ADD handle=~a, mask=~a, flags=~a~%" fd evt-mask evt-flags)
	(epoll-ctl epollfd EPOLL-CTL-ADD fd evt)))))

(defun epoll-mod (epollfd fd evt-filters evt-flags)
  (with-foreign-object (evt '(:struct epoll-event))
    (with-foreign-slots ((events data) evt (:struct epoll-event))
      (let ((evt-mask (union evt-filters evt-flags))
	    (err 0))
	(setf events evt-mask data fd)
	(format t "EPOLL-MOD handle=~a, mask=~a, flags=~a~%" fd evt-mask evt-flags)
	(setf err (epoll-ctl epollfd EPOLL-CTL-MOD fd evt))
	(when (= err -1)
	  (error "epoll mod write error"))
	err))))

(defun epoll-del (epollfd fd)
  (format t "EPOLL-DEL handle=~a~%" fd)
  (epoll-ctl epollfd EPOLL-CTL-DEL fd (null-pointer)))

(defun epoll-events (epollfd &optional (max-evts 128) (timeout -1))
  (with-foreign-object (evtlist '(:struct epoll-event) max-evts)
    (let ((nevts (epoll-wait epollfd evtlist max-evts timeout))
	  (result '()))
      (loop for i from 0 below nevts
	 for evt = (mem-aptr evtlist '(:struct epoll-event) i)
	 then (mem-aptr evtlist '(:struct epoll-event) i)
	 do
	   (with-foreign-slots ((events data) evt (:struct epoll-event))
	     (format t "EVENT: flags=~a~%" events)
	     (labels ((remove-inout-flags (flags)
			(remove :EPOLLIN (remove :EPOLLOUT flags)))
		      (inout-flags (flags)
			(intersection '(:EPOLLIN :EPOLLOUT) flags)))
	       (loop
		  for direction in (inout-flags events)
		  collect
		    (list :handle data
			  :filter direction
			  :flags (remove-inout-flags events))
		  into evtlist
		  finally
		    (loop for el in evtlist
		       do (push el result))))))
      result)))

(defun common-event-flags ()
  (list :EPOLLET :EPOLLPRI :EPOLLERR :EPOLLRDHUP :EPOLLHUP))

(defun read-event-flags ()
  (common-event-flags))

(defun write-event-flags ()
  (common-event-flags))

(defun rw-event-flags ()
  (union (read-event-flags) (write-event-flags)))

(defun filter->enum (filter)
  (case filter
    (:in :EPOLLIN)
    (:out :EPOLLOUT)))

(defun filters->enums (filters)
  (mapcar #'filter->enum filters))

(defun flatten (ls)
  (labels ((mklist (x) (if (listp x) x (list x))))
    (mapcan #'(lambda (x) (if (atom x) (mklist x) (flatten x))) ls)))

(defun flags->enums (flags)
  (flatten
   (mapcar #'flag->enum flags)))

(defun flag->enum (flag)
  (case flag
    (:edge '(:EPOLLET))
    (:eof '(:EPOLLHUP :EPOLLRDHUP))
    (:error '(:EPOLLERR))))

(defun platform->universal-filter (filter)
  (case filter
    (:EPOLLIN :in)
    (:EPOLLOUT :out)))

(defun platform->universal-flag (flag)
  (case flag
    (:EPOLLERR :error)
    (:EPOLLHUP :eof)
    (:EPOLLRDHUP :eof)
    (t nil)))

(defun platform->universal-event (event)
  (loop for elem on event by #'cddr
     do
       (let ((prop (car elem))
	     (val (cadr elem)))
	 (case prop
	   (:filter
	    (setf (cadr elem) (platform->universal-filter val)))
	   (:flags
	    (let ((translated (loop for flag in val
				 with flg
				 do (setf flg (platform->universal-flag flag))
				 when flg collect flg)))
	      (setf (cadr elem) translated))))))
  event)
