(in-package :reactor.epoll)

(defcfun (epoll-create1 "epoll_create1") :int
  (flags :int))

(defcfun (epoll-ctl "epoll_ctl") :int
  (epollfd :int)
  (op :int)
  (fd :int)
  (event (:pointer epoll-event)))

(defcfun (epoll-wait "epoll_wait") :int
  (epollfd :int)
  (event (:pointer epoll-event))
  (maxevts :int)
  (timeout :int))

(defun make-epoll ()
  (epoll-create1 0))

(defun epoll-add (epollfd fd evts)
  (format t "EPOLL-ADD: fd=~a, evts=~a~%" fd evts)
  (with-foreign-object (evt '(:struct epoll-event))
    (with-foreign-slots ((events data) evt (:struct epoll-event))
      (let ((evt-mask (reduce #'logior evts)))
	(setf events evt-mask data fd)
	(epoll-ctl epollfd +EPOLL-CTL-ADD+ fd evt)))))

(defun epoll-mod (epollfd fd evts)
  (format t "EPOLL-MOD: fd=~a, evts=~a~%" fd evts)
  (with-foreign-object (evt '(:struct epoll-event))
    (with-foreign-slots ((events data) evt (:struct epoll-event))
      (let ((evt-mask (reduce #'logior evts))
	    (err 0))
	(setf events evt-mask data fd)
	(setf err (epoll-ctl epollfd +EPOLL-CTL-MOD+ fd evt))
	(when (= err -1)
	  (error "epoll mod write error"))
	err))))

(defun epoll-del (epollfd fd)
  (format t "EPOLL-DEL: fd=~a~%" fd)
  (epoll-ctl epollfd +EPOLL-CTL-DEL+ fd (null-pointer)))

(defun bits->flags (bitmask)
  (loop for flag in (list +EPOLLRDHUP+ +EPOLLPRI+ +EPOLLERR+ +EPOLLHUP+ +EPOLLET+ +EPOLLIN+ +EPOLLOUT+)
     for sym in '(:+EPOLLRDHUP+ :+EPOLLPRI+ :+EPOLLERR+ :+EPOLLHUP+ :+EPOLLET+ :+EPOLLIN+ :+EPOLLOUT+)
     when (= flag (logand bitmask flag)) collect sym))

(defun epoll-events (epollfd &optional (max-evts 128) (timeout -1))
  (with-foreign-object (evtlist '(:struct epoll-event) max-evts)
    (let ((nevts (epoll-wait epollfd evtlist max-evts timeout))
	  (result '()))
      (loop for i from 0 below nevts
	 for evt = (mem-aptr evtlist '(:struct epoll-event) i)
	 then (mem-aptr evtlist '(:struct epoll-event) i)
	 do
	   (with-foreign-slots ((events data) evt (:struct epoll-event))
	     (loop for direction in (list +EPOLLIN+ +EPOLLOUT+)
		for filter in '(:in :out)
		when (= direction (logand events direction))
		collect
		  (list :fd data
			:filter filter
			:flags (bits->flags events))
		into evtlist
		finally
		  (loop for el in evtlist
		     do (push el result)))))
      result)))

(defun common-event-mask ()
  (list +EPOLLET+ +EPOLLPRI+ +EPOLLERR+ +EPOLLRDHUP+ +EPOLLHUP+))

(defun read-event-mask ()
  (cons +EPOLLIN+ (common-event-mask)))

(defun write-event-mask ()
  (cons +EPOLLOUT+ (common-event-mask)))

(defun rw-event-mask ()
  (union (read-event-mask) (write-event-mask)))
