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

(defcfun "ioctl" :int
  (fd :int)
  (request :uint32)
  (result (:pointer :int32)))

(defun rxbytes-available (sd)
  (with-foreign-object (rxbytes :int32)
    (let ((err (ioctl sd +FIONREAD+ rxbytes)))
      (if (zerop err)
	  (mem-ref rxbytes :int32)
	  err))))

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
  (loop for flag in (list +EPOLLRDHUP+ +EPOLLPRI+ +EPOLLERR+ +EPOLLHUP+)
     for sym in '(:+EPOLLRDHUP+ :+EPOLLPRI+ :+EPOLLERR+ :+EPOLLHUP+)
     when (= flag (logand bitmask flag)) collect sym))

(defun epoll-events (epollfd &optional (max-evts 128) (timeout -1))
  (with-foreign-object (evtlist '(:struct epoll-event) max-evts)
    (let ((nevts (epoll-wait epollfd evtlist max-evts timeout))
	  (result '()))
      (format t "epoll returned ~a~%" nevts)
      (loop for i from 0 below nevts
	 for evt = (mem-aptr evtlist '(:struct epoll-event) i)
	 then (mem-aptr evtlist '(:struct epoll-event) i)
	 do
	   (with-foreign-slots ((events data) evt (:struct epoll-event))
	     (format t "event ~a/~a flags = ~a~%" i nevts events)
	     (loop for direction in (list +EPOLLIN+ +EPOLLOUT+)
		for filter in '(:in :out)
		do
		  (format t "checking direction ~x vs ~x~%" direction events)
		when (= direction (logand events direction))
		do
		  (format t "result = ~x~%" (logand events direction))
		and
		collect
		  (list :fd data
			:filter filter
			:bytes-in (rxbytes-available data)
			:flags (bits->flags events))
		into evtlist
		finally
		  (format t "tmp result=~a~%" evtlist)
		  (loop for el in evtlist
		     do (push el result)))))
      result)))
