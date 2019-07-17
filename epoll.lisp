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
  (with-foreign-object (evt '(:struct epoll-event))
    (with-foreign-slots ((events data) evt (:struct epoll-event))
      (let ((evt-mask (reduce #'logior evts)))
	(setf events evt-mask data fd)
	(epoll-ctl epollfd +EPOLL-CTL-ADD+ fd evt)))))

(defun epoll-mod (epollfd fd evts)
  (with-foreign-object (evt '(:struct epoll-event))
    (with-foreign-slots ((events data) evt (:struct epoll-event))
      (let ((evt-mask (reduce #'logior evts)))
	(setf events evt-mask data fd)
	(epoll-ctl epollfd +EPOLL-CTL-MOD+ fd evt)))))

(defun epoll-del (epollfd fd)
  (epoll-ctl epollfd +EPOLL-CTL-DEL+ fd (null-pointer)))

(defun bits->flags (bitmask)
  (loop for flag in (list +EPOLLRDHUP+ +EPOLLPRI+ +EPOLLERR+ +EPOLLHUP+)
     for sym in '(:+EPOLLRDHUP+ :+EPOLLPRI+ :+EPOLLERR+ :+EPOLLHUP)
     when (= flag (logand bitmask flag)) collect sym))

(defun epoll-events (epollfd &optional (max-evts 128))
  (with-foreign-object (evtlist '(:struct epoll-event) max-evts)
    (let ((nevts (epoll-wait epollfd evtlist max-evts 0)))
      (loop for i from 0 below nevts
	 for evt = (mem-aptr evtlist '(:struct epoll-event) i)
	 then (mem-aptr evtlist '(:struct epoll-event) i)
	 collect
	   (car
	    (with-foreign-slots ((events data) evt (:struct epoll-event))
	      (loop for flag in (list (cons +EPOLLIN+ :in) (cons +EPOLLOUT+ :out))
		 when (= (car flag) (logand events (car flag)))
		 collect
		   (list :fd data
			 :filter (cdr flag)
			 :bytes-in (rxbytes-available data)
			 :flags (bits->flags events)))))))))
