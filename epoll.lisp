(in-package :reactor.epoll)

(defcfun "kqueue" :int
  "Create kqueue descriptor")

(defcfun "kevent64" :int
  (kq :int)
  (changelist (:pointer kevent64_s))
  (nchanges :int)
  (eventlist (:pointer kevent64_s))
  (nevents :int)
  (flags :uint32)
  (timespec (:pointer timespec)))

(defun make-kqueue ()
  (kqueue))

(defun kqueue-add (kfd fd evt-filter &optional (evt-udata nil))
  (with-foreign-object (changelist '(:struct kevent64_s))
    (with-foreign-slots
	((ident filter flags fflags data udata ext) changelist (:struct kevent64_s))

      (setf ident fd
	    filter evt-filter
	    flags +EV-ADD+
	    fflags 0
	    data 0
	    udata (if evt-udata evt-udata 0)
	    (mem-aref ext :uint64) 0
	    (mem-aref ext :uint64 1) 0)

      (kevent64 kfd changelist 1 (null-pointer) 0 KEVENT-FLAG-IMMEDIATE (null-pointer)))))

(defun kqueue-del (kfd fd evt-filter)
  (with-foreign-object (changelist '(:struct kevent64_s))
    (with-foreign-slots ((ident filter flags) changelist (:struct kevent64_s))
      (setf ident fd filter evt-filter flags +EV-DELETE+)
      (kevent64 kfd changelist 1 (null-pointer) 0 KEVENT-FLAG-IMMEDIATE (null-pointer)))))

(defun kqueue-events (kfd &optional (max-evts 128))
  (with-foreign-object (evtlist '(:struct kevent64_s) max-evts)
    (let ((nevts (kevent64 kfd (null-pointer) 0
			   evtlist max-evts
			   KEVENT-FLAG-IMMEDIATE (null-pointer))))
      (loop for i from 0 below nevts
	 for evt = (mem-aptr evtlist '(:struct kevent64_s) i)
	 then (mem-aptr evtlist '(:struct kevent64_s) i)
	 collect
	   (with-foreign-slots ((ident filter flags fflags data udata ext) evt (:struct kevent64_s))
	     (concatenate 'list
			  (list :fd ident :filter filter :udata udata)
			  (case filter
			    (:+EVFILT-READ+  (list :rx-avail data))
			    (:+EVFILT-WRITE+ (list :tx-buf-left data)))))))))
