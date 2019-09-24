(in-package :reactor.freebsd)

(defcfun "kqueue" :int
  "Create kqueue descriptor")

(defcfun "kevent" :int
  (kq :int)
  (changelist (:pointer (:struct kevent)))
  (nchanges :int)
  (eventlist (:pointer (:struct kevent)))
  (nevents :int)
  (timespec (:pointer (:struct timespec))))

(defun make-kqueue ()
  (kqueue))

(defun kqueue-add (kfd fd evt-filters evt-flags)
  (declare (optimize (speed 0) (debug 3) (safety 3)))
  (setf evt-flags (union evt-flags '(:EV-ADD :EV-ENABLE)))
  (with-foreign-object (changelist '(:struct kevent) (length evt-filters))
    (loop
       for i from 0 below (length evt-filters)
       for f in evt-filters
       do
	 (let ((kevt (mem-aptr changelist '(:struct kevent) i)))
	   (with-foreign-slots ((ident filter flags fflags data udata ext) kevt (:struct kevent))
	     (setf ident fd
		   filter f
		   flags evt-flags
		   fflags 0
		   data 0
		   udata (null-pointer)
		   (mem-aref ext :uint64) 0
		   (mem-aref ext :uint64 1) 0
		   (mem-aref ext :uint64 2) 0
		   (mem-aref ext :uint64 3) 0))))

      (kevent kfd changelist (length evt-filters) (null-pointer) 0 (null-pointer))))

(defun kqueue-del (kfd fd del-filters)
  (with-foreign-object (changelist '(:struct kevent) (length del-filters))
    (loop
       for i from 0 below (length del-filters)
       for f in del-filters
       do
	 (let ((kevt (mem-aptr changelist '(:struct kevent) i)))
	   (with-foreign-slots ((ident filter flags) kevt (:struct kevent))
	     (setf ident fd
		   filter f
		   flags :EV-DELETE))))

      (kevent kfd changelist (length del-filters) (null-pointer) 0 (null-pointer))))

(defun filter->enum (filter)
  (ecase filter
    (:in :evfilt-read)
    (:out :evfilt-write)))

(defun filters->enums (filters)
  (mapcar #'filter->enum filters))

(defun flag->enum (flag)
  (ecase flag
    (:edge '(:EV-CLEAR))
    (:eof '(:EV-EOF))
    (:error '(:EV-ERROR))))

(defun flatten (ls)
  (labels ((mklist (x) (if (listp x) x (list x))))
    (mapcan #'(lambda (x) (if (atom x) (mklist x) (flatten x))) ls)))

(defun flags->enums (flags)
  (flatten
   (mapcar #'flag->enum flags)))

(defun kqueue-events (kfd &optional (max-evts 128))
  (with-foreign-object (evtlist '(:struct kevent) max-evts)
    (let ((nevts (kevent kfd (null-pointer) 0 evtlist max-evts (null-pointer))))
      (loop for i from 0 below nevts
	 for evt = (mem-aptr evtlist '(:struct kevent) i)
	 then (mem-aptr evtlist '(:struct kevent) i)
	 collect
	   (with-foreign-slots
	       ((ident filter flags fflags data udata ext) evt (:struct kevent))
	     (format t "EVENT: ident=~s, filter=~s, flags=~s~%"
		     ident filter flags)
	     (concatenate
	      'list
	      (list :handle ident :filter filter :udata udata
		    :flags flags :fflags fflags)
	      (case filter
		(:EVFILT-READ  (list :bytes-in data))
		(:EVFILT-WRITE (list :bytes-out data)))))))))

(defun platform->universal-filter (filter)
  (case filter
    (:EVFILT-READ :in)
    (:EVFILT-WRITE :out)))

(defun platform->universal-flag (flag)
  (case flag
    (:EV-ERROR :error)
    (:EV-EOF :eof)
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
