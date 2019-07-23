(in-package :cl-user)
(defpackage reactor-test
  (:use :cl :prove :reactor :reactor.dispatch :socket))

(in-package :reactor-test)

(defparameter +port+ 6622)

(defun test-nonblocking-connect ()
  (let ((srv (make-tcp-listen-socket +port+))
	(srvclis '())
	(naccepts 0)
	(disp (make-dispatcher)))
    (set-non-blocking srv)
    (with-dispatcher (disp)
      (on-read
       srv
       (lambda (ctx evt)
	 (declare (ignore ctx evt))
	 (loop
	    do
	      (handler-case
		  (let ((newcli (accept srv)))
		    (set-non-blocking newcli)
		    (push newcli srvclis)
		    (incf naccepts)
		    (format t "new connection ~a on listen socket ~a. accepted ~a connections."
			    newcli srv naccepts)
		    (on-read newcli
			     (lambda (ctx evt)
			       (format t "on read~%"))))
		(socket-error (err) (loop-finish))))))

      (let ((clients (loop for i from 0 below 10 collect (make-tcp-socket))))
	(loop for c in clients
	   do
	     (set-non-blocking c)
	     (on-write c (lambda (ctx evt) (send c "test")))
	     (handler-case
		 (connect c #(127 0 0 1) +port+)
	       (operation-in-progress ()
		 (format t "connecting...~%")))))

      (loop until (= naccepts 10)
	 do
	   (let ((evts (wait-for-events)))
	     (dispatch-events evts))))

    (disconnect srv)))

(plan 8)
(test-nonblocking-connect)
(finalize)
