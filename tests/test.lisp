(in-package :cl-user)
(defpackage reactor-test
  (:use :cl :prove :reactor :socket :alien-ring))

(in-package :reactor-test)

(defparameter +port+ 6682)

(defvar naccepts)

(defmethod handle-key ((s socket))
  (socket-fd s))

(defun accept-handler (ctx evt)
  (handler-case
      (loop
	 with srv = (context-handle ctx)
	 do
	   (let ((newsock (accept srv)))
	     (format t "Accepted new connection with fd ~a~%" newsock)
	     (set-non-blocking newsock)
	     (on-read
	      newsock
	      (lambda (ctx evt)
		(declare (ignore evt))
		(format t "on read~%")
		(cffi:with-foreign-string (str "1234")
		  (receive (context-handle ctx) (list str) (list 4))
		  (format t "done receiving: '~a'~%"
			  (cffi:foreign-string-to-lisp str)))
		(incf naccepts)))))


    (operation-would-block ()
      (format t "OPERATION WOULD BLOCK~%")
      (return-from accept-handler))

    (operation-in-progress ()
      (format t "OPERATION IN PROGRESS~%")
      (return-from accept-handler))

    (operation-interrupted (err)
      (format t "OPERATION INTERRUPTED~%")
      (return-from accept-handler))

    (socket-error (err)
      (format t "SOCKET ERROR~%")
      (return-from accept-handler))))

(defun client-write-handler (ctx evt)
  (let ((s (make-binary-ring-stream 4)))
    (write-string "test" s)
    (send (context-handle ctx) (ring-buffer-ptr (stream-buffer s)) 4)
    (del-write (context-handle ctx))))

(defun test-nonblocking-connect ()
  (setf naccepts 0)
  (let ((srv (make-tcp-listen-socket +port+))
	(disp (make-dispatcher)))
    (set-non-blocking srv)
    (with-dispatcher (disp)
      (on-read srv #'accept-handler)

      (let ((clients (loop for i from 0 below 10 collect (make-tcp-socket))))
	(loop for c in clients
	   do
	     (set-non-blocking c)
	     (on-write c #'client-write-handler)
	     (handler-case
		 (connect c #(127 0 0 1) +port+)
	       (operation-in-progress ()
		 (format t "connecting...~%"))
	       (operation-interrupted ()
		 (format
		  t "operation interrupted. will continue in the background~%"))))

	(loop until (= naccepts 10)
	   do
	     (let ((evts (wait-for-events)))
	       (dispatch-events evts)))

	(loop for c in clients
	   do
	     (disconnect c))))

    (format t "DISCONNECTING SERVER SOCKET~%")
    (sb-posix:close (socket-fd srv))
    (is naccepts 10)))

(plan 1)
(test-nonblocking-connect)
(finalize)
