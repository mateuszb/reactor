(in-package :cl-user)
(defpackage reactor-test
  (:use :cl :prove :reactor :reactor.dispatch :socket :alien-ring))

(in-package :reactor-test)

(defparameter +port+ 6678)

(defvar naccepts)

(defun accept-handler (ctx evt)
  (handler-case
      (loop
	 with srv = (context-socket ctx)
	 do
	   (let ((newsock (accept srv)))
	     (format t "Accepted new connection with fd ~a~%" newsock)
	     (set-non-blocking newsock)
	     (on-read newsock (lambda (ctx evt)
				(declare (ignore evt))
				(format t "on read~%")
				(cffi:with-foreign-string (str "1234")
				  (receive (context-socket ctx) (list str) (list 4))
				  (format t "done receiving: '~a'~%"
					  (cffi:foreign-string-to-lisp str)))
				(incf naccepts)))))
		       

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
    (send (context-socket ctx) (ring-buffer-ptr (stream-buffer s)) 4)
    (del-write (context-socket ctx))))

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
		 (format t "operation interrupted. will continue in the background~%"))))

	(loop until (= naccepts 10)
	   do
	     (let ((evts (wait-for-events)))	     
	       (dispatch-events evts)))

	(loop for c in clients
	   do
	     (disconnect c))))

    (format t "DISCONNECTING SERVER SOCKET~%")
    (sb-posix:close (socket-fd srv))))

(plan 8)
(test-nonblocking-connect)
(finalize)
