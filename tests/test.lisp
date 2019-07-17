(in-package :cl-user)
(defpackage reactor-test
  (:use :cl :prove :reactor :reactor.dispatch)
  (:export :run! :all-tests :all-reactor-tests))

(in-package :reactor-test)

(plan 8)

(progn
  (let ((srv-socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))
	(cli-socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))

    (setf (sb-bsd-sockets:sockopt-reuse-address srv-socket) t)
    (setf (sb-bsd-sockets:non-blocking-mode srv-socket) t
	  (sb-bsd-sockets:non-blocking-mode cli-socket) t)
  
    (sb-bsd-sockets:socket-bind srv-socket #(0 0 0 0) 31337)
    (sb-bsd-sockets:socket-listen srv-socket 5)

    (with-dispatcher ((make-dispatcher))
      (on-write cli-socket nil)
      (handler-case (sb-bsd-sockets:socket-connect cli-socket #(127 0 0 1) 31337)
	(sb-bsd-sockets:operation-in-progress () t))
      (let ((evts (wait-for-events)))
	(is (getf (first evts) :filter) :out)))

    (sb-bsd-sockets:socket-close cli-socket)
    (sb-bsd-sockets:socket-close srv-socket)))

(let ((srv-socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))
      (cli-socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))

  (setf (sb-bsd-sockets:sockopt-reuse-address srv-socket) t)
  (setf (sb-bsd-sockets:non-blocking-mode srv-socket) t
	(sb-bsd-sockets:non-blocking-mode cli-socket) t)
  
  (sb-bsd-sockets:socket-bind srv-socket #(0 0 0 0) 31337)
  (sb-bsd-sockets:socket-listen srv-socket 5)

  (with-dispatcher ((make-dispatcher))
    (on-read srv-socket nil)
    (handler-case (sb-bsd-sockets:socket-connect cli-socket #(127 0 0 1) 31337)
      (sb-bsd-sockets:operation-in-progress () t))
    (let ((evts (wait-for-events)))
      (is (getf (first evts) :filter) :in)))

  (sb-bsd-sockets:socket-close cli-socket)
  (sb-bsd-sockets:socket-close srv-socket))

(let ((srv-socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))
      (cli-socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))
      (rxbytes 0)
      (rxbuf (make-array 32 :element-type '(unsigned-byte 8))))

  (setf (sb-bsd-sockets:sockopt-reuse-address srv-socket) t)
  (setf (sb-bsd-sockets:non-blocking-mode srv-socket) t
	(sb-bsd-sockets:non-blocking-mode cli-socket) t)
  
  (sb-bsd-sockets:socket-bind srv-socket #(0 0 0 0) 31337)
  (sb-bsd-sockets:socket-listen srv-socket 5)

  (with-dispatcher ((make-dispatcher))
    (let ((newsock nil))
      (labels ((acceptor ()
		 (setf newsock (sb-bsd-sockets:socket-accept srv-socket))
		 (setf (sb-bsd-sockets:non-blocking-mode newsock) t)
		 (on-read newsock
			  (lambda ()
			    (multiple-value-bind (buf len peer) (sb-bsd-sockets:socket-receive newsock rxbuf 32)
			      (declare (ignorable peer buf))
			      (incf rxbytes len))))
		 newsock))
	(on-read srv-socket #'acceptor))

      (on-write cli-socket (lambda () (sb-bsd-sockets:socket-send cli-socket "test" 4)))
      
      ;; connect the client
      (handler-case (sb-bsd-sockets:socket-connect cli-socket #(127 0 0 1) 31337)
	(sb-bsd-sockets:operation-in-progress () t))

      (let ((evts (wait-for-events)))
	(is (getf (second evts) :filter) :in)
	(dispatch-events evts)
	(dispatch-events (wait-for-events)))

      (is rxbytes 4)
      (is (map 'string #'code-char (subseq rxbuf 0 4)) "test")
      (sb-bsd-sockets:socket-send cli-socket "test2" 5)
      (dispatch-events (wait-for-events))
      (is rxbytes 9)
      (is (map 'string #'code-char (subseq rxbuf 0 5)) "test2")
      (is (not (null newsock)) t)
      (when newsock
	(sb-bsd-sockets:socket-close newsock))))

  (sb-bsd-sockets:socket-close cli-socket)
  (sb-bsd-sockets:socket-close srv-socket))

(finalize)
