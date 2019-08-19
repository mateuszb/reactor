(defpackage reactor
  (:use :cl))

#+os-macosx
(defpackage reactor.kqueue
  (:use :cl :cffi)
  (:export :make-kqueue
	   :kqueue-add
	   :kqueue-del
	   :kqueue-events))

#+linux
(defpackage reactor.epoll
  (:use :cl :cffi)
  (:export :make-epoll
	   :epoll-events
	   :epoll-del
	   :epoll-add
	   :epoll-mod
	   :common-event-mask
	   :read-event-mask
	   :write-event-mask
	   :rw-event-mask
	   :+EPOLLIN+
	   :+EPOLLPRI+
	   :+EPOLLRDHUP+
	   :+EPOLLHUP+
	   :+EPOLLERR+
	   :+EPOLLOUT+
	   :+EPOLLET+))

(defpackage reactor
  (:use :cl
	#+os-macosx
	:reactor.kqueue
	#+linux
	:reactor.epoll)
  (:export :reactor
	   :*reactor*
	   :context
	   :with-reactor
	   :make-reactor
	   :reactor-handle
	   :wait-for-events))

(defpackage reactor.dispatch
  (:use :cl
	:reactor
	:socket
	#+linux :reactor.epoll
	#+os-macosx :reactor.kqueue)
  (:export :dispatcher
	   :context-data
	   :context
	   :make-dispatcher
	   :with-dispatcher
	   :on-write
	   :on-read
	   :on-disconnect
	   :del-write
	   :del-read
	   :rem-socket
	   :dispatch-events
	   :context-socket))
