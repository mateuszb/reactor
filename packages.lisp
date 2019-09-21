(defpackage reactor
  (:use :cl))

#+os-macosx
(defpackage reactor.macos
  (:use :cl :cffi)
  (:export :make-kqueue
	   :kqueue-add
	   :kqueue-del
	   :kqueue-events))

#+linux
(defpackage reactor.linux
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
	:reactor.macos
	#+freebsd
	:reactor.freebsd
	#+linux
	:reactor.epoll)
  (:export :reactor
	   :*reactor*
	   :context
	   :with-reactor
	   :make-reactor
	   :reactor-handle
	   :wait-for-events
	   :close-reactor))

(defpackage reactor.dispatch
  (:use :cl
	:reactor
	:socket
	#+linux
	:reactor.linux
	#+os-macosx
	:reactor.macos
	#+freebsd
	:reactor.freebsd)
  (:export :dispatcher
	   :context-data
	   :context
	   :make-context
	   :make-dispatcher
	   :with-dispatcher
	   :on-write
	   :on-read
	   :on-disconnect
	   :del-write
	   :del-read
	   :rem-socket
	   :dispatch-events
	   :context-socket
	   :close-dispatcher
	   :dispatcher-reactor
	   :socket-context))
