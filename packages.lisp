#+os-macosx
(defpackage reactor.macos
  (:use :cl :cffi)
  (:export :make-kqueue
	   :kqueue-add
	   :kqueue-del
	   :kqueue-events
	   :filter->enum
	   :platform->universal-filter
	   :platform->universal-flag
	   :platform->universal-event	   
	   ))

#+freebsd
(defpackage reactor.freebsd
  (:use :cl :cffi)
  (:export :make-kqueue
	   :kqueue-add
	   :kqueue-del
	   :kqueue-events
	   :read-event-flags
	   :write-event-flags
	   :filter->enum
	   :platform->universal-filter
	   :platform->universal-flag
	   :platform->universal-event	   
	   ))

#+linux
(defpackage reactor.linux
  (:use :cl :cffi)
  (:export :make-epoll
	   :epoll-events
	   :epoll-del
	   :epoll-add
	   :epoll-mod
	   :read-event-flags
	   :write-event-flags
	   :EPOLLIN
	   :EPOLLPRI
	   :EPOLLRDHUP
	   :EPOLLHUP
	   :EPOLLERR
	   :EPOLLOUT
	   :EPOLLET
	   :filter->enum
	   :platform->universal-filter
	   :platform->universal-flag
	   :platform->universal-event
	   ))

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
	   :reactor-remove
	   :reactor-add
	   :reactor-modify
	   :reactor-remove-socket
	   :close-reactor
	   :filter->enum)
  (:import-from :socket
		:socket-fd))

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
