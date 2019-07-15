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
	   :+EPOLLIN+
	   :+EPOLLOUT+
	   :+EPOLLET+))

(defpackage reactor.dispatch
  (:use :cl
	#+os-macosx
	:reactor.kqueue
	#+linux
	:reactor.epoll)
  (:export :dispatcher
	   :context
	   :with-dispatcher
	   :make-dispatcher
	   :wait-for-events
	   :on-read))
