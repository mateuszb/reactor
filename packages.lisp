(defpackage reactor
  (:use :cl))

#+os-macosx
(defpackage reactor.kqueue
  (:use :cl :cffi)
  (:export :make-kqueue))

#+linux
(defpackage reactor.epoll
  (:use :cl :cffi)
  (:export :make-epoll))
