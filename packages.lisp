(defpackage reactor
  (:use :cl))

(defpackage reactor.kqueue
  (:use :cl :cffi)
  (:export :make-kqueue))
