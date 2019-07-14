(defsystem "reactor"
  :version "0.1.0"
  :author "Mateusz Berezecki"
  :license "BSD"
  :defsystem-depends-on ("cffi-grovel")
  :serial t
  :depends-on ("cl+ssl"
	       "base64"
	       "cffi")
  :components ((:file "packages")
	       (:cffi-grovel-file "grovel" :depends-on ("packages"))
	       (:file "kqueue" :depends-on ("grovel"))
	       (:file "reactor" :depends-on ("kqueue")))
  :description "epoll/kqueue reactor library")
