(defsystem "reactor"
  :version "0.1.0"
  :author "Mateusz Berezecki"
  :license "BSD"
  :defsystem-depends-on ("cffi-grovel")
  :serial t
  :depends-on ("cl+ssl"
	       "base64"
	       "cffi"
	       "cffi-grovel")
  :components ((:file "packages")
	       (:cffi-grovel-file "grovel" :depends-on ("packages"))
	       #+os-macosx (:file "kqueue" :depends-on ("grovel"))
	       #+linux (:file "epoll" :depends-on ("grovel"))
	       (:file "reactor" :depends-on
		      (#+os-macosx "kqueue" #+linux "epoll")))
  :description "epoll/kqueue reactor library")
