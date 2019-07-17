(defsystem "reactor"
  :version "0.1.0"
  :author "Mateusz Berezecki"
  :license "BSD"
  :depends-on ("cl+ssl"
	       "base64"
	       "cffi"
	       "cffi-grovel")
  :defsystem-depends-on ("cffi-grovel")
  :serial t
  :components ((:file "packages")
	       (:cffi-grovel-file "grovel" :depends-on ("packages"))
	       #+os-macosx (:file "kqueue" :depends-on ("grovel"))
	       #+linux (:file "epoll" :depends-on ("grovel"))
	       (:file "reactor" :depends-on (#+os-macosx "kqueue" #+linux "epoll"))
	       (:file "dispatch" :depends-on ("reactor")))
  :in-order-to ((test-op (test-op "reactor/test")))
  :description "epoll/kqueue reactor library")

(defsystem "reactor/test"
  :depends-on ("prove")
  :defsystem-depends-on (:prove-asdf)
  :serial t
  :components ((:module "tests" :components ((:test-file "test"))))
  :perform (test-op :after (o c)
		    (funcall (intern #.(string :run) :prove) c)))
