#+os-macosx
(progn
  (in-package :reactor.kqueue)
  
  (include "sys/types.h"
	   "sys/event.h"
	   "sys/time.h")

  (constantenum
   (flags :base-type :int16 :define-constants t)
   ((:+ev-add+ "EV_ADD"))
   ((:+ev-enable+ "EV_ENABLE"))
   ((:+ev-disable+ "EV_DISABLE"))
   ((:+ev-delete+ "EV_DELETE"))
   ((:+ev-receipt+ "EV_RECEIPT"))
   ((:+ev-oneshot+ "EV_ONESHOT"))
   ((:+ev-clear+ "EV_CLEAR"))
   ((:+ev-eof+ "EV_EOF"))
   ((:+ev-ooband+ "EV_OOBAND"))
   ((:+ev-error+ "EV_ERROR")))

  (constant (kevent-flag-immediate "KEVENT_FLAG_IMMEDIATE"))

  (constantenum
   (filter :base-type :int16 :define-constants t)
   ((:+evfilt-read+ "EVFILT_READ"))
   ((:+evfilt-write+ "EVFILT_WRITE"))
   ((:+evfilt-except+ "EVFILT_EXCEPT"))
   ((:+evfilt-vnode+ "EVFILT_VNODE"))
   ((:+evfilt-proc+ "EVFILT_PROC"))
   ((:+evfilt-signal+ "EVFILT_SIGNAL"))
   ((:+evfilt-timer+ "EVFILT_TIMER")))

  (cstruct kevent64_s "struct kevent64_s"
	   (ident "ident" :type :uint64)
	   (filter "filter" :type filter)
	   (flags "flags" :type :uint16)
	   (fflags "fflags" :type :uint32)
	   (data "data" :type :int64)
	   (udata "udata" :type :uint64)
	   (ext "ext" :type :uint64 :count 2))

  (ctype :time_t "time_t")
  (cstruct timespec "struct timespec"
	   (sec "tv_sec" :type :time_t)
	   (nsec "tv_nsec" :type :long)))

#+linux
(progn
  ;;; i'm not on linux, so this code is automatically disabled by IDE highlighter
  ;;; and it will also never be evaluated/compiled on macosx
)
