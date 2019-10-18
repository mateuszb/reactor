#+os-macosx
(progn
  (in-package :reactor.macos)

  (include "sys/types.h"
	   "sys/event.h"
	   "sys/time.h")

  (bitfield
   (flags :base-type :uint16)
   ((:ev-add "EV_ADD"))
   ((:ev-enable "EV_ENABLE"))
   ((:ev-disable "EV_DISABLE"))
   ((:ev-delete "EV_DELETE"))
   ((:ev-receipt "EV_RECEIPT"))
   ((:ev-oneshot "EV_ONESHOT"))
   ((:ev-clear "EV_CLEAR"))
   ((:ev-eof "EV_EOF"))
   ((:ev-ooband "EV_OOBAND"))
   ((:ev-error "EV_ERROR")))

  (constant (kevent-flag-immediate "KEVENT_FLAG_IMMEDIATE"))

  (constantenum
   (filter :base-type :int16 :define-constants t)
   ((:evfilt-read "EVFILT_READ"))
   ((:evfilt-write "EVFILT_WRITE"))
   ((:evfilt-except "EVFILT_EXCEPT"))
   ((:evfilt-vnode "EVFILT_VNODE"))
   ((:evfilt-proc "EVFILT_PROC"))
   ((:evfilt-signal "EVFILT_SIGNAL"))
   ((:evfilt-timer "EVFILT_TIMER")))

  (cstruct kevent64_s "struct kevent64_s"
	   (ident "ident" :type :uint64)
	   (filter "filter" :type filter)
	   (flags "flags" :type flags)
	   (fflags "fflags" :type :uint32)
	   (data "data" :type :int64)
	   (udata "udata" :type :pointer)
	   (ext "ext" :type :uint64 :count 2))

  (ctype :time_t "time_t")
  (cstruct timespec "struct timespec"
	   (sec "tv_sec" :type :time_t)
	   (nsec "tv_nsec" :type :long)))

#+linux
(progn
  (in-package :reactor.linux)

  (include "sys/epoll.h"
	   "sys/ioctl.h")

  (constant (+FIONREAD+ "FIONREAD"))
  (cvar ("errno" *errno*) :int)

  (constantenum
   (op :base-type :int :define-constants t)
   ((:epoll-ctl-add "EPOLL_CTL_ADD"))
   ((:epoll-ctl-del "EPOLL_CTL_DEL"))
   ((:epoll-ctl-mod "EPOLL_CTL_MOD")))

  (bitfield
   (epoll-events :base-type :uint32)
   ((:epollin "EPOLLIN"))
   ((:epollout "EPOLLOUT"))
   ((:epollrdhup "EPOLLRDHUP"))
   ((:epollpri "EPOLLPRI"))
   ((:epollerr "EPOLLERR"))
   ((:epollhup "EPOLLHUP"))
   ((:epollet "EPOLLET"))
   ((:epolloneshot "EPOLLONESHOT"))
   ((:epollwakeup "EPOLLWAKEUP"))
   ((:epollexclusive "EPOLLEXCLUSIVE")))

  (cstruct epoll-event "struct epoll_event"
	   (events "events" :type epoll-events)
	   (data "data" :type :uint64)))

#+freebsd
(progn
  (in-package :reactor.freebsd)
  (include "sys/time.h")
  (include "sys/event.h")

  (bitfield
   (kqueue-flags :base-type :uint16)
   ((:ev-add      "EV_ADD"))
   ((:ev-enable   "EV_ENABLE"))
   ((:ev-disable  "EV_DISABLE"))
   ((:ev-dispatch "EV_DISPATCH"))
   ((:ev-delete   "EV_DELETE"))
   ((:ev-receipt  "EV_RECEIPT"))
   ((:ev-oneshot  "EV_ONESHOT"))
   ((:ev-clear    "EV_CLEAR"))
   ((:ev-eof      "EV_EOF"))
   ((:ev-ooband   "EV_OOBAND"))
   ((:ev-error    "EV_ERROR")))

  (constantenum
   (kqueue-filter :base-type :int16)
   ((:evfilt-read   "EVFILT_READ"))
   ((:evfilt-write  "EVFILT_WRITE"))
   ((:evfilt-empty  "EVFILT_EMPTY"))
   ((:evfilt-aio    "EVFILT_AIO"))
   ((:evfilt-vnode  "EVFILT_VNODE"))
   ((:evfilt-proc   "EVFILT_PROC"))
   ((:evfilt-signal "EVFILT_SIGNAL"))
   ((:evfilt-timer  "EVFILT_TIMER"))
   ((:evfilt-user   "EVFILT_USER")))

  (cstruct kevent "struct kevent"
	   (ident  "ident"  :type :uint64)
	   (filter "filter" :type kqueue-filter)
	   (flags  "flags"  :type kqueue-flags)
	   (fflags "fflags" :type :uint32)
	   (data   "data"   :type :int64)
	   (udata  "udata"  :type :pointer)
	   (ext    "ext"    :type :uint64 :count 4))

  (ctype time-t "time_t")
  (cstruct timespec "struct timespec"
	   (sec "tv_sec" :type time-t)
	   (nsec "tv_nsec" :type :long))
  )
