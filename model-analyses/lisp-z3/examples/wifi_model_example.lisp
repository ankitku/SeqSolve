(pushnew (truename "../") ql:*local-project-directories*)
(ql:register-local-projects)
(ql:quickload :lisp-z3)

(defpackage :z3-wifi-model
  (:use :cl :z3))

(in-package :z3-wifi-model)

(solver-init)
(register-tuple-sort
 :frametype
 ((version . (:bv 2))
  (ptype . (:bv 2))
  (subtype . (:bv 3))
  (toDs . :bool)
  (fromDs . :bool)
  (moreFrag . :bool)
  (retry . :bool)
  (powerMgmt . :bool)
  (moreData . :bool)
  (WEP . :bool)
  (order . :bool)
  (durationID . (:bv 12))
  (addr1 . (:bv 48))
  (addr2 . (:bv 48))
  (addr3 . (:bv 48))
  (fragmentNum . (:bv 4))
  (seqNumber . (:bv 12))
  (addr4 . (:bv 48))
  (checksum . (:bv 32))))
