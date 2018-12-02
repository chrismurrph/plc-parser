(ns domain-constants)

;;
;; Zeroing the numbers must be a long term loss of battery thing, that has never happened. For example
;; "NumberOfConnections" is 2 and no ladder logic ever changes it. In fact I think data values can never
;; be lost because they come in with the program, and can't really ever be lost. When I think of zero-ed
;; data it must mean doing the initialization routine again. Ben Paton doesn't like it presumably because
;; it will overwrite what is in the file coming across from version control, which won't be where the
;; sequence was when the machine died, but will be better than my 'factory start' values.
;;
(def zero-retrieved-value? false)

;;
;; These for timer rather than counter
;; We don't yet know what the first (0th) is
;; TODO The first is RECORDED-idx, so change below from 3 to 0
;;
(def TIMER-ACC-idx 2)
(def TIMER-PRE-idx 1)
(def TIMER-RECORDED-idx 3)
;; counter:
(def COUNTER-ACC-idx 0)
(def COUNTER-PRE-idx 1)
;;
(def PATH-idx 0)
(def ER-idx 1)
(def DN-idx 2)

(def PRECISION 10)


