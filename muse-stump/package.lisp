(defpackage :muse-stump
  (:use :cl :stumpwm :dexador :plump :lquery)
  (:import-from :alexandria :lastcar)
  (:import-from :cl-utilities :split-sequence)
  (:shadowing-import-from :dexador :get)
  (:shadowing-import-from :dexador :delete)
  (:export play-artist next-song stop-player
           #:select-artist))


