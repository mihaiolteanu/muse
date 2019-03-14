(defpackage :objects
  (:use :cl)
  (:export artist genre song album))

(defpackage :parser
  (:use :cl :dexador :plump :lquery :objects)
  (:import-from :alexandria :with-gensyms)
  (:import-from :asdf :system-relative-pathname)
  (:import-from :alexandria :lastcar)
  (:import-from :cl-utilities :split-sequence)
  (:shadowing-import-from :dexador :get)
  (:shadowing-import-from :dexador :delete)
  (:export new-artist with-local-htmls))

(defpackage :persistence
  (:use :cl :sqlite :objects :parser)
  (:export songs albums genres))

(defpackage :player
  (:use :cl :uiop)
  (:export play play-pause seek quit))

(defpackage :muse  
  (:use :cl :unix-opts :player)
  (:shadowing-import-from :unix-opts :describe)
  (:export main))
