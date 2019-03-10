(defpackage :muse  
  (:use :cl :unix-opts)
  (:shadowing-import-from :unix-opts :describe)
  (:export main))

(defpackage :persistence
  (:use :cl :sqlite))

(defpackage :parser
  (:use :cl :dexador :plump :lquery)
  (:import-from :alexandria :with-gensyms)
  (:import-from :asdf :system-relative-pathname)
  (:shadowing-import-from :dexador :get)
  (:shadowing-import-from :dexador :delete))

(defpackage :player
  (:use :cl :uiop))
