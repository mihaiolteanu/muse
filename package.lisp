(defpackage :objects
  (:use :cl)
  (:export artist artist-name artist-genres artist-similar artist-albums
           genre genre-name
           song song-name song-duration song-url song-lyrics
           album album-name album-year album-songs))

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
  (:export songs albums genres insert-artist
           with-test-db))

(defpackage :player
  (:use :cl :uiop)
  (:export play play-pause seek quit))

(defpackage :muse  
  (:use :cl :unix-opts :player)
  (:shadowing-import-from :unix-opts :describe)
  (:export main))
