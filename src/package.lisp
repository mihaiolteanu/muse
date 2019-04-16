(defpackage :objects
  (:use :cl)
  (:export artist
           artist-name
           artist-genres
           artist-similar
           artist-albums
           genre
           genre-name
           song
           song-name
           song-duration
           song-url
           song-lyrics
           album
           album-name
           album-year
           album-songs))

(defpackage :parser
  (:use :cl :dexador :plump :lquery :objects)
  (:import-from :alexandria :with-gensyms)
  (:import-from :asdf :system-relative-pathname)
  (:import-from :alexandria :lastcar)
  (:import-from :cl-utilities :split-sequence)
  (:shadowing-import-from :dexador :get)
  (:shadowing-import-from :dexador :delete)
  (:export new-artist
           with-local-htmls
           parse-html
           url-name
           clean-name))

(defpackage :persistence
  (:use :cl :sqlite :objects :parser)
  (:export artists
           songs
           all-songs
           albums
           genres
           all-genres
           all-genre-songs
           similar
           insert-artist
           with-test-db))

(defpackage :player
  (:use :cl :objects :uiop :bt)
  (:export play-song
           play-songs
           continue-with-video
           playing?
           what-is-playing
           next-song
           previous-song
           play-pause
           kill-player
           seek
           quit))

(defpackage :server
  (:use :cl :persistence :parser :objects :player :cl-who :hunchentoot))
