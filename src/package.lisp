(defpackage :objects
  (:use :cl)
  (:export make-artist
           artist-name
           artist-genres
           artist-similar
           artist-albums
           artist-songs
           make-genre
           genre-name
           make-song
           same-songs
           song-artist-name
           song-name
           song-duration
           song-url
           song-lyrics
           make-album
           album-name
           album-year
           album-songs))

(defpackage :parser
  (:use :cl :dexador :plump :lquery :objects)
  (:import-from :alexandria :with-gensyms)
  (:import-from :asdf :system-relative-pathname)
  (:import-from :alexandria :lastcar)
  (:import-from :cl-utilities :split-sequence)
  (:import-from :do-urlencode :urlencode)
  (:shadowing-import-from :dexador :get)
  (:shadowing-import-from :dexador :delete)
  (:export new-artist
           artists-with-tag
           available-youtube-video?
           song-lyrics-from-web
           with-local-htmls
           parse-html
           url-name
           clean-name))

(defpackage :persistence
  (:use :cl :sqlite :objects :parser)
  (:import-from :alexandria :if-let)
  (:export all-artists
           all-songs
           all-genres
           all-genre-artists
           artist-from-db
           insert-artist
           songs-from-lyrics
           artist-song-from-db
           save-song-lyrics
           with-test-db))

(defpackage :player
  (:use :cl :trivia :mockingbird :parser :persistence :objects :uiop :bt)
  (:import-from :alexandria :switch)
  (:import-from :cl-utilities :split-sequence)
  (:import-from :yason :parse)
  (:export play
           playing?
           playing-song
           playing-songs
           open-playing-song-in-browser
           next-song
           prev-song
           toggle-play
           stop-player
           with-test-player
           seek
           #:*playing-songs*))

(defpackage :server
  (:use :cl :persistence :parser :objects :player :cl-who :hunchentoot)
  (:import-from :cl-utilities :split-sequence)
  (:export
   #:display-songs))
