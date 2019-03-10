(in-package :persistence)

(defparameter *db*
  (connect (merge-pathnames "music.db" (asdf:system-relative-pathname :muse ""))))

(defun prepare-in-string (lst)
  "Transform '((1) (2) (3)) into '(1, 2, 3)"
  (format nil "(~{~{~A~}~^, ~})" lst))

(defun songs (artist)
  (execute-to-list
   *db*
   (format nil "SELECT * FROM artist_songs_view WHERE artist=\"~A\"" artist)))

(songs "anathema")

;; (execute-single *db* "SELECT last_insert_rowid()")
;; (execute-non-query *db* "INSERT INTO song(id, name, url) VALUES(NULL, \"Weight\", \"https://www.youtube.com/watch?v=aAst-vyasiM\");")
;; (execute-non-query *db* "INSERT INTO artist_songs(artist, song) VALUES (\"Antimatter\", 1)")


