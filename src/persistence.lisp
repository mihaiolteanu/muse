(in-package :persistence)

(defparameter *db*
  (connect (merge-pathnames "music.db" (asdf:system-relative-pathname :muse ""))))

(defun last-insert-rowid ()
  (execute-single *db* "SELECT last_insert_rowid()"))

(defun available-p (artist)
  (let ((available
          (execute-single *db*
           (format nil
                   "SELECT available FROM artist WHERE name=\"~a\""
                   artist))))
    (when available
      (= available 1))))

;; Extract from db
(defun retrieve (what from-where condition)
  (execute-to-list *db*
   (format nil "SELECT ~a FROM ~a WHERE ~a"
           what from-where condition)))

(defun artists ()
  (retrieve "*" "artist" "available=1"))

(defun songs (artist)
  "Retrieve all songs for the given artist from the db. If artist is not in the db,
download the artist from the web and save it in the db."
  (let ((artist (clean-name artist)))
    (if (available-p artist)
        (let ((raw-songs
                (retrieve "*" "all_songs"
                          (format nil "artist=\"~a\"" artist))))
          (mapcar (lambda (song)
                    (make-instance 'song
                     :name     (third song)
                     :duration (fifth song)
                     :url      (fourth song)))
                  raw-songs))
        (progn
          (insert-artist (new-artist artist))
          ;; Retry now, after retrieving and saving to db.
          (songs artist)))))

(defun all-songs ()
  (retrieve "*" "all_songs" 1))

(defun albums (artist)
  (retrieve "*" "artist_albums_view"
            (format nil "artist=\"~a\"" artist)))

(defun genres (artist)
  (retrieve "genre" "artist_genres"
            (format nil "artist=\"~a\"" artist)))

(defun all-genres ()
  (retrieve "*" "genre" 1))

(defun all-genre-songs (genre)
  (let ((artists (retrieve "artist" "artist_genres"
                           (format nil "genre=\"~a\"" genre))))
    (apply #'append
           (mapcar (lambda (a)
                     (songs (first a)))
                   artists))))

(defun similar (artist)
  (retrieve "similar" "artist_similar"
            (format nil "artist=\"~a\"" artist)))

;; Insert into db
(defun execute (template &rest values)
  (execute-non-query *db*
   (apply #'format `(nil ,template ,@values))))

(defun insert-album (artist album)
  (execute "INSERT INTO album(name,release_date) VALUES(\"~a\", ~a)"
           (album-name album) (album-year album))
  (let ((album-id (last-insert-rowid)))
    (mapcar (lambda (s)
              (execute "INSERT INTO song(name,duration,url) VALUES(\"~a\", \"~a\", \"~a\")"
                       (song-name s)
                       (song-duration s)
                       (song-url s))
              (let ((song-id (last-insert-rowid)))
                (execute "INSERT INTO album_songs(album_id,song_id) VALUES(~a, ~a)"
                         album-id
                         song-id)))
            (album-songs album))
    (execute "INSERT INTO artist_albums(artist,album_id) VALUES(\"~a\", ~a)"
             artist
             album-id)))

(defun insert-albums (artist albums)
  (mapcan (lambda (a)
            (insert-album artist a))
          albums))

(defun insert-artist-name (name available)
  (execute "INSERT INTO artist(name,available) VALUES(\"~a\", ~a)" name available))

(defun insert-genres (names)
  (execute "INSERT INTO genre(name) VALUES~{(\"~A\")~^,~}" names))

(defun insert-genres-assoc (artist genres)
  (declare (type (simple-array character) artist))
  (execute "INSERT INTO artist_genres(artist,genre) VALUES~{(~{\"~A\"~^,~})~^,~}"
              (mapcar (lambda (a)
                     (list artist (genre-name a)))
                   genres)))

(defun insert-similar (artist artists)
  (execute "INSERT INTO artist(name) VALUES~{(\"~A\")~^,~}" artists)
  (execute "INSERT INTO artist_similar(artist,similar) VALUES~{(~{\"~A\"~^,~})~^,~}"
           (mapcar (lambda (a)
                     (list artist a))
                   artists)))

(defun insert-artist (artist)
  (declare (artist artist))
  (let ((name (artist-name artist)))
    (insert-artist-name name 1)
    (insert-similar name (map 'list #'identity (artist-similar artist)))
    (insert-genres (mapcar #'genre-name (artist-genres artist)))
    (insert-genres-assoc name (artist-genres artist))
    (insert-albums name (artist-albums artist))))


;; Delete from db
(defun clean-db ()
  (execute "DELETE FROM album_songs")  
  (execute "DELETE FROM artist_albums")
  (execute "DELETE FROM artist_genres")
  (execute "DELETE FROM artist_similar")
  (execute "DELETE FROM album")
  ;; Reset the autoincremented id
  (execute "DELETE FROM sqlite_sequence where name=\"album\"")
  (execute "DELETE FROM artist")
  (execute "DELETE FROM genre")
  (execute "DELETE FROM song")
  (execute "DELETE FROM sqlite_sequence where name=\"song\""))

(defmacro with-test-db (&body body)
  "Use a clean and identical test database for all further
requests. Let binding might be local only in the calling thread so
a setf is needed to temporarily use a test db."
  `(let ((original-db *db*))
     (setf *db* (connect
                 (merge-pathnames "tests/music_test.db"
                                  (asdf:system-relative-pathname :muse ""))))
     ,@body
     (clean-db)
     (setf *db* original-db)))
