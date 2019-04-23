(in-package :persistence)

(defparameter *db*
  (connect
   (merge-pathnames "music.db"
                    (asdf:system-relative-pathname :muse ""))))

(defun last-insert-rowid ()
  (execute-single *db* "SELECT last_insert_rowid()"))

(defun db-get (what from-where condition)
  (execute-to-list *db*
                   (format nil "SELECT ~a FROM ~a WHERE ~a"
                           what from-where condition)))

(defun db-artist-available (artist)
  (if-let ((available
            (db-get "available" "artist" (format nil "name=\"~a\"" artist))))
    (first (first available))
    nil))

(defun db-all-artists ()
  "Only return artists which have full info available (songs, albums, etc.)"
  (db-get "*" "artist" "available=1"))

(defun db-all-songs ()
  (db-get "*" "all_songs" 1))

(defun db-artist-albums (artist)
  (db-get "*" "artist_albums_view"
          (format nil "artist=\"~a\"" artist)))

(defun db-song-id-from-album-id (album-id)
  (mapcar #'first
          (db-get "song_id" "album_songs"
                  (format nil "album_id=~a" album-id))))

(defun db-song-from-song-id (song-id)
  (first (db-get "*" "song"
                 (format nil "id=~a" song-id))))

(defun db-artist-genres (artist)
  (mapcar #'first
          (db-get "genre" "artist_genres"
                  (format nil "artist=\"~a\"" artist))))

(defun db-all-genres ()
  (mapcar #'first
          (db-get "*" "genre" 1)))
(defun db-genre-artists (genre)
  (mapcar #'first
          (db-get "artist" "genre_artists"
                  (format nil "genre=\"~a\"" genre))))

(defun db-similar-artists (artist)
  (mapcar #'first
          (db-get "similar" "artist_similar"
                  (format nil "artist=\"~a\"" artist))))

(defun artist-available? (artist)
  (if-let ((available (db-artist-available artist)))
    (= available 1)
    nil))

(defun all-artists ()
  (mapcar (lambda (a)
            (make-artist (first a)))
          (db-all-artists)))

(defun all-songs ()
  "All available songs from db."
  (mapcar (lambda (song)
            (make-song (first song) (third song) (fifth song) (fourth song)))
          (db-all-songs)))

(defun album-songs-from-album-id (album-id artist)
  (mapcar (lambda (song-id)
            (let ((raw-song (db-song-from-song-id song-id)))
              (make-song artist (second raw-song)
                         (third raw-song) (fourth raw-song))))
          (db-song-id-from-album-id album-id)))

(defun all-genres ()
  (mapcar (lambda (genre-name)
  
            (make-genre genre-name))
          (db-all-genres)))

(defun all-genre-artists (genre)
  "All artists from the given genre. If not found in db, fetch from
web, save to db and retry."
  (if-let ((artists (db-genre-artists genre)))
    (mapcar (lambda (artist)
              (make-artist artist))
            artists)
    (let ((artists (artists-with-tag genre)))        
        (when artists ;Only continue if genre actually exists in the wild.
          (insert-genre-artists genre artists)
          (all-genre-artists genre)))))

(defun artist-albums-from-db (artist)
  (mapcar (lambda (album)
            (make-album (third album) (fourth album)
                        (album-songs-from-album-id (first album) artist)))
          (db-artist-albums artist)))

(defun artist-genres-from-db (artist)
  (mapcar (lambda (genre-name)
            (make-genre genre-name))
          (db-artist-genres artist)))

(defun artist-similar-from-db (artist)
  "Only adds similar artist names, but not their songs "
  (mapcar (lambda (name)
            (make-artist name))
          (db-similar-artists artist)))

(defun artist-from-db (artist)
  "Complete artist object from db (albums, songs, similar, tags).
If artist doesn't exist, go fetch it, add it to db and retry."
  (if (artist-available? artist)
      (make-artist artist
               :genres (artist-genres-from-db artist)
               :similar (artist-similar-from-db artist)
               :albums (artist-albums-from-db artist))
      (progn
        (insert-artist (new-artist artist))
        (artist-from-db artist))))

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
  (mapcar (lambda (a)
            (unless (artist-available? a)
              (insert-artist-name a 0)))
          artists)
  ;; (execute "INSERT INTO artist(name) VALUES~{(\"~A\")~^,~}" artists)
  (execute "INSERT INTO artist_similar(artist,similar) VALUES~{(~{\"~A\"~^,~})~^,~}"
           (mapcar (lambda (a)
                     (list artist a))
                   artists)))

(defun insert-artist (artist)
  (let ((name (artist-name artist)))
    (insert-artist-name name 1)
    (insert-similar name (map 'list #'identity (artist-similar artist)))
    (insert-genres (mapcar #'genre-name (artist-genres artist)))
    (insert-genres-assoc name (artist-genres artist))
    (insert-albums name (artist-albums artist))))

(defun insert-genre-artists (genre artists)
  (execute "INSERT INTO genre_artists(artist,genre) VALUES~{(~{\"~A\"~^,~})~^,~}"
           (mapcar (lambda (a)
                     (list a genre))
                   artists)))

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
  (execute "DELETE FROM genre_artists")
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
     (clean-db)
     ,@body
     (clean-db)
     (setf *db* original-db)))
