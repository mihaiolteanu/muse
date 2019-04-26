(in-package :objects)

(defclass artist ()
  ((name    :initarg :name
            :accessor artist-name)
   (genres  :initarg :genres
            :initform nil
            :accessor artist-genres)
   (similar :initarg :similar
            :initform nil
            :accessor artist-similar)
   (albums  :initarg :albums
            :initform nil
            :accessor artist-albums)))

(defclass genre ()
  ((name :initarg :name :accessor genre-name)))

(defclass song ()
  ((artist   :initarg :artist   :accessor song-artist-name)
   (name     :initarg :name     :accessor song-name)
   (duration :initarg :duration :accessor song-duration)
   (url      :initarg :url      :accessor song-url    :initform nil)
   (lyrics   :initarg :lyrics   :accessor song-lyrics :initform nil)
   (likeness :initarg :likeness :accessor song-likeness :initform 0)))

(defclass album ()
  ((name  :initarg :name  :accessor album-name)
   (year  :initarg :year  :accessor album-year :initform 0 )
   (songs :initarg :songs :accessor album-songs)))

(defun artist-songs (artist)
  (apply #'append
         (mapcar (lambda (album)
                   (album-songs album))
                 (artist-albums artist))))

(defun same-songs (song1 song2)
  (and (string-equal (song-artist-name song1)
                     (song-artist-name song2))
       (string-equal (song-name song1)
                     (song-name song2))
       (equal (song-duration song1)
              (song-duration song2))
       (equal (song-url song1)
              (song-url song2))))

(defun make-artist (name &key genres similar albums)
  (make-instance 'artist
                 :name name
                 :genres genres
                 :similar similar
                 :albums albums))

(defun make-genre (name)
  (make-instance 'genre
                 :name name))

(defun make-song (artist name duration url &key lyrics likeness)
  (make-instance 'song
                 :artist artist
                 :name name
                 :duration duration
                 :url url
                 :lyrics lyrics
                 :likeness likeness))

(defun make-album (name year songs)
  (make-instance 'album
                 :name name
                 :year year
                 :songs songs))

(defmethod print-object ((obj artist) stream)
  (print-unreadable-object (obj stream :type nil)
    (format stream "Artist: ~A~% Genres: ~A~% Similar: ~A~% ~{~A~%~}"
            (artist-name obj)
            (format nil "~{~A~^, ~}" (mapcar #'genre-name (artist-genres obj)))
            (format nil "~{~A~^, ~}" (map 'list #'artist-name (artist-similar obj)))
            (artist-albums obj))))

(defmethod print-object ((obj genre) stream)
  (print-unreadable-object (obj stream :type nil)
    (format stream "~A" (genre-name obj))))

(defmethod print-object ((obj album) stream)
  (print-unreadable-object (obj stream :type nil)
    (format stream "Album: ~A, Year: ~A~% ~A"
            (album-name obj)
            (album-year obj)
            (album-songs obj))))

(defmethod print-object ((obj song) stream)
  (print-unreadable-object (obj stream :type nil)
    (format stream "~A ~A (~A) ~A"
            (song-artist-name obj)
            (song-name obj)
            (song-duration obj)
            (song-url obj))))
