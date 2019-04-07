(in-package :server)

(defparameter *my-acceptor*
  (make-instance
   'easy-acceptor :port 4007
                  :document-root
                  (namestring (merge-pathnames
                               "src/" (asdf:system-relative-pathname :muse "")))))
(start *my-acceptor*)

(defmacro with-html (&body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t)
     ,@body))

(defun artist-from-uri ()
  (clean-name
   (first (last (cl-utilities:split-sequence #\/ (request-uri*))))))

(defun genre-from-uri ()
  (first (last (cl-utilities:split-sequence #\/ (request-uri*)))))

(defmacro display-songs (lst)
  `(loop for song in ,lst
         do (htm (:p (:a :href (song-url song)
                         :class (str "song")
                         (str (song-name song)))
                     (str (format nil " [~a]" (song-duration song)))))))

(defparameter *play-pause-button* "/img/play.png")

(defmacro standard-page (&body body)
  `(with-html-output-to-string (s)
     (:html
      (:body
       (:a :href (conc "/stop?source-uri=" (request-uri*))
           (:img :src "/img/stop.png"))
       (:text "      ")
       (:a :href (conc "/play-pause?source-uri=" (request-uri*))
           (:img :src *play-pause-button*))
       (:text "      ")
       (:a :href "/artists" "artists")
       ,@body))))

(defun s-artists ()
  (standard-page
      (:h2 "Available Artists")
    (dolist (artist (artists))
      (let ((name (first artist)))
        (htm (:p (:a :href (format nil "/artist/~a" (url-name name))
                     (str name))))))))

(defun s-artist-songs ()
  (let ((artist (artist-from-uri)))
    (standard-page
        (:h2 (str (format nil "~a songs" artist)))
      (display-songs (songs artist)))))

(defun play-artist-songs ()
  (let ((artist (artist-from-uri)))
    (play-songs (list (first (songs artist))
                      (second (songs artist))))
    (standard-page
        (:h2 (str (format nil "Playing ~a songs" artist)))
      (display-songs (songs artist)))))

(defun s-genres ()
  (standard-page
      (:h2 "Available Genres")
    (loop for (g) in (all-genres)
          do (htm (:p (:a :href (format nil "/genre/~a" g)
                          (str g)))))))

(defun s-genre-songs ()
  (let ((genre (genre-from-uri)))
    (standard-page
        (:h2 (str (format nil "~a songs" genre)))
      (display-songs (all-genre-songs genre)))))

(defun play-genre-songs ()
  (let ((genre (genre-from-uri)))
    (standard-page
        (:h2 (str (format nil "Playing ~a songs" genre)))
      (display-songs (all-genre-songs genre)))))

(defun redirect-to-source ()
  (redirect (url-name (get-parameter "source-uri"))))

(defun s-play-pause ()
  (play-pause)
  (redirect-to-source))

(defun s-stop ()
  (kill-player)
  (redirect-to-source))

(defun s-artist-similar ()
  (let ((artist (artist-from-uri)))
    (standard-page
        (:h2 (str (format nil "~a similar artists" artist)))
      (loop for (a) in (similar artist)
            do (htm (:p
                     (:a :href (format nil "/artist/~a" (url-name a))
                         (str a))))))))

(defparameter *base-directory*
  (make-pathname
   :name nil
   :type nil
   :defaults #.(or *compile-file-truename* *load-truename*)))

(setq *dispatch-table*
      (nconc (list
              (create-regex-dispatcher "^/artist/[a-zA-Z0-9 ]+$" 's-artist-songs)
              (create-regex-dispatcher "^/genre/[a-zA-Z0-9 ]+$" 's-genre-songs)
              (create-regex-dispatcher "^/similar/[a-zA-Z0-9 ]+$" 's-artist-similar))
              (create-folder-dispatcher-and-handler
               "/img/"
               (merge-pathnames (make-pathname :directory '(:relative "img"))
                                *base-directory*)))
             (mapcar (lambda (args)
                       (apply 'create-prefix-dispatcher args))
                     '(("/test.html" parameter-test)
                       ("/stop" s-stop)
                       ("/play-pause" s-play-pause)
                       ("/artists" s-artists)
                       ("/genres" s-genres)))))


;; (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 4123))
;; (asdf:oos 'asdf:load-op :hunchentoot-test)
