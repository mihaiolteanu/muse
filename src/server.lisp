(in-package :server)

(defparameter *port* 4007)
(load #P"~/muserc.lisp" :if-does-not-exist nil)

(defparameter *my-acceptor*
  (make-instance
   'easy-acceptor :port *port*
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

(defparameter *pause-button* "⏸")
(defparameter *play-button* "▶")
(defparameter *play-pause-button* *play-button*)

(defmacro standard-page (&body body)
  `(with-html-output-to-string (s)
     (:html
      (:body
       (:a :style "text-decoration:none"
           :href (conc "/stop?source-uri=" (request-uri*)) "⏹")
       (:text " ")
       (:a :style "text-decoration:none"
           :href (conc "/previous?source-uri=" (request-uri*)) "⏪")
       (:text " ")
       (:a :style "text-decoration:none"
           :href (conc "/play-pause?source-uri=" (request-uri*)) (str *play-pause-button*))
       (:text " ")
       (:a :style "text-decoration:none"
           :href (conc "/next?source-uri=" (request-uri*)) "⏩")
       (:br)
       (:a :href "/artists" "artists")
       ,@body))))

(defun server-status ()
  (standard-page
    (:h2 "Server Status")
    (:h3 "Player Status")
    (:p :class "player-status"
        (if (playing?)
            (htm
              (:p :class "playing-song"
                  (str "playing")
                  (display-songs (list (what-is-playing)))))
            (str "stopped")))))

(defun s-artists ()
  (standard-page
    (:h2 "Available Artists")
    (dolist (artist (artists))
      (let ((name (artist-name artist)))
        (htm (:p (:a :class "artist"
                     :href (format nil "/artist/~a" (url-name name))
                     (str name))))))))

(defun s-artist-songs ()
  (let ((artist (artist-from-uri)))
    (standard-page
        (:h2 (str (format nil "~a songs" artist)))
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

(defun redirect-to-source ()
  "Only redirect back to calling source, when such a source was given"
  (let ((source-uri (get-parameter "source-uri")))
    (when source-uri
      (redirect (url-name source-uri)))))

(defun s-play-pause ()
  "If the player is already started, toggle the play/pause status of
the player and change the button status.  Otherwise, figure out what
page we were on when the button was pressed and play the contents for
that page based on the url. For example, if we were on an artist page
when the button was pressed, play that artist songs, if on similar
artists page, play similar artists, and so on."
  (if (playing?)
      (progn
        (if (string= *play-pause-button* *play-button*)
            (setf *play-pause-button* *pause-button*)
            (setf *play-pause-button* *play-button*))
        (play-pause))
      (let ((what-to-play (uiop:split-string
                           (get-parameter "source-uri")
                           :separator "/")))
        (cond ((string= (second what-to-play) "artist")
               (play-songs (songs (third what-to-play))))

              ((string= (second what-to-play) "genre")
               (format t "Playing the ~a genre~%" (third what-to-play)))

              ((string= (second what-to-play) "similar")
               (format t "Playing similar artists to ~a~%" (third what-to-play))))
        (setf *play-pause-button* *pause-button*)))
  (redirect-to-source))

(defun s-previous ()
  (when (playing?)
    (previous-song))
  (redirect-to-source))

(defun s-next ()
  (when (playing?)
    (next-song))
  (redirect-to-source))

(defun s-stop ()
  (when (playing?)
    (setf *play-pause-button* *play-button*)
    (kill-player))
  (redirect-to-source))

(defun s-continue-with-video ()
  (when (playing?)
    (continue-with-video)))

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
              (create-regex-dispatcher "^/similar/[a-zA-Z0-9 ]+$" 's-artist-similar)
              (create-folder-dispatcher-and-handler
               "/img/"
               (merge-pathnames (make-pathname :directory '(:relative "img"))
                                *base-directory*)))
             (mapcar (lambda (args)
                       (apply 'create-prefix-dispatcher args))
                     '(("/status" server-status)
                       ("/video" s-continue-with-video)
                       ("/stop" s-stop)
                       ("/previous" s-previous)
                       ("/play-pause" s-play-pause)
                       ("/next" s-next)
                       ("/artists" s-artists)
                       ("/genres" s-genres)))))

;; (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 4123))
;; (asdf:oos 'asdf:load-op :hunchentoot-test)
