(in-package :server)

(defparameter *port* 4007)
(load #P"~/muserc.lisp" :if-does-not-exist nil)

(defparameter *pause-symbol* "⏸")
(defparameter *play-symbol* "▶")
(defparameter *play-button* *play-symbol*)
(defparameter *shuffle-play* nil)
(defparameter *shuffle-off* "[ ]")
(defparameter *shuffle-on*  "[s]")
(defparameter *shuffle-status* *shuffle-off*)

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
   (first (last (split-sequence #\/ (request-uri*))))))

(defun tag-from-uri ()
  (clean-name
   (first (last (split-sequence #\/ (request-uri*))))))

(defun artist-and-album-from-uri ()
  (let ((seq (split-sequence #\/ (request-uri*))))
    (values (third seq)
            (fifth seq))))

(defmacro display-songs (songs &key (with-artist nil))
  "We don't need to display the artist name all the time for
evey song on the page, since that info can be inferred."
  `(htm
    (:div :style "text-align: center"
          (:div :style "display: inline-block; text-align:left"
                (dolist (song ,songs)
                  (let ((name (song-name song)))
                    (htm (:a :class "song"
                             :href (song-url song)
                             (str
                              ,(if with-artist
                                   `(format nil  "~a - ~a "
                                            (song-artist-name song)
                                            (song-name song))
                                   `(format nil "~a" (song-name song))))
                             ;; Additional href string to add to the link
                             (unless (uiop:emptyp ,string)
                               (str (format nil "| ~a" ,string))))
                         (:br))))))))

(defmacro display-artists (artists)
  `(dolist (artist ,artists)
     (let ((name (artist-name artist)))
       (htm (:a :class "artist"
                :href (format nil "/artist/~a" (url-name name))
                (str name))
            (:br)))))

(defmacro display-similar (artists)
  `(dolist (artist ,artists)
     (let ((name (artist-name artist)))
       (htm (:a :class "similar"
                :href (format nil "/artist/~a" (url-name name))
                (str name))))))

(defmacro display-tags (tags)
  `(dolist (tag ,tags)
     (let ((name (genre-name tag)))
       (htm (:a :class "tag"
                :href (format nil "/tag/~a" (url-name name))
                (str name))))))

(defmacro display-albums (albums)
  "Return a html snippet with all album names"
  `(dolist (album ,albums)
     (let ((name (album-name album)))
       (htm (:a :class "album"
                :href (format nil "/artist/~a/album/~a"
                              (url-name artist-name) (url-name name))
                (str name))))))

(defun button-link (action)
  (format nil "/~a?source=~a&redirect=t" action (request-uri*)))

(defmacro standard-page (&body body)
  `(with-html-output-to-string (s)
     (:html
      (:head
       (:link :type "text/css"
              :rel "stylesheet"
              :href "/css/muse.css"))
      (:body
       (:p :class "play-buttons"
           (:a :href (button-link "stop") "⏹")
           (:a :href (button-link "previous") "⏪")
           (:a :href (button-link "play") (str *play-button*))
           (:a :href (button-link "next") "⏩")
           (:a :class "shuffle-status"
               :href (button-link "toggle-shuffle")
               (str *shuffle-status*)))
       (:p :class "menu-bar"
           (:a :href "/home" "home")
           (:a :href "/artists" "artists")
           (:a :href "/songs" "songs")
           (:a :href "/tags" "tags"))
       ,@body))))

(defun server-status ()
  (standard-page
    (:h2 "Server Status")
    (:h3 "Player Status")
    (if (playing?)
        (htm
         (:div :class "player-status" (str "playing"))
         (:p :class "playing-song"
             (display-songs (list (playing-song)) :with-artist t))
         (:p :class "playing-song-lyrics"
             (str (song-lyrics (playing-song)))))
        (htm
         (:div :class "player-status" (str "stopped"))))))

(defun artists-page ()
  (standard-page
    (:h2 "Available Artists")
    (display-artists (all-artists))))

(defun songs-page ()
  (standard-page
    (:h2 "All Database Songs")
    (display-songs (all-songs) :with-artist t)))

(defun tags-page ()
  (standard-page
    (:h2 "Available Tags")
    (display-tags (all-genres))))

(defun tag-artists-page ()
  (let ((tag (tag-from-uri)))
    (standard-page
      (:h2 (str (format nil "Artists with the ~a tag" tag)))
      (display-artists (all-genre-artists tag)))))

(defun artist-info-page ()
  (let* ((artist-name (artist-from-uri))
         (artist (artist-from-db artist-name)))
    (standard-page
      (:h2 (str (artist-name artist)))
      (display-tags (artist-genres artist))
      (:h2 (str "similar artists"))
      (display-similar (artist-similar artist))
      (:h2 (str "albums"))
      (display-albums (artist-albums artist))
      (:h2 (str "songs"))
      (display-songs (artist-songs artist))
      )))

(defun artist-album-page ()
  (multiple-value-bind (artist album)
      (artist-and-album-from-uri)
    (setf artist (clean-name artist))
    (setf album (clean-name album))
    (standard-page
      (:h2 (str (format nil "~a - ~a (album)" album artist)))
      (let ((alb (find album (artist-albums (artist-from-db artist))
                       :key #'album-name
                       :test #'string=)))
        (when alb
          (display-songs (album-songs alb)))))))

(defun redirect-to-source ()
  "Only redirect back to calling source, when such a source was given"
  (let ((red (get-parameter "redirect"))
        (source (get-parameter "source")))
    (when (and source red)
      (when (string-equal red "T")
        (redirect (url-name source))))))

(defun toggle-play-button ()
  (if (string= *play-button* *play-symbol*)
      (setf *play-button* *pause-symbol*)
      (setf *play-button* *play-symbol*)))

(defun split-play-parameter (param)
  ;; First element after splitting will be an empty string; skip it
  (rest (uiop:split-string
         param
         :separator "/")))

(defun play-page ()
  "If already playing, pause it and change the playing buttons state.
If not, figure out what needs to be played and send it to the player
as a list of strings. All play requests go to the same url which
redirects to this function, with the parameter source set to what
needs to be played."
  (if (playing?)
      (progn
        (toggle-play-button)
        (toggle-play))
      (progn
        (play (split-play-parameter (get-parameter "source")) *shuffle-play*)
        (toggle-play-button)))
  (redirect-to-source))

(defun previous-page ()
  (when (playing?)
    (prev-song))
  (redirect-to-source))

(defun next-page ()
  (when (playing?)
    (next-song))
  (redirect-to-source))

(defun shuffle-page ()
  "Toggle the shuffle status, but only if player is not already started.
Stop, change the shuffle status and restart the player if that's what
you need."
  (unless (playing?)
    (if *shuffle-play*
        (progn
          (setf *shuffle-play* nil)
          (setf *shuffle-status* *shuffle-off*))
        (progn
          (setf *shuffle-play* T)
          (setf *shuffle-status* *shuffle-on*))))
  (redirect-to-source))

(defun stop-page ()
  (when (playing?)
    (setf *play-button* *play-symbol*)
    (stop-player))
  (redirect-to-source))

(defun continue-with-video-page ()
  (when (playing?)
    (open-playing-song-in-browser)))

(defparameter *base-directory*
  (make-pathname
   :name nil
   :type nil
   :defaults #.(or *compile-file-truename* *load-truename*)))

(setq *dispatch-table*
      (nconc (list              
              (create-regex-dispatcher
               "^/artist/[a-zA-Z0-9 ]+$" 'artist-info-page)
              (create-regex-dispatcher
               "^/artist/[a-zA-Z0-9 ]+/album/[a-zA-Z0-9 ]+$" 'artist-album-page)
              (create-regex-dispatcher
               "^/similar/[a-zA-Z0-9 ]+$" (lambda () (print "ignore")))
              (create-regex-dispatcher
               "^/tag/[a-zA-Z0-9 ]+$" 'tag-artists-page)
              (create-regex-dispatcher
               "^/lyrics/[a-zA-Z0-9 ]+$" 'songs-from-lyrics-page)
              (create-folder-dispatcher-and-handler "/img/"
               (merge-pathnames (make-pathname :directory '(:relative "img"))
                                *base-directory*))
              (create-folder-dispatcher-and-handler "/css/"
               (merge-pathnames (make-pathname :directory '(:relative "css"))
                                *base-directory*)))
             (mapcar (lambda (args)
                       (apply 'create-prefix-dispatcher args))
                     '(("/home" server-status)
                       ("/video" continue-with-video-page)
                       ("/stop" stop-page)
                       ("/previous" previous-page)
                       ("/play" play-page)
                       ("/next" next-page)
                       ("/toggle-shuffle" shuffle-page)
                       ("/artists" artists-page)
                       ("/songs" songs-page)
                       ("/tags" tags-page)
                       ("/genres" genres-page)))))

;; (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 4123))
;; (asdf:oos 'asdf:load-op :hunchentoot-test)
