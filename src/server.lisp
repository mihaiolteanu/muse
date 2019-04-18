(in-package :server)

(defparameter *port* 4007)
(load #P"~/muserc.lisp" :if-does-not-exist nil)

(defparameter *pause-button* "⏸")
(defparameter *play-button* "▶")
(defparameter *play-pause-button* *play-button*)

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

(defun tag-from-uri ()
  (clean-name
   (first (last (cl-utilities:split-sequence #\/ (request-uri*))))))

(defun artist-and-album-from-uri ()
  (let ((seq (cl-utilities:split-sequence #\/ (request-uri*))))
    (values (third seq)
            (fifth seq))))

(defmacro display-songs (lst &key (with-artist nil))
  "We don't need to display the artist name all the time for
evey song on the page, since that info can be inferred."
  `(loop for song in ,lst
         do (htm (:a :href (song-url song)
                     :class (str "song")
                     (str
                      ,(if with-artist
                           `(format nil  "~a - ~a "
                                    (song-artist-name song)
                                    (song-name song))
                           `(format nil "~a" (song-name song)))))
                 (str (format nil "[~a]" (song-duration song)))
                 (:br))))

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
                :href (format nil "/artist/~a/album/~a" (url-name artist) (url-name name))
                (str name))))))

(defmacro standard-page (&body body)
  `(with-html-output-to-string (s)
     (:html
      (:head
       (:link :type "text/css"
              :rel "stylesheet"
              :href "/css/muse.css"))
      (:body
       (:p :class "play-buttons"
           (:a :href (conc "/stop?source-uri=" (request-uri*)) "⏹")
           (:a :href (conc "/previous?source-uri=" (request-uri*)) "⏪")
           (:a :href (conc "/play-pause?source-uri=" (request-uri*))
               (str *play-pause-button*))
           (:a :href (conc "/next?source-uri=" (request-uri*)) "⏩"))
       (:p :class "menu-bar"
           (:a :href "/home" "home")
           (:a :href "/artists" "artists")
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
             (display-songs (list (what-is-playing)) :with-artist t)))
        (htm
         (:div :class "player-status" (str "stopped"))))))

(defun s-artists ()
  (standard-page
    (:h2 "Available Artists")
    (display-artists (artists))))

(defun s-tags ()
  (standard-page
    (:h2 "Available Tags")
    (display-tags (all-genres))))

(defun s-tag-artists ()
  (let ((tag (tag-from-uri)))
    (standard-page
      (:h2 (str (format nil "Artists with the ~a tag" tag)))
      (display-artists (genre-artists tag)))))

(defun s-artist-info ()
  (let ((artist (artist-from-uri)))
    (standard-page
      (:h2 (str artist))
      (display-tags (genres artist))
      (:h2 (str "similar artists"))
      (display-similar (similar artist))
      (:h2 (str "albums"))
      (display-albums (albums artist))
      (:h2 (str "songs"))
      (display-songs (songs artist)))))

(defun s-artist-album ()
  (multiple-value-bind (artist album)
      (artist-and-album-from-uri)
    (setf artist (clean-name artist))
    (setf album (clean-name album))
    (standard-page
      (:h2 (str (format nil "~a - ~a (album)" album artist)))
      (let ((alb (find album (albums artist)
                       :key #'album-name
                       :test #'string=)))
        (when alb
          (display-songs (album-songs alb)))))))

(defun redirect-to-source ()
  "Only redirect back to calling source, when such a source was given"
  (let ((source-uri (get-parameter "source-uri")))
    (when source-uri
      (redirect (url-name source-uri)))))

(defun toggle-play-pause ()
  (if (string= *play-pause-button* *play-button*)
      (setf *play-pause-button* *pause-button*)
      (setf *play-pause-button* *play-button*))
  (play-pause))

(defun split-play-parameter (param)
  ;; First element after splitting will be an empty string; skip it
  (rest (uiop:split-string
         param
         :separator "/")))

(defun s-play-pause ()
  (if (playing?)
      (toggle-play-pause)
      (play (split-play-parameter (get-parameter "source-uri"))))
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

(defparameter *base-directory*
  (make-pathname
   :name nil
   :type nil
   :defaults #.(or *compile-file-truename* *load-truename*)))

(setq *dispatch-table*
      (nconc (list              
              (create-regex-dispatcher "^/artist/[a-zA-Z0-9 ]+$" 's-artist-info)
              (create-regex-dispatcher
               "^/artist/[a-zA-Z0-9 ]+/album/[a-zA-Z0-9 ]+$" 's-artist-album)
              (create-regex-dispatcher "^/tag/[a-zA-Z0-9 ]+$" 's-tag-artists)
              (create-folder-dispatcher-and-handler "/img/"
               (merge-pathnames (make-pathname :directory '(:relative "img"))
                                *base-directory*))
              (create-folder-dispatcher-and-handler "/css/"
               (merge-pathnames (make-pathname :directory '(:relative "css"))
                                *base-directory*)))
             (mapcar (lambda (args)
                       (apply 'create-prefix-dispatcher args))
                     '(("/home" server-status)
                       ("/video" s-continue-with-video)
                       ("/stop" s-stop)
                       ("/previous" s-previous)
                       ("/play-pause" s-play-pause)
                       ("/next" s-next)
                       ("/artists" s-artists)
                       ("/tags" s-tags)
                       ("/genres" s-genres)))))

;; (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 4123))
;; (asdf:oos 'asdf:load-op :hunchentoot-test)
