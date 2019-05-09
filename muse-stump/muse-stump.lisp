(in-package :muse-stump)

(defparameter *muse-page* "http://127.0.0.1:4007/")
(defparameter *play-url* "http://127.0.0.1:4007/play?source=/~{~a~^/~}")

(defun parse-html (template &rest components)
  "Given an url template and some url components,
 build the actual url and return a parsed object."
  (let* ((url (eval `(format nil ,template ,@components)))
         (request (if (not (null (search "http" url)))
                      (get url)    ; Handle http requests
                      (uiop:read-file-string url)))) ;handle local html files
    (parse request)))

(defun url-name (str)
  "Replace any spaces with pluses; string will be used in url address and requests"
  (substitute #\+ #\Space str))

(defun clean-name (str)
  "Replace any pluses with spaces; string will be used in html page content."
  (substitute #\Space #\+ str))

(defun muse-command (what)
  (get (concatenate 'string *muse-page* what)))

(defun muse-play (&rest what)
  (get (format nil *play-url*
               (mapcar #'url-name
                       what))))

(defun muse-info (what)
  (let ((node (parse-html "http://127.0.0.1:4007/home")))
    ($ node what (text))))

(defun muse-player-status ()
  (muse-info ".player-status"))

(defun muse-shuffle-status ()
  (aref (muse-info ".shuffle-status") 0))

(defun muse-playing-song ()
  (aref (muse-info ".playing-song") 0))

(defun muse-playing-song-lyrics ()
  (aref (muse-info ".playing-song-lyrics") 0))

(defun muse-playing? ()
  (equalp (muse-player-status) #("playing")))

(defun muse-artists-page ()
  (concatenate 'string *muse-page* "artists"))

(defun muse-all-songs-page ()
  (concatenate 'string *muse-page* "songs"))

(defun muse-artist-page (artist)
  (concatenate 'string *muse-page* "artist/" artist))

(defun muse-lyrics-page (lyrics)
  (concatenate 'string *muse-page*
               (format nil "lyrics/~a" (url-name lyrics))))

(defun muse-tags-page ()
  (concatenate 'string *muse-page* "tags"))

(defmacro parse-page (url query)
  "Parse muse url and return all elements matching query"
  `(map 'list #'identity
        (let ((node (parse-html ,url)))
          ($ node ,query (text)))))

(defun artists ()
  (parse-page (muse-artists-page) "a.artist"))

(defun lyrics-entries (lyrics)
  (parse-page (muse-lyrics-page lyrics) ".song"))

(defun all-songs ()
  "Parse /songs server page; filter songs without a url"
  (let ((songs
          (map 'list #'identity
               (let ((node (parse-html (muse-all-songs-page))))
                 ($ node "a.song"
                   (map (lambda (node) 
                          (append (list (text node)
                                        (attribute node "href"))))))))))
    (remove-if #'uiop:emptyp songs :key #'second)))

(defun tags ()
  (parse-page (muse-tags-page) "a.tag"))

(defun artist-info (artist query)
  "Every artist has it's own page. Helper function to get all artist info."
  (parse-page (muse-artist-page artist) query))

(defun artist-albums (artist)
  (artist-info artist "a.album"))

(defun artist-tags (artist)
  (artist-info artist "a.tag"))

(defun artist-songs (artist)
  (artist-info artist "a.song"))

(defun toggle-play ()
  (get "http://127.0.0.1:4007/play"))

(defmacro item-selection (list prompt)
  `(alexandria:if-let
       ((item                           ;unclean, but only use locally
         (select-from-menu
          (current-screen)
          ;; Stumpwm real-time searching/selection requires
          ;; the list is a list of pairs.
          (mapcar (lambda (a)
                    (list a a))
                  ,list)
          ,prompt)))
     (first item)))

(defun select-tag ()
  (item-selection (tags) "Tag:"))

(defun select-artist ()
  (let ((selection (item-selection (cons "__new artist__"
                                         (artists))
                                   "Artist:")))
    (if (string= selection "__new artist__")
        (read-one-line (current-screen) "Artist name: ")
        selection)))

(defun select-artist-album (artist)
  (item-selection (artist-albums artist) "Album:"))

(defun prepare-song-for-playing (string)
  "Transform 'artist - song | some lyrics' into 'artist/song'"
  (let* ((artist-song (first (split-sequence #\| string)))
         (artist (string-trim " " (first (split-sequence #\- artist-song))))
         (song (string-trim " " (lastcar (split-sequence #\- artist-song))))
         (artist-song-url-ready (mapcar #'url-name (list artist song))))
    (reduce (lambda (first second)
              (concatenate 'string first "/" second))
            artist-song-url-ready)))

(defun select-song-from-all-songs ()
  "Present a list of songs that have urls to the user, but without the
url and return the user selection together with the url."
  (let* ((songs (all-songs))
         (song
          (item-selection
           (mapcar #'first songs)
           "Song: ")))
    (prepare-song-for-playing song)))

(defun select-song-from-lyrics ()
  (let* ((lyrics (read-one-line (current-screen) "Lyrics: "))
         (song (item-selection
                (lyrics-entries lyrics) "Song: ")))
    (prepare-song-for-playing song)))

(defun select-what-to-play ()
  (item-selection '("artist" "similar" "song" "lyrics" "album" "tag" "list") "Play:"))

(defcommand what-is-playing () ()
  (if (muse-playing?)
      (message (format nil "~a  ~a" (muse-playing-song) (muse-shuffle-status)))
      (message (format nil "Player is stopped  ~a" (muse-shuffle-status)))))

(defcommand song-lyrics () ()
  (if (muse-playing?)
      (stumpwm::message-no-timeout
       (format nil "~a ~% ~a" (muse-playing-song) (muse-playing-song-lyrics)))
      (message (format nil "Player is stopped  ~a" (muse-shuffle-status)))))

(defcommand play () ()
  (if (muse-playing?)
      (toggle-play)
      (alexandria:if-let ((what-to-play (select-what-to-play)))
        (alexandria:switch (what-to-play :test #'string=)
          ("artist"
           (muse-play "artist" (select-artist)))
          ("similar"
           (muse-play "similar" (select-artist)))
          ("song"
           (muse-play "song" (select-song-from-all-songs)))
          ("lyrics"
           (muse-play "song" (select-song-from-lyrics)))
          ("album"
           (alexandria:if-let ((artist (select-artist)))
             (muse-play "artist" artist
                        "album" (select-artist-album
                                 artist))))
          ("tag"
           (muse-play "tag" (select-tag)))
          (T nil))))
  nil)

(defcommand continue-with-video () ()
  (muse-command "video"))

(defcommand next-song () ()
  (muse-command "next")
  nil)

(defcommand previous-song () ()
  (muse-command "previous")
  nil)

(defcommand toggle-shuffle () ()
  (muse-command "toggle-shuffle")
  (message (format nil "Shuffle status: ~a" (muse-shuffle-status))))

(defcommand stop-player () ()
  (muse-command "stop"))
