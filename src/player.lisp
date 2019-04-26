(in-package :player)

(defvar *mpvsocket* "/tmp/mpvsocket")
(defparameter *playing-thread* '())
(defparameter *timeout-checking-thread* '())
(defparameter *shuffle-play* nil
  "If true, the next song is chosen at random.")
(defconstant +playlist-buffer+ 2
  "Number of songs availalble in the playlist after the current one")
(defconstant +buffer-check-timeout+ 60
  "Number of seconds before checking the playlist buffer and adding
new songs if buffer too small.")
(defparameter *playing-songs* '())

(defun mpv-command (&rest args)
  (parse
   (with-output-to-string (out)
     (uiop:run-program
      (format nil "echo '{\"command\": [~{\"~a\"~^, ~}]}' | socat - ~A"
              args *mpvsocket*)
      :output out))))

(defun set-mpv-property (property value)
  (mpv-command "set_property" property value))

(defun get-mpv-property (property)
  (gethash "data" (mpv-command "get_property" property)))

(defun play-pause ()
  "Toggle playing status"
  (mpv-command "cycle" "pause"))
(defun pause ()
  "Make sure the player is paused"
  (unless (get-mpv-property "pause")
    (play-pause)))
(defun next-song ()
  (condition-notify *playlist-check-update*)
  (mpv-command "playlist-next"))

(defun prev-song ()
  (mpv-command "playlist-prev"))

(defun replay-song ()
  (set-mpv-property "percent-pos" 0))

(defun forward-song (seconds)
  (mpv-command "seek" seconds))

(defun playlist-position ()
  (get-mpv-property "playlist-pos"))

(defun playlist-count ()
  (get-mpv-property "playlist-count"))

(defun enough-songs-in-playlist? ()
  (handler-case
      (> (- (playlist-count) (playlist-position))
         +playlist-buffer+)
    (subprocess-error ()                ;Maybe mpv has not started yet
      nil)))

(defun song-position ()
  (get-mpv-property "time-pos"))

(defun playlist-urls ()
  (map 'list
       (lambda (e)
         (gethash "filename" e))
       (get-mpv-property "playlist")))

(defun playing-song-url ()
  (nth (playlist-position)
       (playlist-urls)))

(defun append-to-playlist (url)
  (mpv-command "loadfile" url "append"))

(defun open-playing-song-in-browser ()
  (run-program `("xdg-open" ,(playing-song-url)))
  (pause))

(defun start-mpv (&rest urls)
  (launch-program
    (format nil
            "mpv --log-file=/home/mihai/quicklisp/local-projects/muse/mpvlog --msg-level=all=trace -ytdl-format=best --vid=no --input-ipc-server=~a ~{~a ~}"
            *mpvsocket* urls)))

(defun quit-mpv ()
  (mpv-command "quit" 0))

(defun cleanup ()
  (setf *playing-songs* '())
  (when (thread-alive-p *timeout-checking-thread*)
    (destroy-thread *timeout-checking-thread*))
  (when (thread-alive-p *playing-thread*)
    (destroy-thread *playing-thread*)))

(defun quit-mpv-and-cleanup ()
  (quit-mpv)
  (cleanup))

(defun remove-nil-urls (songs)
  (remove-if (lambda (song)
               (string= (song-url song) "n/a"))
             songs))

(defparameter *playlist-lock* (make-lock))
(defparameter *playlist-check-update* (make-condition-variable))

(defun start-timeout-checking ()
  (setf *timeout-checking-thread*
        (make-thread
         (lambda ()
           (loop (sleep *buffer-check-timeout*)
                 (condition-notify *playlist-check-update*)))
         :name "mpv-timeout-checking")))

(defun random-item (list)
  (nth (random (length list))
       list))

(defun random-artist (artists)
  "Return a complete artist object from db. If the list of artists was
obtained from a artist-similar slot, the artist doesn't have all the
available info (albums, songs, etc.) since that would mean returning
too much info from db at once. Instead, request it only when needed."
  (artist-from-db (artist-name (random-item artists))))

(defun random-song (songs)
  "Return a random, playable(i.e. available) url."
  (do ((song (random-item songs)
             (random-item songs)))
      ((available-youtube-video? (song-url song))
       song)))

(defun play-single-artist (artist)
  (play-songs (artist-songs artist)))

(defun play-artist-album (artist album)
  (play-songs
   (album-songs
    (find album (artist-albums artist)
          :key #'album-name
          :test #'string-equal))))

(defun add-to-playing-list (songs)
  "Now is the perfect time to also download and save the song lyrics
to the db if they're not already there. "
  (mapcar (lambda (s)
            (when (emptyp (song-lyrics s))
              (if-let ((lyrics (song-lyrics-from-web (song-artist-name s)
                                                     (song-name s))))
                (save-song-lyrics lyrics (song-url s))
                (setf (song-lyrics s) lyrics)))
            (setf *playing-songs*
                  (append *playing-songs* (list s))))
          songs))

(defun add-songs-to-player (startup &rest songs)
  (if startup
      (apply #'start-mpv (mapcar #'song-url songs))
      (apply #'append-to-playlist (mapcar #'song-url songs)))
  (add-to-playing-list songs))

(defun play-artists (artists)
  (let* ((artist (random-artist artists))
         (song (random-song (artist-songs artist))))
    (add-songs-to-player T song))
  (setf *playing-thread*
        (make-thread
         (lambda ()
           (with-lock-held (*playlist-lock*)
             (loop (condition-wait *playlist-check-update* *playlist-lock*)
                   (unless (enough-songs-in-playlist?)
                     (let* ((artist (random-artist artists))
                            (song (random-song (artist-songs artist))))
                       (add-songs-to-player nil song))))))
         :name "mpv-playing-thread"))
  (start-timeout-checking))

(defun first-playable-songs (songs)
  "Returns a list where the first song is playable"
  (do ((ret songs (rest ret)))
      ((or (null ret)
           (available-youtube-video? (song-url (first ret))))
       ret)))

(defmacro choose-song (&key (startup nil))
  "Choose a song based on the shuffle parameter. If we're just
starting up, return the chosen song as is, otherwise add it to the
playing list. Songs is defined in the calling function "
  `(let ((song '()))
     (if *shuffle-play*
         (setf song (random-song songs))
         (let* ((playable-songs (first-playable-songs songs))
                (first-song (first playable-songs)))
           (setf songs (rest playable-songs))
           (setf song first-song)))
     song))

(defun play-songs (raw-songs)
  (let* ((songs (remove-nil-urls raw-songs))
         (song1 (choose-song :startup T))
         (song2 (choose-song :startup T))
         (song3 (choose-song :startup T)))
    (add-songs-to-player T song1 song2 song3)
    (setf *playing-thread*
          (make-thread
           (lambda ()
             (with-lock-held (*playlist-lock*)
               (loop (condition-wait *playlist-check-update* *playlist-lock*)
                     (unless (enough-songs-in-playlist?)
                       (let ((song (choose-song)))
                         ;; Stop the thread if no more songs available to be played.
                         (if song
                             (add-songs-to-player nil song)
                             (return nil)))
                       ))))
           :name "mpv-playing-thread"))
    (start-timeout-checking)))

(defun play (what shuffle-play)
  (setf *shuffle-play* shuffle-play)
  (trivia:match what
    ((list "artist" artist)
     (play-single-artist (artist-from-db artist)))
    ((list "similar" artist)
     (play-artists (artist-similar (artist-from-db artist))))
    ((list "tag" tag)
     (play-artists (all-genre-artists tag)))
    ((list "artist" artist "album" album)
     (play-artist-album (artist-from-db artist) album))))

(defun playing? ()
  (when *playing-thread*
    (thread-alive-p *playing-thread*)))

(defun playing-song ()
  (nth (playlist-position)
       *playing-songs*))

(defun playing-songs ()
  "Songs currently in playlist, past, present and future.
One of them is the currently playing song."
  *playing-songs*)

(defun playground ()
  (start-mpv "https://www.youtube.com/watch?v=NmyWeOvF_Sg"
             "https://www.youtube.com/watch?v=XFo332Y5uIA")
  (what-is-playing)
  (quit-mpv)
  (playlist-count)
  (playlist-position)
  (condition-notify *playlist-check-update*)
  
  (enough-songs-in-playlist?)
  (append-to-playlist "https://www.youtube.com/watch?v=NmyWeOvF_Sg")
  (prev-song)
  (next-song)
  (prev-song)
  (play-pause)
  (pause)
  (open-playing-song-in-browser)
  (playing-song-url)
  (setf *shuffle-play* t)
  (play '("artist" "Aerosmith"))
  (play '("artist" "Queen"))
  (play '("artist" "Queen" "album" "Jazz"))
  )
