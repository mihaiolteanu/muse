(in-package :player)

(defvar *mpvsocket* "/tmp/mpvsocket")
(defparameter *playing-thread* '())
(defparameter *timeout-checking-thread* '())
(defparameter *playing-list* '())
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
  (mpv-command "quit" 0)
  (destroy-thread *timeout-checking-thread*)
  (destroy-thread *playing-thread*))

(defun remove-nil-urls (songs)
  (remove-if (lambda (song)
               (null (song-url song)))
             songs))

(defparameter *playlist-lock* (make-lock))
(defparameter *playlist-check-update* (make-condition-variable))

(defun start-timeout-checking ()
  (setf *timeout-checking-thread*
        (make-thread
         (lambda ()
           (loop (sleep +buffer-check-timeout+)
                 (condition-notify *playlist-check-update*))))))

(defun random-playable-object (objs)
  "Get a random entry from the objs list, making sure it can be used
with no further complications by the player."
  (labels ((nth-random ()
             (nth (random (length objs))
                  objs)))
    (match (first objs)
      ;; Go for a random artist
      ((objects::artist)       
       (nth-random))

      ;; Only return a song object with playable url (not disable by the user, etc)
      ((objects::song)
       (do ((selected (nth-random) (nth-random)))
           ((available-youtube-video? (song-url selected)) selected))))))

(defun play-single-artist (artist)
  (play-songs (artist-songs artist)))

(defun play-artist-album (artist album)
  (play-songs
   (album-songs
    (find album (artist-albums artist)
          :key #'album-name
          :test #'string-equal))))

(defun play-artists (artists)
  (let* ((random-artist (artist-from-db (artist-name (random-playable-object artists))))
         (random-song (random-playable-object (artist-songs random-artist))))
    (setf *playing-songs* (append *playing-songs* (list random-song)))
    (start-mpv (song-url random-song)))
  (setf *playing-thread*
        (make-thread
         (lambda ()
           (with-lock-held (*playlist-lock*)
             (loop (condition-wait *playlist-check-update* *playlist-lock*)
                   (unless (enough-songs-in-playlist?)
                     (let* ((random-artist (artist-from-db
                                            (artist-name (random-playable-object artists))))
                            (random-song (random-playable-object (artist-songs random-artist))))
                       (setf *playing-songs* (append *playing-songs* (list random-song)))
                       (append-to-playlist (song-url random-song)))))))))
  (start-timeout-checking))

(defun first-playable-songs (songs)
  "Returns a list where the first song is playable"
  (do ((ret songs (rest ret)))
      ((or (null ret)
           (available-youtube-video? (song-url (first ret))))
       ret)))

(defmacro choose-song (song-url-action)
  "Choose a song based on shuffle, save it as the current playing song
 and then decide what to do with the chosen song's url (play it, return it, etc.)"
  `(if *shuffle-play*
       (let ((chosen-song (random-playable-object songs)))
         (setf *playing-songs* (append *playing-songs* (list chosen-song)))
         (funcall ,song-url-action (song-url chosen-song)))
       (progn
         (let* ((playable-songs (first-playable-songs songs))
                (chosen-song (first playable-songs)))
           (setf songs (rest playable-songs))
           (setf *playing-songs* (append *playing-songs* (list chosen-song)))
           (funcall ,song-url-action (song-url chosen-song))))))

(defun play-songs (raw-songs)  
  (let* ((songs (remove-nil-urls raw-songs))
         (first-song (choose-song #'identity))
         (second-song (choose-song #'identity))
         (third-song (choose-song #'identity)))
    (start-mpv first-song second-song third-song)
    (setf *playing-thread*
          (make-thread
           (lambda ()
             (with-lock-held (*playlist-lock*)
               (loop (condition-wait *playlist-check-update* *playlist-lock*)
                     (unless (enough-songs-in-playlist?)
                       (choose-song #'append-to-playlist)))))))
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

(defun what-is-playing ()
  (nth (playlist-position)
       *playing-songs*))

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
