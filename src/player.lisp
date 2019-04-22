(in-package :player)

(defvar *mpvsocket* "/tmp/mpvsocket")
(defparameter *playing-thread* '())
(defparameter *timeout-checking-thread* '())
(defparameter *playing-list* '())
(defparameter *shuffle-play* nil
  "If true, the next song is chosen at random.")
(defconstant +playlist-buffer+ 2
  "Number of songs availalble in the playlist after the current one")
(defconstant +buffer-check-timeout+ 15
  "Number of seconds before checking the playlist buffer and adding
new songs if buffer too small.")
(defparameter *previous-song* '())
(defparameter *playing-song* '())

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
           "mpv -ytdl-format=best --vid=no --input-ipc-server=~a ~{~a ~}"
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

(defun random-object (objs)
  (nth (random (length objs))
       objs))

(defun play-single-artist (artist)
  (play-songs (artist-songs artist)))

(defun play-artist-album (artist album)
  (play-songs
   (album-songs
    (find album (artist-albums artist)
          :key #'album-name
          :test #'string-equal))))

(defun play-artists (artists)
  (let* ((random-artist (artist-from-db (random-object artists)))
         (random-song (random-object (artist-songs random-artist))))
    (start-mpv (song-url random-song)))
  (setf *playing-thread*
        (make-thread
         (lambda ()
           (with-lock-held (*playlist-lock*)
             (loop (condition-wait *playlist-check-update* *playlist-lock*)
                   (unless (enough-songs-in-playlist?)
                     (let* ((random-artist (artist-from-db (random-object artists)))
                            (random-song (random-object (artist-songs random-artist))))
                       (append-to-playlist (song-url random-song)))))))))
  (start-timeout-checking))

(defun play-songs (songs)
  (let ((song-urls (mapcar #'song-url
                            (remove-nil-urls songs))))
    (start-mpv (if *shuffle-play*
                   (random-object song-urls)
                   (progn
                     (first song-urls)
                     (setf song-urls (rest song-urls)))))
    (setf *playing-thread*
          (make-thread
           (lambda ()
             (with-lock-held (*playlist-lock*)
               (loop (condition-wait *playlist-check-update* *playlist-lock*)
                     (unless (enough-songs-in-playlist?)
                       (if *shuffle-play*
                           (append-to-playlist
                            (random-object song-urls))
                           (progn
                             (append-to-playlist (first song-urls))
                             (setf song-urls (rest song-urls))))))))))
    (start-timeout-checking)))

(defun play (what)
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
  *playing-song*)

(defun playground ()
  (start-mpv "https://www.youtube.com/watch?v=NmyWeOvF_Sg"
             "https://www.youtube.com/watch?v=XFo332Y5uIA")

  (quit-mpv)
  (playlist-count)
  (playlist-position)
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
  (play '("artist" "Queen"))
  (play '("artist" "Queen" "album" "Jazz"))
  )
