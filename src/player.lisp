(in-package :player)

(defvar *mpvsocket* "/tmp/muse_mpv_socket")
(defparameter *playing-thread* '())
(defparameter *playing-song* '())

(defun send-mpv-command (&rest args)
  (uiop:run-program
   (format nil "echo '{\"command\": [~{\"~a\"~^, ~}]}' | socat - ~A" args *mpvsocket*)))

(defun set-mpv-property (property value)
  (send-mpv-command "set_property" property value))

(defun quit ()
  (send-mpv-command "quit" 0))

(defun go-to-song-beginning ()
  "Set the playtime to 0:00 for the current running song.
Useful to use before quiting, since we're using the save-position-on-quit,
but we don't want that to happend when the command is next-song, for example "
  (set-mpv-property "percent-pos" "0"))

(defun continue-with-video ()
  "Continue from the point of where this song is, but with video support."
  (interrupt-thread
   *playing-thread*
   (lambda ()
     ;; save-position-on-quit also saves the --vid=no property
     (set-mpv-property "vid" "yes")
     (quit)
     (play (song-url *playing-song*) :video T))))

(defun previous-song ()
  (go-to-song-beginning)
  ;; to be done
  )

(defun next-song ()
  ;; We don't want the next play of this song to start from the middle of the song
  (go-to-song-beginning)
  (quit))

(defun play-pause ()
  (send-mpv-command "cycle" "pause"))

(defun seek (seconds)
  (send-mpv-command `("seek" ,seconds)))

(defun play-songs (lst)
  (print lst)
  (setf *playing-thread*
        (make-thread
         (lambda ()
           (loop for song in lst
                 do (setf *playing-song* song)
                    (play-song song))))))

(defun playing? ()
  (when *playing-thread*
    (thread-alive-p *playing-thread*)))

(defun what-is-playing ()
  *playing-song*)

(defun play-song (song)
  (declare (song song))
  (play (song-url song)))

(defun play (url &key (video nil))
  "Open the given url with mpv with audio only, by default or with video
if video is specified as T "
  (run-program
   (if video
       (format nil "mpv --save-position-on-quit --input-ipc-server=~a ~A" *mpvsocket* url)
       (format nil "mpv --save-position-on-quit --vid=no --input-ipc-server=~a ~A" *mpvsocket* url))))

(defun kill-player()
  "Prevents reopening the player with a new song from list"
  (when (playing?)
    (destroy-thread *playing-thread*)
    (go-to-song-beginning)
    (quit)))
