(in-package :player)

(defvar *mpvsocket* "/tmp/muse_mpv_socket")

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

(defun play-song (song)
  (declare (song song))
  (play (song-url song)))

(defun play (url &key (video nil))
  (run-program
   (if video
       (format nil "mpv --save-position-on-quit --input-ipc-server=~a ~A" *mpvsocket* url)
       (format nil "mpv --save-position-on-quit --vid=no --input-ipc-server=~a ~A" *mpvsocket* url))))

(defun continue-with-video ()
  "Close the current mpv session and reopen it with video support"
  (interrupt-thread
   *playing-thread*
   (lambda ()
     ;; save-position-on-quit also saves the --vid=no property
     (mpv-run "set_property" "vid" "yes")
     (quit)
     (play (song-url *playing-song*) :video T)))
  "all good"
  )

(defun previous-song ()
  )

(defun next-song ()
  (quit))

(defun mpv-command (args)
  (launch-program
   (eval
    `(format nil "echo '{\"command\": [\"~A\", \"~A\"]}' | socat - ~A" ,@args *mpvsocket*))))

(defun mpv-run (&rest args)
  (run-program
   (format nil "echo '{\"command\": [~{\"~a\"~^, ~}]}' | socat - ~A" args *mpvsocket*)))

(defun play-pause ()
  (mpv-command '("cycle" "pause")))

(defun seek (seconds)
  (mpv-command `("seek" ,seconds)))

(defun quit ()
  (mpv-command '("quit" 0)))

(defun kill-player()
  ;; Prevent reopening the player with a new song from list
  (destroy-thread *playing-thread*)
  (quit))

;; (quit)
;; (play-pause)
;; (seek 20)
;; (play (third (first (songs "Anathema"))))



;; (ql:quickload :muse)


