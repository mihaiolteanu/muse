(in-package :player)

(defvar *mpvsocket* "/tmp/muse_mpv_socket")

(defun play (url)
  (run-program
   (format nil "mpv --input-ipc-server=~a ~A" *mpvsocket* url)))

(defun mpv-command (args)
  (launch-program
   (eval
    `(format nil "echo '{\"command\": [\"~A\", \"~A\"]}' | socat - ~A" ,@args *mpvsocket*))))

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


