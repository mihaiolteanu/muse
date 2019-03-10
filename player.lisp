(in-package :player)

(defun play-top-artist (artist)
  (multiple-value-bind (similar genres top-tracks wiki)
      (artist-data artist)
    (let ((artist-tracks (map 'list (lambda (pl)
                                      (concatenate 'string "anathema " pl))
                              top-tracks)))
      (dolist (song artist-tracks)
        (uiop:run-program
         (format nil "mpv ytdl://ytsearch:\"~A\"" song))))))

