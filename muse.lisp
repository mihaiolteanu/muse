(in-package :muse)

(defparameter *my-acceptor* (make-instance 'easy-acceptor :port 4007))
(start *my-acceptor*)

(defmacro with-html (&body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t)
     ,@body))

(defparameter *foo* 0)
(defparameter *bar* 0)

(defun parameter-test ()
  (setf (content-type*)
        (format nil "text/html; charset=~A" :iso-8859-1))
  (let ((foo (get-parameter "foo"))
        (bar (get-parameter "bar")))
    (setf *foo* foo)
    (setf *bar* bar)
    (format nil "Here is ~a" foo)))


;; (with-output-to-string (out)
;;   (with-html-output (out)
;;     (loop for (url . song) in '(("https://www.youtube.com/watch?v=aiw7ha2Yziw" . "Paranova")
;;                                 ("https://www.youtube.com/watch?v=0NTs2g0hVt8" . "Wintersun"))
;;           do (htm (:a :href url
;;                       (:b (str )))))))


(defun s-artists ()
  (with-html-output-to-string (s)
    (:html
     (:body
      (:h2 "Available Artists")
      (do ((el (artists) (rest el)))
          ((null el))
        (htm (:p (str (first (first el))))))
      ))))

(defun s-artist-songs ()
  (let ((artist (get-parameter "artist")))
    (with-html-output-to-string (s)
      (:html
       (:body
        (:h2 (str (format nil "~a songs" artist)))
        (loop for (ignore album name url duration) in (songs artist)
              do (htm (:a :href url
                          (:b (str name)))
                      (str (format nil " [~a]" duration))
                      (:hr))
              )
        ;; (do ((el (songs artist) (rest el)))
        ;;     ((null el))
        ;;   (htm (:p (str (second el)))))
        ))))
  )


(setq *dispatch-table*
      (mapcar (lambda (args)
                (apply 'create-prefix-dispatcher args))
              '(("/test.html" parameter-test)
                ("/artists" s-artists)
                ("/songs" s-artist-songs))))


(hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 4123))
(asdf:oos 'asdf:load-op :hunchentoot-test)
