(in-package :server)

(defparameter *my-acceptor* (make-instance 'easy-acceptor :port 4007))
(start *my-acceptor*)

(defmacro with-html (&body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t)
     ,@body))

(defun artist-from-uri ()
  (substitute #\Space #\+
              (first (last (cl-utilities:split-sequence #\/ (request-uri*))))))

(defun genre-from-uri ()
  (first (last (cl-utilities:split-sequence #\/ (request-uri*)))))

(defun s-artists ()
  (with-html-output-to-string (s)
    (:html
     (:body
      (:h2 "Available Artists")
      (do ((el (artists) (rest el)))
          ((null el))
        (let ((artist (first (first el))))
          (htm (:p (:a :href (format nil "/artist/~a" (substitute #\+ #\Space artist))
                       (str (first (first el))))))))
      ))))

(defmacro display-songs (source)
  "Template for html songs format"
  `(loop for (ignore album name url duration) in ,source
         do (htm (:p (:a :href url
                         :class (str "song")
                         (str name))
                     (str (format nil " [~a]" duration))))))

(defun s-artist-songs ()
  (let ((artist (artist-from-uri)))
    (with-html-output-to-string (s)
      (:html
       (:body
        (:h2 (str (format nil "~a songs" artist)))
        (display-songs (songs artist)))))))

(defun s-genres ()
  (with-html-output-to-string (s)
    (:html
     (:body
      (:h2 "Available Genres")
      (loop for (g) in (all-genres)
            do (htm (:p (:a :href (format nil "/genre/~a" g)
                            (str g)))))))))

(defun s-genre-songs ()
  (let ((genre (genre-from-uri)))
    (with-html-output-to-string (s)
      (:html
       (:body
        (:h2 (str (format nil "~a songs" genre)))
        (display-songs (all-genre-songs genre)))))))

(defun s-artist-similar ()
  (let ((artist (artist-from-uri)))
    (with-html-output-to-string (s)
      (:html
       (:body
        (:h2 (str (format nil "~a similar artists" artist)))
        (loop for (a) in (similar artist)
              do (htm (:p
                       (:a :href (format nil "/artist/~a" (substitute #\+ #\Space a))
                           (str a))))))))))

(setq *dispatch-table*
      (nconc (list
              (create-regex-dispatcher "^/artist/[a-zA-Z0-9 ]+$" 's-artist-songs)
              (create-regex-dispatcher "^/genre/[a-zA-Z0-9 ]+$" 's-genre-songs)
              (create-regex-dispatcher "^/similar/[a-zA-Z0-9 ]+$" 's-artist-similar))
             (mapcar (lambda (args)
                       (apply 'create-prefix-dispatcher args))
                     '(("/test.html" parameter-test)
                       ("/artists" s-artists)
                       ("/genres" s-genres)))))


;; (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 4123))
;; (asdf:oos 'asdf:load-op :hunchentoot-test)
