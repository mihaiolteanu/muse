(in-package :parser)

(defvar *root-path* (system-relative-pathname :muse ""))

(defvar *lastfm-artist-page* "https://www.last.fm/music/~A")
(defvar *test-artist-page*   (namestring (merge-pathnames "test_pages/music/~A/main.html" *root-path*)))
(defvar *artist-page*        *lastfm-artist-page*)

(defvar *lastfm-album-page*  "https://www.last.fm/~A")
(defvar *test-album-page*    (namestring (merge-pathnames "test_pages/music/~A.html" *root-path*)))
(defvar *album-page*         *lastfm-album-page*)

(defvar *lastfm-user-loved-url* "https://www.last.fm/user/~A/loved")

(defun parse-html (template &rest components)
  "Give an url template and some url components, build the actual url and returned a parsed object."
  (let* ((url (eval `(format nil ,template ,@components)))
         (request (if (not (null (search "http" url)))
                      (get url)    ; Handle http requests
                      (uiop:read-file-string url)))) ;handle local html files
    (parse request)))

(defun user-loved-tracks (user)
  (let ((node (parse-html user)))
    ($ node "section#user-loved-tracks-section td.chartlist-play a"
      (map (lambda (node)
           (list (attribute node "data-artist-name")
                 (attribute node "data-track-name")
                 (attribute node "href")))))))

(defun album-page (artist album)
  "Given an artist name and an album name, return a url with info for such an artist and album"
  (format nil *album-page*
          (format nil "~A/~A" artist
                  (substitute #\+ #\Space album))))

(defun trim-whitespace (str)
  (string-trim '(#\Space #\Tab #\Newline) str))

(defun album-tracks (url)
  (let* ((node (parse-html url))
         (release-date ($ node "ul.metadata-list p.metadata-display" (text)))
         (tracks ($ node "section#tracks-section tr"
                   (map (lambda (node)
                          (concatenate 'list ;(song-title duration link)
                                       ($ node "td.chartlist-name a.link-block-target" (text))
                                       (map 'vector #'trim-whitespace
                                            ($ node "td.chartlist-duration" (text)))
                                       ($ node "td.chartlist-play a" (attr :href))))))))
    (list (lastcar (split-sequence #\Space (aref release-date 0)))
          (remove-if #'null tracks))))

(with-local-htmls
  (album-tracks (album-page "Pendragon" "Pure")))

(defun artist-data (artist)
  (let ((node (parse-html *artist-page* artist)))
    (list ($ node "div.col-main li.tag" (text))
          ($ node "section div ol a.link-block-target" ;; Build a list of tracks for each of the albums
            (map (lambda (node) 
                   (append (list (text node)) ;album name
                           (album-tracks (album-page artist (text node))))))))))

(defmacro with-local-htmls (&body body)
  `(let ((parser::*artist-page* parser::*test-artist-page*)
         (parser::*album-page* parser::*test-album-page*))
     ,@body))


(with-local-htmls
  (album-tracks (album-page "Pendragon" "Pure")))

(with-local-htmls
  (album-tracks (album-page "Lost+in+Kiev" "Nuit+Noire")))

(with-local-htmls
  (artist-data "Pendragon"))

(with-local-htmls
  (let* ((parsed (parse-html *test-artist-page* "Pendragon")))
    ($ parsed "div.col-main li.tag" (text))))

(defparameter *nuit-noire*
  (with-local-htmls
  (make-instance
   'album
   :name "Nuit Noire"
   :songs (mapcar (lambda (song)
                    (make-instance
                     'song
                     :name (first song)
                     :duration (second song)
                     :url (third song)))
                  (album-tracks (album-page "Lost+in+Kiev" "Nuit+Noire"))))))

(defclass artist ()
  ((name
   :initarg :name
   :accessor artist-name)
  (genres
   :initarg :genres
   :accessor artist-genres)
  (albums
   :initarg :albums
   :accessor artist-albums)))

(defclass genre ()
  ((name
    :initarg :name
    :accessor genre-name)))

(defclass song ()
  ((name
    :initarg :name
    :accessor song-name)
   (url
    :initarg :url
    :initform nil
    :accessor song-url)
   (duration
    :initarg :duration
    :accessor song-duration)
   (lyrics
    :initarg :lyrics
    :initform nil
    :accessor song-lyrics)))

(defclass album ()
  ((name
    :initarg :name
    :accessor album-name)
   (songs
    :initarg :songs
    :accessor album-songs)))

(defmethod print-object ((obj album) stream)
  (print-unreadable-object (obj stream :type nil)
    (format stream "~A, ~A" (album-name obj) (album-songs obj))))

(defmethod print-object ((obj song) stream)
  (print-unreadable-object (obj stream :type nil)
    (format stream "~A, ~A, ~A" (song-name obj) (song-url obj) (song-duration obj))))

*nuit-noire*








;; (defparameter *metalstorm-url* "http://www.metalstorm.net/bands/index.php?b_where=s.style&b_what=Doom&prefix=Funeral")
;; (defparameter *metalstorm-bands-genre-url*
;;   "http://www.metalstorm.net/bands/index.php?b_where=s.style&b_what=~A&prefix=~A")
;; (defparameter *metalstorm-band-page* "http://www.metalstorm.net/bands/~A")
;; (defparameter *metalstorm-album-tracks* "http://www.metalstorm.net/bands/~A")

;; (defun metalstorm-bands (prefix genre &optional (count 10))
;;   "i.e. Funeral doom should be given with prefix=funeral and genre=doom"
;;   (let* ((url (format nil *metalstorm-bands-genre-url* genre prefix))
;;          (request (dex:get url))
;;          (parsed (plump:parse request))
;;          (bands (lquery:$ parsed "table.table td b a"
;;                   (map (lambda (node)
;;                          (list (plump:text node)
;;                                (plump:attribute node "href")))))))
    
;;     (or (and (>= (length bands) count)
;;              (subseq bands 0 count))
;;         bands)))

;; (defun band-discography (band-link)
;;   (let* ((url (format nil *metalstorm-band-page* band-link))
;;          (request (dex:get url))
;;          (parsed (plump:parse request))
;;          (discs (lquery:$ parsed "#discotab1 td a[href^=album]"
;;                   (map (lambda (node)
;;                          (list (plump:text node)
;;                                (plump:attribute node "href")))))))
;;     discs))

;; (defun album-tracks (album-link)
;;   (let* ((url (format nil *metalstorm-album-tracks* album-link))
;;          (request (dex:get url))
;;          (parsed (plump:parse request)))
;;     parsed))

;; ;;;; Metalstorm example usage
;; (defparameter *request* (dex:get *metalstorm-url*))
;; (defparameter *parsed* (plump:parse *request*))
;; (defparameter *bands+band-link*
;;   (lquery:$ *parsed* "table.table td b a"
;;     (map (lambda (node)
;;            (list (plump:text node)
;;                  (plump:attribute node "href"))))))

;; (and (> (length *bands+band-link*)
;;         10)
;;      (subseq  *bands+band-link* 0 10))


;; (defparameter *genius-lyrics-url* "https://genius.com/~A-lyrics")
;; (defun genius-lyrics (artist-song)
;;   (with-lqueryed-url *genius-lyrics-url* (artist-song)
;;       "div.lyrics p"
;;     (text)))
;; (defparameter *anathema-one-last-goodbye-lyrics* (genius-lyrics "anathema-one-last-goodbye"))

;; (aref *anathema-one-last-goodbye-lyrics* 0)


;; (defparameter *genius-slow-mort*
;;   (let* ((url "https://genius.com/Slow-mort-lyrics" )
;;          (request (dex:get url))
;;          (parsed (plump:parse request)))
;;     parsed))

;; (lquery:$ *genius-slow-mort* "[initial-content-for=\"song_body\"]" (text))
;; (lquery:$ *genius-slow-mort* "div.lyrics p" (text))

