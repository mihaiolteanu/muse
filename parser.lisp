(in-package :parser)

(defvar *root-path* (system-relative-pathname :muse ""))

;; Parameters. For each set, both the real url (first parameter) and
;; local test pages path (second parameter) is specified. By default,
;; the real deal is used by the app (third parameter), but this should
;; be overwritten for test pourposes by the second parameter.

(defvar *lastfm-artist-page* "https://www.last.fm/music/~A")
(defvar *test-artist-page*   (namestring (merge-pathnames "test_pages/music/~A/main.html" *root-path*)))
(defvar *artist-page*        *lastfm-artist-page*)

(defvar *lastfm-artist-similar* "https://www.last.fm/music/~A/+similar")
(defvar *test-artist-similar* (namestring (merge-pathnames "test_pages/music/~A/similar.html" *root-path*)))
(defvar *artist-similar* *lastfm-artist-similar*)

(defvar *lastfm-album-page*  "https://www.last.fm/music/~A")
(defvar *test-album-page*    (namestring (merge-pathnames "test_pages/music/~A.html" *root-path*)))
(defvar *album-page*         *lastfm-album-page*)

(defvar *lastfm-user-loved-url* "https://www.last.fm/user/~A/loved")

(defun parse-html (template &rest components)
  "Given an url template and some url components,
 build the actual url and return a parsed object."
  (let* ((url (eval `(format nil ,template ,@components)))
         (request (if (not (null (search "http" url)))
                      (get url)    ; Handle http requests
                      (uiop:read-file-string url)))) ;handle local html files
    (parse request)))

(defun album-page (artist album)
  "Given an artist name and an album name, return a url
 with info for such an artist and album"
  (format nil *album-page*
          (format nil "~A/~A" artist
                  (substitute #\+ #\Space album))))

(defun trim-whitespace (str)
  (string-trim '(#\Space #\Tab #\Newline) str))

(defun user-loved-tracks (user)
  (let ((node (parse-html user)))
    ($ node "section#user-loved-tracks-section td.chartlist-play a"
      (map (lambda (node)
           (list (attribute node "data-artist-name")
                 (attribute node "data-track-name")
                 (attribute node "href")))))))

(defun album-info (artist album)
  (let* ((node (parse-html (album-page artist album)))
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

(defun biography (artist)
  (let* ((artist (substitute #\+ #\Space artist))
         (main-node (parse-html *artist-page* artist)) ;artist main page
         (similar-node (parse-html *artist-similar* artist))) ;similar artist page
    (list
     ($ similar-node "h3.big-artist-list-title" (text))
     ($ main-node "div.col-main li.tag" (text)) ;tags (genres)
     ($ main-node "section div ol a.link-block-target" ;; Build a list of tracks for each of the albums
       (map (lambda (node) 
              (append (list (text node)) ;album name
                      (album-info artist (text node)))))))))

(with-local-htmls
  (biography "Pendragon"))

(defmacro with-local-htmls (&body body)
  `(let ((parser::*artist-page* parser::*test-artist-page*)
         (parser::*album-page* parser::*test-album-page*)
         (parser::*artist-similar* parser::*test-artist-similar))
     ,@body))

(defun new-songs (raw)
  "Create a list of song objects from raw parsed data"
  (map 'list (lambda (song)
               (make-instance 'song
                :name     (first song)
                :duration (second song)
                :url      (third song)))
       raw))

(defun new-albums (raw)
  (map 'list (lambda (album)
               (make-instance 'album
                :name  (first album)
                :year  (second album)
                :songs (new-songs (third album))))
       raw))

(defun new-artist (artist)
  (let ((bio (biography artist)))
    (make-instance
     'artist
     :name (substitute #\Space #\+ artist)
     :genres (map 'list (lambda (g)
                          (make-instance 'genre :name g))
                  (second bio))
     :similar (first bio)
     :albums (new-albums (third bio)))))

(with-local-htmls
  (similar "Pendragon"))

(defparameter *lost-in-kiev*
  (with-local-htmls
    (new-artist "Lost in Kiev")))

(defparameter *pendragon*
  (with-local-htmls
    (new-artist "Pendragon")))

(with-local-htmls
  (new-artist "Lost in Kiev"))



;; (defvar *metalstorm-url* "http://www.metalstorm.net/bands/index.php?b_where=s.style&b_what=Doom&prefix=Funeral")
;; (defvar *metalstorm-bands-genre-url*
;;   "http://www.metalstorm.net/bands/index.php?b_where=s.style&b_what=~A&prefix=~A")
;; (defvar *metalstorm-band-page* "http://www.metalstorm.net/bands/~A")
;; (defvar *metalstorm-album-tracks* "http://www.metalstorm.net/bands/~A")

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
;; (defvar *request* (dex:get *metalstorm-url*))
;; (defvar *parsed* (plump:parse *request*))
;; (defvar *bands+band-link*
;;   (lquery:$ *parsed* "table.table td b a"
;;     (map (lambda (node)
;;            (list (plump:text node)
;;                  (plump:attribute node "href"))))))

;; (and (> (length *bands+band-link*)
;;         10)
;;      (subseq  *bands+band-link* 0 10))


;; (defvar *genius-lyrics-url* "https://genius.com/~A-lyrics")
;; (defun genius-lyrics (artist-song)
;;   (with-lqueryed-url *genius-lyrics-url* (artist-song)
;;       "div.lyrics p"
;;     (text)))
;; (defvar *anathema-one-last-goodbye-lyrics* (genius-lyrics "anathema-one-last-goodbye"))

;; (aref *anathema-one-last-goodbye-lyrics* 0)


;; (defvar *genius-slow-mort*
;;   (let* ((url "https://genius.com/Slow-mort-lyrics" )
;;          (request (dex:get url))
;;          (parsed (plump:parse request)))
;;     parsed))

;; (lquery:$ *genius-slow-mort* "[initial-content-for=\"song_body\"]" (text))
;; (lquery:$ *genius-slow-mort* "div.lyrics p" (text))

