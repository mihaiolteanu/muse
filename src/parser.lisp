(in-package :parser)

(defvar *root-path* (system-relative-pathname :muse ""))

;; Parameters. For each set, both the real url (first parameter) and
;; local test pages path (second parameter) is specified. By default,
;; the real deal is used by the app (third parameter), but this should
;; be overwritten for test pourposes by the second parameter.

(defvar *lastfm-artist-page*
  "https://www.last.fm/music/~A")
(defvar *test-artist-page*
  (namestring (merge-pathnames "tests/html/music/~A/main.html" *root-path*)))
(defvar *artist-page*
  *lastfm-artist-page*)

(defvar *lastfm-artist-similar*
  "https://www.last.fm/music/~A/+similar")
(defvar *test-artist-similar*
  (namestring (merge-pathnames "tests/html/music/~A/similar.html" *root-path*)))
(defvar *artist-similar*
  *lastfm-artist-similar*)

(defvar *lastfm-album-page* "https://www.last.fm/music/~A")
(defvar *test-album-page*
  (namestring (merge-pathnames "tests/html/music/~A.html" *root-path*)))
(defvar *album-page*
  *lastfm-album-page*)

(defvar *lastfm-tag-artists*
  "https://www.last.fm/tag/~a/artists?page=~a")
(defvar *test-tag-artists*
  (namestring (merge-pathnames "tests/html/tag/~a/~a.html" *root-path*)))
(defvar *tag-artists*
  *lastfm-tag-artists*)

(defvar *lastfm-user-loved-url*
  "https://www.last.fm/user/~A/loved")

(defun url-name (str)
  "Replace any spaces with pluses; string will be used in url address and requests"
  (substitute #\+ #\Space str))

(defun clean-name (str)
  "Replace any pluses with spaces; string will be used in html page content."
  (substitute #\Space #\+ str))

(defun parse-html (template &rest components)
  "Given an url template and some url components,
 build the actual url and return a parsed object."
  (let* ((url (eval `(format nil ,template ,@components)))
         (request (if (not (null (search "http" url)))
                      (handler-case (get url) ; Handle http requests
                        (http-request-not-found ()
                          nil)) 
                      (uiop:read-file-string url)))) ;handle local html files
    (when request
      (parse request))))

(defun album-page (artist album)
  "Given an artist name and an album name, return a url
 with info for such an artist and album"
  (format nil *album-page*
          (format nil "~A/~A" artist
                  (url-name album))))

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
  (declare (string artist))
  (let* ((artist (url-name artist))
         (main-node (parse-html *artist-page* artist)) ;artist main page
         (similar-node (parse-html *artist-similar* artist))) ;similar artist page
    (list
     ($ similar-node "h3.big-artist-list-title" (text))
     ($ main-node "div.col-main li.tag" (text)) ;tags (genres)
     ($ main-node "section div ol a.link-block-target" ;; Build a list of tracks for each of the albums
       (map (lambda (node) 
              (append (list (text node)) ;album name
                      (album-info artist (text node)))))))))

(defmacro with-local-htmls (&body body)
  "Use local, pre-saved and original last.fm html pages to make requests.
Useful for testing the parser or playing around."
  `(let ((original-artist-page *artist-page*)
         (original-album-page *album-page*)
         (original-artist-similar *artist-similar*)
         (original-tag-artists *tag-artists*))
     (setf *artist-page* *test-artist-page*
           *album-page* *test-album-page*
           *artist-similar* *test-artist-similar*
           *tag-artists* *test-tag-artists*)
     ,@body
     (setf *artist-page* original-artist-page
           *album-page* original-album-page
           *artist-similar* original-artist-similar
           *tag-artists* original-tag-artists)
))

(defun new-songs (raw artist)
  "Create a list of song objects from raw parsed data"
  (map 'list (lambda (song)
               (make-instance 'song
                :artist   artist
                :name     (first song)
                :duration (second song)
                :url      (third song)))
       raw))

(defun new-albums (raw artist)
  (map 'list (lambda (album)
               (make-instance 'album
                :name  (first album)
                :year  (second album)
                :songs (new-songs (third album) artist)))
       raw))

(defun new-artist (artist)
  (let ((bio (biography artist)))
    (make-instance
     'artist
     :name (clean-name artist)
     :genres (map 'list (lambda (g)
                          (make-instance 'genre :name g))
                  (second bio))
     :similar (first bio)
     :albums (new-albums (third bio) (clean-name artist)))))

(defun tag-artists (tag)
  "Get a list of artists with the tag genre from the first 3 pages."
  (apply #'append
         (mapcar (lambda (page)
                   (let ((node
                           (parse-html
                            *tag-artists* (url-name tag) page)))
                     (coerce ($ node "h3.big-artist-list-title" (text))
                             'list)))
                 '(1 2 3))))

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

