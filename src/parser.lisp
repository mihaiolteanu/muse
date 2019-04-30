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
  (when (not (null (search "http" template)))
    (setf components (mapcar #'urlencode components)))
  (let* ((url (eval `(format nil ,template ,@components)))
         (request (if (not (null (search "http" url)))
                      (handler-case (get url) ; Handle http requests
                        (http-request-not-found ()
                          nil)) 
                      (uiop:read-file-string url)))) ;handle local html files
    (when request
      (parse request))))

(defun available-youtube-video? (url)
  "Return true if the video is made available by its user."
  (let* ((node (parse-html url))
         (available-msg ($ node "div#unavailable-submessage.submessage" (text))))
    (= (length (string-trim '(#\Space #\Tab #\Newline)
                            (aref available-msg 0)))
       0)))

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

(defun raw-album-info (artist album)
  (let* ((node (parse-html (album-page artist album)))
         (release-date ($ node "ul.metadata-list p.metadata-display" (text)))
         (tracks ($ node "section#tracks-section tr"
                   (map (lambda (node)
                          (concatenate
                           'list ;(song-title duration link)
                           ($ node "td.chartlist-name a.link-block-target" (text))
                           (map 'vector #'trim-whitespace
                                ($ node "td.chartlist-duration" (text)))
                           (let ((url ($ node "td.chartlist-play a" (attr :href))))
                             (if (equalp url #())
                                 (list "")  ;not all songs have a url
                                 url))))))))
    (list (lastcar (split-sequence #\Space (aref release-date 0)))
          ;; Same string needed as above to clear entries which contain
          ;; artist name but not a song name (?!). Hackish
          (remove-if (lambda (tr)
                       (or (null tr)
                           (equalp tr '(""))))
                     tracks))))

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
                      (raw-album-info artist (text node)))))))))

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
           *tag-artists* original-tag-artists)))

(defun new-songs (raw-songs artist-name)
  "Create a list of song objects from raw parsed data"
  (map 'list (lambda (song)
               (make-song artist-name (first song) (second song) (third song)))
       raw-songs))

(defun new-albums (raw-albums artist-name)
  (map 'list (lambda (album)
               (make-album (first album) (second album)
                           (new-songs (third album) artist-name)))
       raw-albums))

(defun new-artist (artist-name)
  (let* ((artist-name (clean-name artist-name))
         (bio (biography artist-name)))
    (make-artist artist-name
                 :genres (map 'list (lambda (genre-name)
                                      (make-genre genre-name))
                              (second bio))
                 :similar (first bio)
                 :albums (new-albums (third bio) artist-name))))

(defun artists-with-tag (tag)
  "Get a list of artists with the tag genre from the first 3 pages."
  (apply #'append
         (mapcar (lambda (page)
                   (let ((node
                           (parse-html
                            *tag-artists* (url-name tag) page)))
                     (coerce ($ node "h3.big-artist-list-title" (text))
                             'list)))
                 '(1 2 3))))


;;; Lyrics
(defun site-lyrics (artist song url-template parse-param)
  "Template for lyrics parsing."
  (let* ((node (parse-html
                (format nil url-template
                        (substitute #\- #\Space artist)
                        (substitute #\- #\Space song))))
         (result ($ node parse-param (text))))
    (unless (= (length result)
               0)
      (aref result 0))))

(defun genius-lyrics (artist song)
  (site-lyrics artist song
               "https://genius.com/~a-~a-lyrics"
               "div.lyrics p"))

(defun songlyrics (artist song)
  (site-lyrics artist song
               "http://www.songlyrics.com/~a/~a-lyrics/"
               "p#songLyricsDiv"))

(defun musix-lyrics (artist song)
  (site-lyrics artist song
               "https://www.musixmatch.com/lyrics/~a/~a"
               "span.lyrics__content__warning"))

(defun song-lyrics-from-web (artist song)
  "Get the lyrics from web for the given artist and song."
  (or (genius-lyrics artist song)
      (songlyrics artist song)
      (musix-lyrics artist song)))
