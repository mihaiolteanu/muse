(in-package :muse-tests)

(def-suite :all-tests)
(in-suite :all-tests)

(test artist-parse-from-html-page
  (with-local-htmls
    (let* ((pendragon (new-artist "Pendragon"))
           (albums (artist-albums pendragon))
           (pure-album (first albums))
           (pure-album-songs (album-songs pure-album)))
      (is (string= (artist-name pendragon) "Pendragon"))
      (is (= (length (artist-genres pendragon)) 6))
      (is-true
       (equal (mapcar #'genre-name (artist-genres pendragon))
              '("progressive rock" "neo-prog" "neo progressive rock" "rock" "progressive"
                "neo-progressive rock")))
      ;; Similar artists
      (is-true
       (equal (map 'list #'identity (artist-similar pendragon))
              '("Arena" "IQ" "Satellite" "Pallas" "Marillion" "The Tangent"
                "Comedy of Errors" "Magenta" "Big Big Train" "Knight Area" "Sylvan" "Fish"
                "Mostly Autumn" "Transatlantic" "RPWL")))
      (is (= (length (artist-albums pendragon)) 6))
      ;; Album names
      (is-true
       (equal (mapcar #'album-name albums)
              '("Pure" "Not of This World" "The Masquerade Overture" "Believe" "Passion"
                "The Window of Life")))
      ;; Album years
      (is-true
       (equal (mapcar #'album-year albums)
              '("2008" "2001" "1996" "2005" "2011" "1993")))
      ;; Album songs (name, duration and url)
      (is-true
       (equal (mapcar #'song-name pure-album-songs)
              '("Indigo" "Eraserhead" "Comatose, Part I: View From the Seashore"
                "Comatose, Part II: Space Cadet" "Comatose, Part III: Home and Dry"
                "The Freak Show" "It's Only Me")))
      (is-true
       (equal (mapcar #'song-url pure-album-songs)
              '("https://www.youtube.com/watch?v=ccr-VkEpE18"
                "https://www.youtube.com/watch?v=gHeWRLugWQA"
                NIL NIL NIL             ;No urls available for these songs
                "https://www.youtube.com/watch?v=42m2llb0w84"
                "https://www.youtube.com/watch?v=3obNa5BE5MY")))
      (is-true
       (equal (mapcar #'song-duration pure-album-songs)
              '("13:43" "9:05" "7:40" "4:02" "5:55" "4:11" "8:16")))
      (is (= (length (tag-artists "melancholic"))
             63))
      (is (equal (subseq (tag-artists "melancholic") 0 10)
                 '("Antimatter" "My Dying Bride" "Blackfield"
                   "Sopor Aeternus & The Ensemble of Shadows"
                   "Juzhin" "Lake of Tears" "Anathema" "Katatonia"
                   "Radiohead" "Placebo")))
      )))

(test retrieve-album-from-db
  (with-test-db
    (with-local-htmls
      (insert-artist (new-artist "Pendragon"))
      (is (equal (mapcar #'album-name
                         (albums "Pendragon"))
                 '("Pure" "Not of This World" "The Masquerade Overture"
                   "Believe" "Passion" "The Window of Life")))
      (is (equal (mapcar #'album-year
                         (albums "Pendragon"))
                 '(2008 2001 1996 2005 2011 1993)))
      (is (equal (mapcar (lambda (s)
                           (length (album-songs s)))
                         (albums "Pendragon"))
                 '(7 11 10 9 7 9))
          "Correct number of songs in each album"))))

(test insert-artist-in-db
  (with-test-db
    (with-local-htmls
      (is (= (length (artists)) 0))
      (insert-artist (new-artist "Pendragon"))
      ;; Artist was added
      (is (= (length (artists)) 1))
      (is (string= (artist-name (first (artists))) "Pendragon"))
      ;; Simlar artists
      (is (equal
           (mapcar #'first (similar "Pendragon"))
           '("Arena" "IQ" "Satellite" "Pallas" "Marillion" "The Tangent" "Comedy of Errors"
             "Magenta" "Big Big Train" "Knight Area" "Sylvan" "Fish" "Mostly Autumn"
             "Transatlantic" "RPWL")))
      ;; All albums available
      (is (= (length (albums "Pendragon")) 6))
      ;; All songs parsed
      (is (= (length (all-songs)) 53))
      (is (= (length (songs "Pendragon")) 53))
      ;; Correct song url, duration, etc..
      (let ((pendragon-songs (songs "Pendragon")))
        (is (equal
             (subseq (mapcar #'song-name pendragon-songs)
                     0 10)
             '("Indigo" "Eraserhead" "Comatose, Part I: View From the Seashore"
               "Comatose, Part II: Space Cadet" "Comatose, Part III: Home and Dry"
               "The Freak Show" "It's Only Me"
               "If I Were the Wind (and You Were the Rain)"
               "Dance of the Seven Veils - Pt. 1 Faithless"
               "Dance of the Seven Veils - Pt. 2 All Over Now")))
        (is (equal
             (subseq (mapcar #'song-duration pendragon-songs)
                     0 10)
             '("13:43" "9:05" "7:40" "4:02" "5:55"
               "4:11" "8:16" "9:23" "4:09" "7:31")))
        (is (equal
             (subseq (mapcar #'song-url pendragon-songs)
                     0 10)
             '("https://www.youtube.com/watch?v=ccr-VkEpE18"
               "https://www.youtube.com/watch?v=gHeWRLugWQA"
               "NIL" "NIL" "NIL"
               "https://www.youtube.com/watch?v=42m2llb0w84"
               "https://www.youtube.com/watch?v=3obNa5BE5MY"
               "https://www.youtube.com/watch?v=V4BUn1YYWJE"
               "NIL" "NIL")))))))

(test new-artist-if-artist-not-in-db
  "If the artist is not in the database, calling songs should retrieve
it first, save it in the db and then returning the correct number of
entries."
  (with-test-db
    (with-local-htmls
      (is (= (length (songs "Pendragon"))
             53)))))

(test new-genre-artist-if-not-in-db
  "If artists for the given tag do not exists in the db, make sure to 
fetch them first and then return the results from the db"
  (with-test-db
    (with-local-htmls
      (let ((artists (genre-artists "melancholic")))
        (is (= (length artists)
               63))
        (is (equal (subseq (mapcar #'artist-name
                                   (genre-artists "melancholic"))
                           0 5)
                   '("Antimatter" "My Dying Bride" "Blackfield"
                     "Sopor Aeternus & The Ensemble of Shadows" "Juzhin")))
        ))))

(defparameter *localhost* "http://127.0.0.1:~a/")

(test server-interaction
  "Test the pages returned by the http server"
  (load #P"~/muserc.lisp" :if-does-not-exist nil)
  (let ((localhost (format nil *localhost* *port*)))
    (with-test-db
      (with-local-htmls
        (is (= (length (artists)) 0))
        (insert-artist (new-artist "Pendragon"))
        (insert-artist (new-artist "Lost in Kiev"))
        (let ((artists
                (let ((node (parse-html (concatenate 'string localhost "artists"))))
                  ($ node "a.artist" (text)))))
          (is (= (length (artists)) 2))
          (is (equalp artists #("Pendragon" "Lost in Kiev"))
              "The artists page returned by the server contains the
          two artists just added"))))))


(setf 5am:*run-test-when-defined* T)

;; (run!)

;; (progn (ql:quickload :muse)
;;        (ql:quickload :muse/tests))
