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
      )))

(test insert-artist-in-db
  (with-test-db
    (with-local-htmls
      (insert-artist (new-artist "Pendragon"))
      ;; Artist was added
      (is (= (length (artists)) 1))
      (is (string= (first (first (artists))) "Pendragon"))
      ;; Simlar artists
      (is (equal
           (mapcar #'first (similar "Pendragon"))
           '("Arena" "IQ" "Satellite" "Pallas" "Marillion" "The Tangent" "Comedy of Errors"
             "Magenta" "Big Big Train" "Knight Area" "Sylvan" "Fish" "Mostly Autumn"
             "Transatlantic" "RPWL")))
      ;; All albums available
      (is (= (length (albums "Pendragon")) 6))
      (is (every (lambda (a)
                   (string= (second a) "Pendragon"))
                 (albums "Pendragon")))
      ;; Album id autoincrement
      (is (equal (mapcar #'first
                         (albums "Pendragon"))
                 '(1 2 3 4 5 6)))
      ;; Album names
      (is (equal (mapcar #'third
                         (albums "Pendragon"))
                 '("Pure" "Not of This World" "The Masquerade Overture"
                   "Believe" "Passion" "The Window of Life")))
      ;; All songs parsed
      (is (= (length (all-songs)) 53))
      (is (= (length (songs "Pendragon")) 53))
      ;; Correct song url, duration, etc..
      (is (equal (first (songs "Pendragon"))
                 '("Pendragon" "Pure" "Indigo"
                   "https://www.youtube.com/watch?v=ccr-VkEpE18"
                   "13:43")))
      )))

;; (run!)

;; (progn (ql:quickload :muse)
;;        (ql:quickload :muse/tests))
