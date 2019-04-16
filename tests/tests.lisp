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
        (is (equal (mapcar #'song-name pendragon-songs)
                   '("Indigo" "Eraserhead" "Comatose, Part I: View From the Seashore"
                     "Comatose, Part II: Space Cadet" "Comatose, Part III: Home and Dry"
                     "The Freak Show" "It's Only Me"
                     "If I Were the Wind (and You Were the Rain)"
                     "Dance of the Seven Veils - Pt. 1 Faithless"
                     "Dance of the Seven Veils - Pt. 2 All Over Now"
                     "Not of This World: Not of This World, Pt. 1"
                     "Not of This World: Give It to Me, Pt. 2"
                     "Not of This World: Green Eyed Angel, Pt. 3"
                     "A Man of Nomadic Traits"
                     "World's End - Pt. 1 The Lost Children"
                     "Worlds End: And Finally, Pt. 2"
                     "Paintbox (acoustic version)"
                     "King of the Castle (acoustic version)"
                     "The Masquerade Overture"
                     "As Good as Gold"
                     "Paintbox"
                     "The Pursuit of Excellence"
                     "Guardian Of My Soul"
                     "The Shadow"
                     "Masters Of Illusion"
                     "Bird of Paradise"
                     "Midnight Running"
                     "A Million Miles Away"
                     "Believe"
                     "No Place for the Innocent"
                     "Wisdom Of Solomon"
                     "The Wishing Well, Pt.1 - For Your Journey"
                     "The Wishing Well: II. Sou' By Sou' West"
                     "The Wishing Well, Pt.3 - We Talked"
                     "The Wishing Well, Pt.4 - Two Roads"
                     "Learning Curve" "The Edge of the World"
                     "Passion"
                     "Empathy"
                     "Feeding Frenzy"
                     "This Green And Pleasant Land"
                     "It's Just A Matter Of Not Getting Caught"
                     "Skara Brae"
                     "Your Black Heart"
                     "The Walls of Babylon"
                     "Ghosts"
                     "Breaking the Spell"
                     "The Last Man on Earth"
                     "Nostradamus (Stargazing)"
                     "Am I Really Losing You?"
                     "Third World In The UK"
                     "Dune" "Fallen Dreams and Angels")))
        (is (equal (mapcar #'song-duration pendragon-songs)
                   '("13:43" "9:05" "7:40" "4:02" "5:55" "4:11" "8:16" "9:23" "4:09"
                     "7:31" "7:20"  "2:24" "6:43" "11:43" "10:46" "7:15" "4:25" "4:44"
                     "3:02" "7:15" "8:38" "2:36" "12:41" "9:53" "3:35" "6:57" "7:46"
                     "3:22" "2:57" "5:36" "7:07" "4:31" "6:48" "5:29" "4:19" "6:34" "8:20"
                     "5:27" "11:21" "6:15" "13:13" "4:41" "7:31" "6:46" "10:50" "7:58"
                     "8:30" "14:46" "6:23" "4:47" "7:15" "4:40" "5:23")))
        (is (equal (mapcar #'song-url pendragon-songs)
                   '("https://www.youtube.com/watch?v=ccr-VkEpE18"
                     "https://www.youtube.com/watch?v=gHeWRLugWQA"
                     "NIL" "NIL" "NIL"
                     "https://www.youtube.com/watch?v=42m2llb0w84"
                     "https://www.youtube.com/watch?v=3obNa5BE5MY"
                     "https://www.youtube.com/watch?v=V4BUn1YYWJE"
                     "NIL" "NIL" "NIL" "NIL"
                     "https://www.youtube.com/watch?v=pvuBpgXNOcg"
                     "https://www.youtube.com/watch?v=0m32f4X3ROU"
                     "NIL" "NIL"
                     "https://www.youtube.com/watch?v=5Qveh4b3qkA"
                     "https://www.youtube.com/watch?v=0pb8UG4iODE"
                     "https://www.youtube.com/watch?v=2G5hHEuG65I"
                     "https://www.youtube.com/watch?v=qQH5Pvawkic"
                     "https://www.youtube.com/watch?v=5Qveh4b3qkA"
                     "https://www.youtube.com/watch?v=iChCibt5ztQ"
                     "https://www.youtube.com/watch?v=S7MDO-GnvEY"
                     "https://www.youtube.com/watch?v=yIW-uIbeBis"
                     "https://www.youtube.com/watch?v=dz_C0wvb4y4"
                     "https://www.youtube.com/watch?v=BJaVEjTLt3E"
                     "NIL" "NIL" "NIL"
                     "https://www.youtube.com/watch?v=AUWIfefu4wE"
                     "NIL" "NIL" "NIL" "NIL" "NIL"
                     "https://www.youtube.com/watch?v=DgffQKNnxc4"
                     "NIL"
                     "https://www.youtube.com/watch?v=nfKE3lfdoHk"
                     "https://www.youtube.com/watch?v=O9R2dZCshK0"
                     "https://www.youtube.com/watch?v=1xQR14OeD0M"
                     "https://www.youtube.com/watch?v=eORvMUrLqt0"
                     "https://www.youtube.com/watch?v=Q9ne4MFcp18"
                     "https://www.youtube.com/watch?v=t0N6Q7huah8"
                     "https://www.youtube.com/watch?v=7lANa-oS944"
                     "https://www.youtube.com/watch?v=RcRw-JXzW-s"
                     "https://www.youtube.com/watch?v=6MjeaMmcJtU"
                     "https://www.youtube.com/watch?v=8564jSjUn1s"
                     "NIL"
                     "https://www.youtube.com/watch?v=4IydG57-mrw"
                     "https://www.youtube.com/watch?v=nPHRoF-SOGk"
                     "NIL"
                     "https://www.youtube.com/watch?v=1sDjW-NL0uQ"
                     "https://www.youtube.com/watch?v=_8onzQSfoME")))))))

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
        (is (equal (subseq artists 0 5)
                   '("Antimatter" "My Dying Bride" "Blackfield"
                     "Sopor Aeternus & The Ensemble of Shadows" "Juzhin")))))))

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
