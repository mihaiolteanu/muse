(in-package :muse-tests)

(defmacro def-muse-test (name &body body)
  `(test ,name
     (with-test-db
       (with-local-htmls
         ,@body))))

(def-suite :all-tests)

(def-suite :mpv-tests
  :description "Test the mpv interaction: opening with correct urls, adding songs, etc."
  :in :all-tests)

(def-suite :no-dependency-tests
  :description "Tests that don't depend on external programs"
  :in :all-tests)

(in-suite :no-dependency-tests)

(def-muse-test artist-parse-from-html-page
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
              "" "" ""             ;No urls available for these songs
              "https://www.youtube.com/watch?v=42m2llb0w84"
              "https://www.youtube.com/watch?v=3obNa5BE5MY")))
    (is-true
     (equal (mapcar #'song-duration pure-album-songs)
            '("13:43" "9:05" "7:40" "4:02" "5:55" "4:11" "8:16")))
    (is (= (length (artists-with-tag "melancholic"))
           63))
    (is (equal (subseq (artists-with-tag "melancholic") 0 10)
               '("Antimatter" "My Dying Bride" "Blackfield"
                 "Sopor Aeternus & The Ensemble of Shadows"
                 "Juzhin" "Lake of Tears" "Anathema" "Katatonia"
                 "Radiohead" "Placebo")))))

(def-muse-test retrieve-album-from-db
  (insert-artist (new-artist "Pendragon"))
  (let ((pendragon (artist-from-db "Pendragon")))
    (is (equal (mapcar #'album-name
                       (artist-albums pendragon))
               '("Pure" "Not of This World" "The Masquerade Overture"
                 "Believe" "Passion" "The Window of Life")))
    (is (equal (mapcar #'album-year
                       (artist-albums pendragon))
               '(2008 2001 1996 2005 2011 1993)))
    (is (equal (mapcar (lambda (s)
                         (length (album-songs s)))
                       (artist-albums pendragon))
               '(7 11 10 9 7 9))
        "Correct number of songs in each album")))

(def-muse-test insert-artist-in-db
  (is (= (length (all-artists)) 0))
  (insert-artist (new-artist "Pendragon"))
  ;; Artist was added
  (is (= (length (all-artists)) 1))
  (is (string= (artist-name (first (all-artists))) "Pendragon"))
  
  (let ((pendragon (artist-from-db "Pendragon")))
    ;; Simlar artists
    (is (equal
         (mapcar #'artist-name (artist-similar pendragon))
         '("Arena" "IQ" "Satellite" "Pallas" "Marillion" "The Tangent" "Comedy of Errors"
           "Magenta" "Big Big Train" "Knight Area" "Sylvan" "Fish" "Mostly Autumn"
           "Transatlantic" "RPWL")))
    ;; All albums available
    (is (= (length (artist-albums pendragon)) 6))
    ;; All songs parsed
    (is (= (length (all-songs)) 53))
    (is (= (length (artist-songs pendragon)) 53))
    ;; Correct song url, duration, etc..
    (let ((pendragon-songs (artist-songs pendragon)))
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
             "" "" ""
             "https://www.youtube.com/watch?v=42m2llb0w84"
             "https://www.youtube.com/watch?v=3obNa5BE5MY"
             "https://www.youtube.com/watch?v=V4BUn1YYWJE"
             "" ""))))))

(def-muse-test new-artist-if-artist-not-in-db
  "Fetch the artist from web and save it in db if not already in db."
  (is (= (length (artist-songs
                  (artist-from-db "Pendragon")))
         53)))

(def-muse-test new-genre-artist-if-not-in-db
  "If artists for the given tag do not exists in the db, make sure to 
fetch them first and then return the results from the db"
  (let ((artists (all-genre-artists "melancholic")))
    (is (= (length artists)
           63))
    (is (equal (subseq (mapcar #'artist-name
                               artists)
                       0 5)
               '("Antimatter" "My Dying Bride" "Blackfield"
                 "Sopor Aeternus & The Ensemble of Shadows" "Juzhin")))))

(load #P"~/muserc.lisp" :if-does-not-exist nil)
(defparameter *localhost* (format nil "http://127.0.0.1:~a/" *port*))

(defun build-url (param)
  (let ((localhost (format nil *localhost* *port*)))
    (concatenate 'string localhost param)))

(def-muse-test server-interaction
  "Test the pages returned by the http server"
  (insert-artist (new-artist "Pendragon"))
  (insert-artist (new-artist "Lost in Kiev"))

  (let* ((node (parse-html (build-url "artists")))
         (artists ($ node "a.artist" (text))))
    (is (equalp artists #("Pendragon" "Lost in Kiev"))
        "The artists page returned by the server contains the
          two artists just added"))

  (let* ((node (parse-html (build-url "tags")))
         (tags ($ node "a.tag" (text))))
    (is (equalp tags
                #("progressive rock" "neo-prog" "neo progressive rock"
                  "rock" "progressive" "neo-progressive rock"
                  "post-rock" "post-metal" "instrumental" "french"))))

  (let* ((node (parse-html (build-url "artist/Pendragon")))
         (pendragon-tags ($ node "a.tag" (text))))
    (is (equalp pendragon-tags
                #("progressive rock" "neo-prog" "neo progressive rock"
                  "rock" "progressive" "neo-progressive rock"))))

  (let* ((node (parse-html (build-url "artist/Pendragon")))
         (pendragon-songs ($ node "a.song" (text))))
    (is (= (length pendragon-songs) 53))
    (is (equalp (subseq pendragon-songs 0 5)
                #("Indigo" "Eraserhead"
                  "Comatose, Part I: View From the Seashore"
                  "Comatose, Part II: Space Cadet"
                  "Comatose, Part III: Home and Dry")))))

(def-muse-test tags-retrieval
  (let* ((node (parse-html (build-url "tag/progressive+rock")))
         (artists ($ node "a.artist" (text))))
    (is (= (length artists) 63))
    (is (equalp (subseq artists 0 10)
                #("Pink Floyd" "Muse" "Porcupine Tree" "Tool"
                  "The Mars Volta" "King Crimson" "Rush"
                  "Led Zeppelin" "Yes" "Genesis")))))

(defmacro server-call-expect (url fn expect)
  "I have no fking clue why, on the first call, the play mock is not called.
  That is, the call-times-for play returns zero and the nth-mock-args-for returns nil.
  On the second run, the call-times for play is 2 and the args are correct.
  I have verified that the player is correctly called every time I call the server
  url, so no clue what is going on. Hack: call the same stuff twice, the first one
  for warm-up, the second time for actuall testing."
  `(progn
     (with-dynamic-mocks (play next-song prev-song stop-player)
       (parse-html (build-url (format nil "play?source=~a" ,url))))
     (with-dynamic-mocks (play next-song prev-song stop-player)
       (parse-html (build-url (format nil "play?source=~a" ,url)))
       (is (equal (nth-mock-args-for 1 ,fn)
                  ,expect)))
     (clear-calls)))

(def-muse-test server-player-start
  ;; Start the player with different sources. The source of playable items
  ;; it's in the parameter attached to the /play url. After calling this url
  ;; the play function in the player package should receive the correct playable
  ;; items. What the player does after that is not this test's business. As a result,
  ;; mock the play function and only test if it's called with the correct parameters.
  (server-call-expect
   "/song/Pendragon/out+of+this+world"
   'play '(("song" "Pendragon" "out of this world") NIL))
  
  (server-call-expect
   "/artist/Pendragon"
   'play '(("artist" "Pendragon") NIL))

  (server-call-expect
   "/similar/Pendragon"
   'play '(("similar" "Pendragon") NIL))

  (server-call-expect
   "/artist/Pendragon/album/Pure"
   'play '(("artist" "Pendragon" "album" "Pure") NIL))

  (server-call-expect
   "/tag/rock"
   'play '(("tag" "rock") NIL))

  ;; Set shuffle on and expect play to be called with toggle param set to true
  (parse-html (build-url "toggle-shuffle"))
  (server-call-expect "/tag/rock"
                      'play '(("tag" "rock") t))

  ;; Turn shuffle off again
  (parse-html (build-url "toggle-shuffle"))
  (server-call-expect
   "/tag/rock"
   'play '(("tag" "rock") NIL))

  ;; Other player functionality
  (server-call-expect
   "/next"
   'next-song NIL)
  
  (server-call-expect
   "/previous"
   'prev-song NIL)

  (server-call-expect
   "/stop"
   'stop-player NIL))

(in-suite :mpv-tests)

(defun server-status (item)
  (let ((node (parse-html (build-url "home"))))
    ($ node item (text))))

(def-muse-test mpv-starts-and-plays
  "Actually test if mpv is called on startup with correct urls.
Sleep is needed as mpv is an external program."
  (let ((indigo     (make-song "Pendragon" "Indigo" "13:43"
                               "https://www.youtube.com/watch?v=ccr-VkEpE18"))
        (eraserhead (make-song "Pendragon" "Eraserhead" "9:05"
                               "https://www.youtube.com/watch?v=gHeWRLugWQA"))
        (freak-show (make-song "Pendragon" "The Freak Show" "4:11"
                               "https://www.youtube.com/watch?v=42m2llb0w84"))
        (is-only-me (make-song "Pendragon" "It's Only Me" "8:16"
                               "https://www.youtube.com/watch?v=3obNa5BE5MY")))

    ;; Player and server both return the correct playing status
    (is-false (playing?))
    (is (equalp (server-status ".player-status")
                #("stopped")))

    ;; Start the player and wait for it to warm up
    (play '("artist" "Pendragon" "album" "Pure") nil)
    (sleep 1)

    ;; Is the mpv status returned correctly?
    (is-true (playing?))
    (is (equalp (server-status ".player-status")
                #("playing")))

    ;; Begin with the first song on the album
    (is (same-songs (playing-song) indigo))
    (is (equalp (server-status ".playing-song")
                #("Pendragon - Indigo ")))

    ;; Lyrics are also downloaded and saved from web when the song is first played
    (is (equalp (subseq
                 ;; Check the first line only
                 (aref (server-status ".playing-song-lyrics") 0)
                 8 41)
                "Never felt too good about my life"))

    ;; The playlist contains the first 3 playable songs on the album.
    (mapcar (lambda (playing expected)
              (is (same-songs playing expected)))
            (playing-songs)
            (list indigo eraserhead freak-show))

    ;; Go and play the next song on the album.
    (next-song) (sleep 1)
    (is (same-songs (playing-song) eraserhead))

    ;; The playlist should also be updated because muse is set up
    ;; fetch urls in advance
    ;; (is (same-songs (fourth (playing-songs)) is-only-me))

    ;; Go to the last playable song and hope nothing crashes in the process
    (next-song) (sleep 1)
    (next-song) (sleep 1)
    (is (same-songs (playing-song) is-only-me))
    (next-song) (sleep 1)
    (is (same-songs (playing-song) is-only-me))

    ;; Can we also go back to the previous song?
    (prev-song)
    (sleep 1)
    (is (same-songs (playing-song) freak-show))

    ;; Quiting muse should close mpv also, clear the list, etc
    (stop-player)
    (is-false (playing?))
    (is (null (playing-songs)))))

(setf 5am:*run-test-when-defined* T)
