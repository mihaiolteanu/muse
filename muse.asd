;;;; muse.asd

(defsystem :muse
  :description "Last.fm like music player"
  :author "Mihai Olteanu"
  :license "BSD"
  :version "0.0.1"
  :depends-on (:hunchentoot
               :cl-who
               :do-urlencode
               :yason
               :trivia
               :bt-semaphore
               :dexador
               :plump
               :lquery
               :sqlite
               :unix-opts)
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "objects")
                             (:file "parser")               
                             (:file "persistence")
                             (:file "player")
                             (:file "server"))))
  :in-order-to ((test-op (test-op "muse/tests"))))

(defsystem :muse/tests
  :depends-on (:muse :fiveam :mockingbird)
  :components ((:module "tests"
                :serial t
                :components ((:file "package")
                             (:file "tests"))))
  :perform (test-op (o c) (symbol-call :fiveam '#:run! :all-tests)))

;; (or (probe-file #p"~/muse.lisp")
;;     (with-open-file (str "/.../filename.txt"
;;                      :direction :output
;;                      :if-exists :supersede
;;                      :if-does-not-exist :create)
;;       (format str "write anything ~%")))
