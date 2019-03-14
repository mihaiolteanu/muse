;;;; muse.asd

;; (defmethod perform ((o program-op) (c system))
;;   (uiop:dump-image (output-file o c) :executable t :compression t))

(defsystem :muse
  :description "Last.fm like music player"
  :author "Mihai Olteanu"
  :license "BSD"
  :version "0.0.1"
  :depends-on (:hunchentoot
               :dexador
               :plump
               :lquery
               :sqlite
               :unix-opts)
  :build-operation asdf:program-op
  :build-pathname "muse"
  :entry-point "muse:main"
  :serial t
  :components ((:file "package")
               (:file "objects")
               (:file "parser")               
               (:file "persistence")
               (:file "player")
               (:file "muse")))

