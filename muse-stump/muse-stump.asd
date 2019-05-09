(asdf:defsystem :muse-stump
  :description "Interface to the muse player"
  :author "Mihai Olteanu <mihai_olteanu@fastmail.fm"
  :license  "BSD"
  :version "0.0.1"  
  :depends-on (:stumpwm :dexador :plump :lquery
                        :alexandria)
  :serial t
  :components ((:file "package")
               (:file "muse-stump")))
