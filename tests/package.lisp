(defpackage :muse-tests
  (:use
   :cl :dexador :plump :lquery
   :fiveam :mockingbird
   :persistence :objects
   :parser)
  (:shadowing-import-from :dexador :get)
  (:shadowing-import-from :dexador :delete)
  (:export run!
           all-tests))

