(in-package :muse)

(define-opts
  (:name :help
   :short #\h :long "help"
   :description "print this help text")
  (:name :play/pause
   :short #\p :long "play/pause"
   :description "play or pause")
  (:name :level
   :description "the program will run on LEVEL level"
   :short #\l :long "level"
   :arg-parser #'parse-integer
   :meta-var "LEVEL")
  (:name :output
   :description "redirect output to file FILE"
   :short #\o
   :long "output"
   :arg-parser #'identity
   :meta-var "FILE"))

(defun unknown-option (condition)
  (format t "warning: ~s option is unknown!~%" (option condition))
  (invoke-restart 'skip-option))

(defmacro when-option ((options opt) &body body)
  `(let ((it (getf ,options ,opt)))
     (when it
       ,@body)))

(defun main ()
  (multiple-value-bind (options free-args)
      (handler-case
          (handler-bind ((unknown-option #'unknown-option))
            (get-opts))
        (missing-arg (condition)
          (format t "fatal: option ~s needs an argument!~%"
                  (option condition)))
        (arg-parser-failed (condition)
          (format t "fatal: cannot parse ~s as argument of ~s~%"
                  (raw-arg condition)
                  (option condition))))
    (when-option (options :help)
      (describe
       :prefix "example — program to demonstrate unix-opts library"
       :suffix "so that's how it works…"
       :usage-of "example.sh"
       :args     "[FREE-ARGS]"))
    (when-option (options :play/pause)
      (format t "toggling the play..."))
    (when-option (options :level)
      (format t "I see you've supplied level option, you want ~a level!~%" it))
    (when-option (options :output)
      (format t "I see you want to output the stuff to ~s!~%"
              (getf options :output)))
    (when (null options)
      (print "no options given"))
    (format t "free args: ~{~a~^, ~}~%" free-args)))

