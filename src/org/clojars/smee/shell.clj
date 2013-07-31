(ns org.clojars.smee.shell
  (:require [conch.core :as sh])
  (:use [clojure.java.io :only (file input-stream)]))

(defn run-cmd 
  "Run a shell command, return a string of its standard output contents."
  [& cmd-and-parameters]
  (let [process (apply sh/proc cmd-and-parameters)]
    (future (sh/stream-to process :err #_System/err (NullOutputStream.)))
    (sh/stream-to-string process :out)))

(defn process-cmd 
  "Run command line programs specified via `cmd-and-parameters`. Prints errors to System/err, 
return the result of running `handler-fn` on the standard output of the process.

Example: To process the contents of all files within a zip archive:
    (process-cmd (fn [^java.io.InputStream in] ...) \"unzip\" \"-c\" \"foo.zip\""
  [handler-fn & cmd-and-parameters ]
  (let [process (apply sh/proc cmd-and-parameters)
        errors (future (sh/stream-to process :err #_System/err (NullOutputStream.)))
        res (try 
              (handler-fn (input-stream  (:out process) :buffer-size (* 1024 1 )))
              (finally (sh/destroy process)))
        ;exit-code (sh/exit-code process)
        ]
    res))