(ns 
  ^{:author "Steffen Dienst",
    :doc "useful file handling functions"}
  com.github.smee.file
  (:use 
    [clojure.java.io :only (file reader)]
    [clojure.contrib.string :only (replace-char)])
  (:import
    [java.io File BufferedReader]))

(defn find-files 
  "Traverse directory dirpath depth first, return all files matching
the regular expression pattern. Per default returns only files, no directories."
  ([dirpath] (find-files dirpath #".*")) 
  ([dirpath pattern] (find-files dirpath pattern true))
  ([dirpath pattern files-only?]
    (for [file (-> dirpath file file-seq) 
          :when (re-matches pattern (.getName file))
          :when (or (not files-only?) (.isFile file))]
      file)))

(defn read-lines-enc
  "Like clojure.core/line-seq but opens f with reader. An encoding may be specified, too.
 Automatically closes the reader AFTER YOU CONSUME THE ENTIRE SEQUENCE."
  ([f] (read-lines-enc f "UTF-8"))
  ([f encoding]
    (let [read-line (fn this [^BufferedReader rdr]
                      (lazy-seq
                        (if-let [line (.readLine rdr)]
                          (cons line (this rdr))
                          (.close rdr))))]
      (read-line (reader f :encoding encoding)))))


(defn extract-relative-path 
  "Extract path relative to base directory."
  [^File base ^File file]
  (-> base .toURI (.relativize (.toURI file)) .getPath))

(defn unix-path [^String s]
  (replace-char \\ \/ s))