(ns 
  ^{:author "Steffen Dienst",
    :doc "useful misc. functions"}
  org.clojars.smee.util
  (:use [clojure.stacktrace :only (print-cause-trace print-stack-trace)]
        [clojure.pprint :only (cl-format)]
        [clojure.java.io :only (file)])
  (:require
    [clojure.string :as cs])
  (:import
    [java.io File]))


(defn try-times*
  "Executes thunk. If an exception is thrown, will retry. At most n retries
  are done. If still some exception is thrown it is bubbled upwards in
  the call chain.
  http://stackoverflow.com/questions/1879885/clojure-how-to-to-recur-upon-exception"
  [n thunk]
  (loop [n n]
    (if-let [result (try
                      [(thunk)]
                      (catch Exception e
                        (when (zero? n)
                          (throw e))))]
      (result 0)
      (recur (dec n)))))

(defmacro try-times
  "Executes body. If an exception is thrown, will retry. At most n retries
  are done. If still some exception is thrown it is bubbled upwards in
  the call chain."
  [n & body]
  `(try-times* ~n (fn [] ~@body)))

  

(defmacro ignore-exceptions 
  "Catch any exception and print the message of its root cause."
  [ & body ]
  `(try 
     ~@body
     (catch Exception e# (.printStackTrace e#))))

(defn wrap-ignore-exceptions [f]
  (fn [& args]
    (ignore-exceptions (apply f args))))


(defn sleep-random 
  "Sleep for a random amount of milliseconds between [min,max]."
  [min max]
  {:pre [(<= min max) (>= min 0)]}
  (Thread/sleep (+ min (.nextInt (java.util.Random.) (- max min)))))

(defn print-latex-table [a-map]
  (letfn [(f [string] (cs/replace string "_" "\\_"))]
    (str "\\begin{longtable}{lr}" \newline
      (apply str (for [[k v] a-map] (str (f k) " & " (f v) " \\\\" \newline)))
      "\\end{longtable}" \newline)))

(defn print-simple-table [a-map]
  (doseq [[k v] a-map] (println k " " v)))

;(defn starts-with-any 
  ;"Does the string s start with any string within str-set?"
  ;[str-set ^String s]
  ;(some #(when (.startsWith s %) %) str-set))
(defn starts-with-any 
  "Does the string s start with any string within str-set?"
  [str-set ^String s]
  (some str-set (map (partial subs s 0) (range 0 (count s)))))


(defn table
  "Given a seq of hash-maps, prints a plaintext table of the values of the hash-maps.
  If passed a list of keys, displays only those keys.  Otherwise displays all the
  keys in the first hash-map in the seq.
Source: http://briancarper.net/blog/527/printing-a-nicely-formatted-plaintext-table-of-data-in-clojure"
  ([xs]
    (table xs (keys (first xs))))
  ([xs ks]
    (when (seq xs)
      (let [f (fn [old-widths x]
                (reduce (fn [new-widths k]
                          (let [length (inc (count (str (k x))))]
                            (if (> length (k new-widths 0))
                              (assoc new-widths k length)
                              new-widths)))
                  old-widths ks))
            widths (reduce f {} (conj xs (zipmap ks ks)))
            total-width (reduce + (vals widths))
            format-string (str "~{"
                            (reduce #(str %1 "~" (%2 widths) "A") "" ks)
                            "~}~%")]
        (cl-format true format-string (map str ks))
        (cl-format true "~{~A~}~%" (repeat total-width \-))
        (doseq [x xs]
          (cl-format true format-string (map x ks)))))))

(defn s2f 
  "Parse float string representation."
  ([s] (Float/parseFloat s))
  ([s dflt] (try (Float/parseFloat s) (catch NumberFormatException _ dflt))))

(defn s2i 
  "Parse integer string representation."
  ([s] (Integer/parseInt s))
  ([s dflt] (try (Integer/parseInt s) (catch NumberFormatException _ dflt))))
(defn s2l 
  "Parse long string representation."
  ([s] (Long/parseLong s))
  ([s dflt] (try (Long/parseLong s) (catch NumberFormatException _ dflt))))


(defn per-thread-singleton
  "Returns a per-thread singleton function.  f is a function of no
  arguments that creates and returns some object.  The singleton
  function will call f only once for each thread, and cache its value
  for subsequent calls from the same thread.  This allows you to
  safely and lazily initialize shared objects on a per-thread basis.

  Warning: due to a bug in JDK 5, it may not be safe to use a
  per-thread-singleton in the initialization function for another
  per-thread-singleton.  See
  http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=5025230"
  [f]
  (let [thread-local (proxy [ThreadLocal] [] (initialValue [] (f)))]
    (fn [] (.get thread-local))))

(defn read-properties
  "Read properties from file-able."
  [file-able]
  (with-open [f (java.io.FileInputStream. (file file-able))]
    (doto (java.util.Properties.)
      (.load f))))

(defmacro dbg[x] `(let [x# ~x] (println '~x "=" x#) x#))