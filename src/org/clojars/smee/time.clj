(ns 
  ^{:author "Steffen Dienst",
    :doc "useful time handling functions"}
  org.clojars.smee.time
  (:use [clojure.contrib.def :only (defvar-)]
        [clojure.contrib.singleton :only (per-thread-singleton)]))


(defvar- dateformat (per-thread-singleton #(java.text.SimpleDateFormat. "yyyy-MM-dd HH:mm:ss,SSS")))
(defvar- dateonlyformat (per-thread-singleton #(java.text.SimpleDateFormat. "yyyy-MM-dd")))


(defn parse-time [^String s]
  (.getTime (.parse (dateformat) s)))

(defn time-to-string 
  "Format milliseconds into format \"yyyy-MM-dd HH:mm:ss,SSS\""
  [t]
  (.format (dateformat) (java.util.Date. t)))

(defn time-to-date-string 
  "Format milliseconds into format \"yyyy-MM-dd\""
  [t]
  (.format (dateonlyformat) (java.util.Date. t)))

(defn date-string 
  "Get date as string with format yyyyMMdd."
  ([]
    (date-string (java.util.Date.)))
  ([date]
    (.format (java.text.SimpleDateFormat. "yyyyMMddHHmm") date)))

(defn millis-to-time-units
  "Convert time in milliseconds to a seq that contains
entries for different time units: :seconds, :minutes, :hours, :days :weeks :years"
  ([time-in-msec] 
    (millis-to-time-units 
      []
      [52 7 24 60 60 1000 1] 
      time-in-msec))
  ([result time-unit-durations rest-time]
    (if (empty? time-unit-durations)
      (interleave [:years :weeks :days :hours :minutes :seconds :milliseconds] result)
      ;else
      (let [duration-in-msec (apply * time-unit-durations)]
        (recur 
          (conj result (quot rest-time duration-in-msec))
          (rest time-unit-durations)
          (rem rest-time duration-in-msec))))))

(defn millis-to-string 
  "String representation of the time in msec. Uses only time modulo 24h."
  [time]
  (let [m   (apply hash-map (millis-to-time-units time))
        i2s (fn [i] (if (< i 10) (str "0" i) (str i)))]
    (str (i2s (:hours m)) \: (i2s (:minutes m)) \: (i2s (:seconds m)) \, (i2s (:milliseconds m)))))
