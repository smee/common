(ns 
  ^{:author "Steffen Dienst",
    :doc "useful time handling functions"}
  org.clojars.smee.time
  (:use 
        [org.clojars.smee.util :only (per-thread-singleton)])
  (:import java.text.SimpleDateFormat 
           java.sql.Timestamp
           java.util.Date
           java.util.Calendar))

(defprotocol ^{:added "1.2"} Time-Coercions
  "Coerce between various 'resource-namish' things."
  (^{:tag java.sql.Timestamp} as-sql-timestamp [x] "Coerce argument to java.sql.Timestamp.")
  (^{:tag java.lang.Long} as-unix-timestamp [x] "Coerce argument to time in milliseconds")
  (^{:tag java.util.Date} as-date [x] "Coerce argument to java.util.Date")
  (^{:tag java.util.Calendar} as-calendar [x] "Coerce argument to java.util.Calendar"))

(extend-protocol Time-Coercions
  nil
  (as-sql-timestamp [_] nil)
  (as-unix-timestamp [_] nil)
  (as-date [_] nil) 
  (as-calendar [_] nil)
  
  Long
  (as-sql-timestamp [i] (Timestamp. i))
  (as-unix-timestamp [i] i)
  (as-date [i] (Date. i))
  (as-calendar [i] (doto (Calendar/getInstance) (.setTimeInMillis i)))
  
  Timestamp
  (as-sql-timestamp [s] s)
  (as-unix-timestamp [s] (.getTime s))
  (as-date [s] (Date. (.getTime s)))
  (as-calendar [s] (doto (Calendar/getInstance) (.setTimeInMillis (.getTime s)))) 
  
  java.util.Date
  (as-sql-timestamp [d] (Timestamp. (.getTime d)))
  (as-unix-timestamp [d] (.getTime d))
  (as-date [d] d)
  (as-calendar [d] (doto (Calendar/getInstance) (.setTime d)))
  
  java.util.Calendar
  (as-sql-timestamp [c] (Timestamp. (.getTimeInMillis c)))
  (as-unix-timestamp [c] (.getTimeInMillis c))
  (as-date [c] (.getTime c)) 
  (as-calendar [c] c)
  )

(def ^:private dateformat (per-thread-singleton #(SimpleDateFormat. "yyyy-MM-dd HH:mm:ss,SSS")))
(def ^:private dateonlyformat (per-thread-singleton #(SimpleDateFormat. "yyyy-MM-dd")))


(defn parse-time [^String s]
  (.getTime (.parse ^SimpleDateFormat (dateformat) s)))

(defn time-to-string 
  "Format milliseconds into format \"yyyy-MM-dd HH:mm:ss,SSS\""
  [t]
  (.format ^SimpleDateFormat (dateformat) (as-date t)))

(defn time-to-date-string 
  "Format milliseconds into format \"yyyy-MM-dd\""
  [t]
  (.format ^SimpleDateFormat (dateonlyformat) (as-date t)))

(defn date-string 
  "Get date as string with format yyyyMMdd."
  ([]
    (date-string (java.util.Date.)))
  ([date]
    (.format ^SimpleDateFormat (SimpleDateFormat. "yyyyMMddHHmm") date)))

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
        i2s (fn [i] (if (< i 10) (str "0" i) (str i)))
        ms2s (fn [i] (cond (< i 10) (str "00" i) 
                           (< i 100) (str "0" i) 
                           :else (str i)))]
    (reduce (fn [s [k unit]] 
              (if (< 0 (k m)) (str s (i2s (k m)) unit) s)) 
              "" 
              (partition 2 [:years \y :weeks \w :days \d :hours \h :minutes \m :seconds \s ;:milliseconds "ms"
                            ]))
            #_(str (i2s (:days m)) \d (i2s (:hours m)) \h (i2s (:minutes m)) \m (i2s (:seconds m)) \s (ms2s (:milliseconds m)) "ms")))

(defn dates-seq 
  "Create sequence of dates."
  ([start-date] (dates-seq start-date (Date.)))
  ([start-date end-date] 
    (when (and start-date end-date) 
      (let [start-date (as-date start-date)
            end-date (as-date end-date)
            cal (doto (Calendar/getInstance)
                  (.setTime start-date);
                  (.add Calendar/DAY_OF_MONTH 1))
            date (.getTime cal)]
        (when (.before start-date end-date) 
          (lazy-seq
            (cons start-date (dates-seq date end-date))))))))