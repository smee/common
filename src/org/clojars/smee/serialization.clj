(ns org.clojars.smee.serialization
  (:use [clojure.pprint :only [pprint]]
        [clojure.java.io :only (file reader writer)])
  (:import [java.io File FileWriter FileReader PushbackReader]))

(defn serialize 
  "Serialize the native clojure datastructure obj to file."
  ([file-name obj] (serialize file-name obj :append false))  
  ([file-name obj & opts]
    (let [{:keys [append pretty]} (if (map? (first opts)) (first opts) (apply hash-map opts))
          prn (if pretty clojure.pprint/pprint clojure.core/prn)] 
      (with-open [w (writer (file file-name) :append append)] 
        (binding [*out* w 
                  *print-length* nil] 
          (if (seq? obj)
            ;;serialize big sequences without retaining its head...
            (do
              (print \()
              (dorun (map prn obj)) ;;make sure members of sequences may be garbage collected upon realization
              (print \)))  
            (prn obj)))))))


(defn deserialize [f]
  "Read clojure datastructure from file."
  (with-open [r (PushbackReader. (reader f))]
    (read r)))

(defn deserialize-all [f]
  "Read clojure datastructures from file, returns a seq of all contents."
  (with-open [r (PushbackReader. (reader f))]
    (loop [r r, res []]
      (let [token (read r false :eof)]
        (if (= :eof token)
          res
          (recur r (conj res token)))))))
