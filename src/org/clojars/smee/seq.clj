(ns 
  ^{:author "Steffen Dienst",
    :doc "useful sequence handling functions"}
  org.clojars.smee.seq
  (:use
    [clojure.contrib.seq :only (indexed)]))

(defn distinct-by
  "Returns a lazy sequence of object with duplicates removed,
  where duplicates are defined by applying the function func to each item.
  Calling (distinct-by _ identity) is equivalent to (clojure.core/distinct _)."
  [func coll]
    (let [step (fn step [xs seen]
                 (lazy-seq
                   ((fn [[f :as xs] seen]
                      (when-let [s (seq xs)]
                        (let [f-val (func f)]
                          (if (contains? seen f-val) 
                            (recur (rest s) seen)
                            (cons f (step (rest s) (conj seen f-val)))))))
                     xs seen)))]
      (step coll #{})))

(defn seq-counter 
  "calls callback after every n'th entry in sequence is evaluated with current index as parameter."
  [sequence n callback]
  (map #(do (when (= (rem %1 n) 0) (callback %1)) %2) (iterate inc 1) sequence))

(defn unchunk 
  "Disable the chunking behaviour introduced in clojure 1.1"
  [s]
  (when (seq s)
    (lazy-seq
      (cons (first s)
        (unchunk (next s))))))

(defn index-filter [pred coll]
  (when pred
    (for [[idx elt] (indexed coll) :when (pred elt)] idx)))

(defn take-to-first
  "Returns a lazy sequence of successive items from coll up to
  and including the point at which it (pred item) returns true.
  pred must be free of side-effects."
  [pred coll]
  (lazy-seq
   (when-let [s (seq coll)]
       (if-not (pred (first s))
         (cons (first s) (take-to-first pred (rest s)))
         (list (first s))))))

(defn partition-when
  "Applies f to each value in coll, splitting it each time f returns
   true. Returns a lazy seq of lazy seqs."
  [f coll]
  (when-let [s (seq coll)]
  (lazy-seq
    (let [run (take-to-first f s)
          res (drop (count run) s)]
        (cons run (partition-when f res)))))) 

