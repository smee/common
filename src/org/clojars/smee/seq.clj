(ns 
  ^{:author "Steffen Dienst",
    :doc "useful sequence handling functions"}
  org.clojars.smee.seq
  (:use
    [org.clojars.smee.time :only (millis-to-time-units)]
    ))

(defn distinct-by
  "Returns a lazy sequence of object with duplicates removed,
  where duplicates are defined by comparing the result of applying func to each item.
  Calling (distinct-by identity _) is equivalent to (clojure.core/distinct _)."
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
  (map #(do (when (= (rem %1 n) 0) (callback %1)) %2) (iterate inc 0) sequence))

(defn wrap-time-estimator [total-count call-every sequence]
  (let [starttime (atom nil)
        callback (fn [i] (do
                           (if (not @starttime)
                             (reset! starttime (System/currentTimeMillis))
                             (let [now (System/currentTimeMillis)
                                   passed (- now @starttime)
                                   time-per-item (/ passed i)
                                   items-left (- total-count i)]
                               (println "#_\"processing" i "items took :" (millis-to-time-units passed) "\"")
                               (println "#_\"est. time for remaining" items-left "items :" (millis-to-time-units (* time-per-item items-left)) "\"")
                               (flush)))
                           ))]
    (seq-counter sequence call-every callback)))

(defn unchunk 
  "Disable the chunking behaviour introduced in clojure 1.1"
  [s]
  (when (seq s)
    (lazy-seq
      (cons (first s)
        (unchunk (next s))))))

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


(defn bf-tree-seq
  "Returns a lazy sequence of the nodes in a tree, via a breadth-first walk.
   branch? must be a fn of one arg that returns true if passed a node
   that can have children (but may not).  children must be a fn of one
   arg that returns a sequence of the children. Will only be called on
   nodes for which branch? returns true. Root is the root node of the
  tree."
  [branch? children root & [max-depth]]
  (let [max-depth (or max-depth Double/POSITIVE_INFINITY)
        walk (fn walk [depth & nodes]
               ;(println "depth " depth ", nodes" (mapcat keys nodes))
               (when nodes
                 (lazy-cat nodes
                     (when (< depth max-depth)
                       (->> nodes 
                         (mapcat children)
                         (apply walk (inc depth)))))))]
    (walk 0 root)))

(defn df-tree-seq 
  "see clojure.core/tree-seq. Takes an additional parameter max-depth that
specifies when to abort the traversal."
  [branch? children root & [max-depth]]
  (let [max-depth (or max-depth Double/POSITIVE_INFINITY)
        walk (fn walk [depth node]
               (when (and node (< depth max-depth))
                 (lazy-seq
                   (cons node
                         (when (branch? node)
                           (mapcat (partial walk (inc depth)) (children node)))))))]
    (walk 0 root)))

(defn first-max-key
  "Returns the first x for which (k x), a number, is greatest."
  {:static true}
  ([k x] x)
  ([k x y] (if (> (k x) (k y)) x y))
  ([k x y & more]
   (reduce #(max-key k %2 %1) (max-key k x y) more)))

(defn find-where 
  "Find the first element in `coll` where `(pred element)` returns truthy."
  [pred coll]
  (some #(when (pred %) %) coll))

(defn partition-by-equivalence 
  "Copied from http://www.learningclojure.com/2013/03/partition-by-equivalence.html
Example: 
    (partition-by-equivalence (fn [x y] (= (inc x) y)) [0 1 2  0 1 2 3 4 8 9 10])
    => ((0 1 2) (0 1 2 3 4) (8 9 10))"
  [f coll]
  (let [recaccacc (fn [f acc1 acc2 coll]
                    (if (empty? coll) (reverse (cons (reverse acc2) acc1))
                      (if (empty? acc2) (recur f acc1 (cons (first coll) acc2) (rest coll))
                        (if (f (first acc2) (first coll))
                          (recur f acc1 (cons (first coll) acc2) (rest coll))
                          (recur f (cons (reverse acc2) acc1) '() coll)))))]
    (recaccacc f '() '() coll)))


(defn partition-by-state 
  "Like partition-by but calls `f` with a accumulated value. `f` needs to take two parameters:
an accumulated value and a value of `coll`. The return value is either `nil` if a split should occur or the next value for the accumulator.
Example:
    (partition-by-state (fn [acc x] (let [acc (+ acc x)] (if (>= acc 10) nil) acc)) (range 100))
    => "
  [new-acc-fn split-fn acc coll]
  (let [rec-fn (fn rec-fn [res crnt-acc coll]
                 (lazy-seq 
                   (if (empty? coll)
                     (list res)
                     (let [new-acc (new-acc-fn crnt-acc (first coll))
                           split? (split-fn new-acc)]
                       (if split?
                         (cons res (rec-fn [(first coll)] (new-acc-fn acc (first coll)) (next coll)))
                         (rec-fn (conj res (first coll)) new-acc (next coll)))))))]
    (rec-fn [(first coll)] (new-acc-fn acc (first coll)) (next coll))))

;(binding [*print-length* 100 *print-level* 10] (println (partition-by-state + even? 0 (range 20))))