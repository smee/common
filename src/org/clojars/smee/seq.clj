(ns 
  ^{:author "Steffen Dienst",
    :doc "useful sequence handling functions"}
  org.clojars.smee.seq
  (:use
    [org.clojars.smee.time :only (millis-to-time-units)]
    ))

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


(defn bf-tree-seq ;;FIXME bug: the depth is associated with each node, not with each invocation of walk!
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