(ns 
  ^{:author "Steffen Dienst",
    :doc "useful map processing functions"}
  org.clojars.smee.map)

(defn map-values 
  "Change all values or all keys and values by applying a function to each of them."
  ([vf m] (map-values identity vf m))
  ([kf vf m]
  (into (empty m) (for [[k v] m] [(kf k) (vf v)]))))


(defn remove-empty-values 
  "Remove all key-values where value is empty."
  [m]
  (into (empty m) (for [[k v] m :when (not (empty? v))] [k v])))

(defn sort-by-value
  "Sort map by values. If two values are equal, sort by keys. Sort order may be 
:ascending or :descending"
  ([my-map] (sort-by-value my-map :ascending))
  ([my-map sort-order]
    (into 
      (sorted-map-by (fn [key1 key2] 
                       (let[val-res (compare (get my-map key1) (get my-map key2))
                            res (if (zero? val-res)
                                  (compare key1 key2)
                                  val-res)]
                         (if (= :ascending sort-order)
                           res
                           (* -1 res))))) 
      my-map)))

(defn mapp
  "Use this instead of (partial map xy)"
  ([f] (partial map f))
  ([f x & args]
     (apply map (partial f x)
            args )))
  
(defn mapc [& args]
  "From: http://erl.nfshost.com/2011/05/22/map-mapp-and-mapc/
  Examples: 
     user> (mapc inc sq inc (range 1 6))
     (5 10 17 26 37)
	 user> ((mapc sq inc) (range 1 6))
     (4 9 16 25 36)"
  (let [[fns xs] (partition-by fn? args)
        g (apply comp fns)]
    (if (empty? xs)
      (partial map g)
      (apply map g xs ))))

(defn pmapcat [f & colls]
  (apply concat (apply pmap f colls)))

(defn pmap-chunked 
  "Split all collections `colls` into chunks of length `n` and maps `f` over each chunk in parallel.
It is most useful when each call to `f` is quite fast so the overhead of parallelizing would be to big. By
splitting the inputs into chunks that get processed in parallel, it should result in a performance boost.
This functions is not lazy!"
  [n f & colls]
  (let [chunked-colls (map (partial partition-all n) colls)]
    (apply concat (apply pmap (fn [& colls] (doall (apply map f colls))) chunked-colls))))

(defn dissoc-where-v 
  "Remove all mappings [ k v] where (f v) is logically true."
  [f m]
  (into (empty m) (for [[k v] m] (when (not (f v)) [k v]))))

(defn reverse-map
  "Reverse the keys/values of a map"
  [m]
  (into (empty m) (map (fn [[k v]] [v k]) m)))

(defn deep-merge-with
  "Like merge-with, but merges maps recursively, applying the given fn
  only when there's a non-map at a particular level.

  (deepmerge + {:a {:b {:c 1 :d {:x 1 :y 2}} :e 3} :f 4}
               {:a {:b {:c 2 :d {:z 9} :z 3} :e 100}})
  -> {:a {:b {:z 3, :c 3, :d {:z 9, :x 1, :y 2}}, :e 103}, :f 4}"
  [f & maps]
  (apply
    (fn m [& maps]
      (if (every? map? maps)
        (apply merge-with m maps)
        (apply f maps)))
    maps))

(defn flatten-keys 
  "(flatten-keys {:z 1 :a 9 :b {:c Double/NaN :d 1 :e 2 :f {:g 10 :i 22}}})
=> {[:z] 1, [:a] 9, [:b :c] Double/NaN, [:b :d] 1, [:b :e] 2, [:b :f :g] 10, [:b :f :i] 22} 
  "
  ([m] (flatten-keys {} [] m))
  ([a ks m]
    (if (map? m)
      (reduce into (map (fn [[k v]] (flatten-keys a (conj ks k) v)) (seq m)))
      (assoc a ks m))))

(defmacro transform-map
  "Reduce an associative datastructure into a new one. Assumes that `input` can be transient."
  {:style/indent 2}
  [input [k v] & body]
  `(let [input# ~input]
     (persistent!
      (reduce-kv (fn [m# ~k ~v]
                   (assoc! m# ~k ~@body))
                 (transient (empty input#))
                 input#))))
