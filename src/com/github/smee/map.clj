(ns 
  ^{:author "Steffen Dienst",
    :doc "useful map processing functions"}
  com.github.smee.map)

(defn map-values 
  "Change all map values by applying f to each one."
  [f m]
  (into {} (for [[k v] m] [k (f v)])))

(defn mapmap
  "Apply kf and vf to a sequence, s, and produce a map of (kf %) to (vf %).
from http://tech.puredanger.com/2010/09/24/meet-my-little-friend-mapmap/"
  ([vf s]
     (mapmap identity vf s))
  ([kf vf s]
     (zipmap (map kf s)
              (map vf s))))

(defn remove-empty-values 
  "Remove all key-values where value is empty."
  [m]
  (into {} (for [[k v] m :when (not (empty? v))] [k v])))

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

(defn dissoc-where-v 
  "Remove all mappings [ k v] where (f v) is logically true."
  [f m]
  (into {} (for [[k v] m] (when (not (f v)) [k v]))))
