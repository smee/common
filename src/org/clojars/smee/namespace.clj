(ns org.clojars.smee.namespace
  (:require [clojure.set :as s]
            [clojure.string :as string]))


(defn referred-ns 
  "Set of all namespaces referred to by a-namespace."
  [a-namespace]
     (disj (set (map #(. % ns) (filter var? (map second (ns-map a-namespace))))) a-namespace))

(defn dependency-tree 
  "Show an adjacency lists (as a map) of references between namespaces."
  [a-namespace] 
  (loop [result {}, ns-to-go [a-namespace]]
    (if (empty? ns-to-go)
      (into {} (for [[k vs] result] [(ns-name k) (map ns-name vs)]))
      (let [updated (reduce #(assoc % %2 (referred-ns %2)) result ns-to-go)
            known (keys updated)
            referred (apply concat (vals updated))]
        (recur updated (s/difference (set referred) (set known)))))))

(defn google-image-url 
  "Print a url to google static charts that visualizes a tree."
  [tree]
  (let [q #(str \" % \")
        url "https://chart.googleapis.com/chart?cht=gv&chl=strict digraph{%s}"
        edges (apply str (map (fn [[k vs]] (str (q k) "->{" (string/join " " (map q vs)) "};")) tree))]
    (println (format url edges))))

(comment 
  (google-image-url (dependency-tree *ns*))
  )