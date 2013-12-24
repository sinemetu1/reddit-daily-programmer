(ns reddit-daily-programmer.core)

(defn easy-146
  [& args]
  (when args
    (let [str-args      (.split (first args) " ")
          num-of-sides  (Integer/valueOf (first str-args))
          circum-radius (Double/valueOf  (last str-args))
          side-length   (* (* (Math/sin (/ Math/PI num-of-sides)) 2) circum-radius)]
      (format "%.3f" (* num-of-sides side-length)))))

;; the-node = 0 1 0 0 1 1 0 0 0 0
;; returns  = [2, 5, 6]
(defn- get-connected-nodes
  [the-node adj-arr]
  (let [values (into [] (clojure.string/split the-node #"\s+"))]
    (loop [index       0
           curr-value  (first values)
           rest-values (rest values)
           to-ret      '[]]
      (if curr-value
        (recur (inc index)
               (first rest-values)
               (rest rest-values)
               (if (= curr-value "1")
                 (conj to-ret index)
                 to-ret))
        to-ret))))

(defn- distinct-list
  [a-list]
  (loop [to-ret         '[]
         curr-value     (first a-list)
         rest-values    (rest a-list)]
    (if curr-value
      (recur (if (some #(= curr-value %) to-ret)
               to-ret
               (conj to-ret curr-value))
             (first rest-values)
             (rest rest-values))
      to-ret)))

(defn- get-radius-from
  [node adj-arr number-of-nodes]
  (loop [level-map  {(keyword (str node)) 0}
         radius     ((keyword (str node)) level-map)
         nodes      [(Integer/valueOf node)]
         curr-node  (first nodes)
         node-count (count nodes)
         idx        0]
    ;(println "level-map:" level-map
             ;"radius:" radius "node-count:" node-count "curr-node:" curr-node)
    (if (or (>= node-count number-of-nodes)
            (>= radius number-of-nodes))
      (apply max (map val level-map))
      (let [new-nodes     (distinct-list
                            (into nodes
                                  (get-connected-nodes (get adj-arr curr-node) adj-arr)))
            new-count     (count new-nodes)
            new-level-map (into level-map
                                (for [a-node (drop node-count new-nodes)]
                                  [(keyword (str a-node)) (inc radius)]))
            new-radius    ((keyword (str curr-node)) new-level-map)
            new-idx       (inc idx)]
        (println "new-nodes:" new-nodes "new-level-map:" new-level-map "new-count:" new-count
                 "new-radius:" new-radius)
        (recur new-level-map
               new-radius
               new-nodes
               (nth new-nodes new-idx)
               new-count
               new-idx)))))

(defn interm-146
  [node-num adj-str]
  (when node-num
    (let [adj-arr (into [] (clojure.string/split adj-str #"\n\s+"))]
      (loop
        [node-idx      (dec (Integer/valueOf node-num))
         lowest-radius node-idx]
        (if (< node-idx 0)
          lowest-radius
          (recur (dec node-idx)
                 (let [radius (get-radius-from node-idx adj-arr (Integer/valueOf node-num))]
                   (println "node-idx" node-idx " radius:" radius)
                   (if (< radius lowest-radius)
                     radius
                     lowest-radius))))))))
