(ns smucket.statistics)

(defn diff-maps [map1 map2]
  "Returns words that are in map1 that are not in map 2"
  (into {} (filter #(not (contains? map2 (first %))) map1))
  )

(defn compare-frequency-maps [map1 map2]
  (let [difference-map (apply conj (diff-maps map1 map2) (diff-maps map2 map1))
        count-differences (count difference-map)
        difference (/ count-differences (+ (count map1) (count map2)))]
    (println difference)))
