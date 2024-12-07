(require '[clojure.string :as str])
(require '[clojure.set :as set])

(defn read-input [file]
  (->> (slurp file)
       (str/split-lines)
       (filter #(seq %))
       (partition-by #(str/includes? % "|"))))

(defn parse-rules [input]
  (let [rules (first input)]
    (->> (map #(str/split % #"\|") rules)
         (map #(hash-map (first %) #{(second %)}))
         (apply (partial merge-with set/union)))))

(defn parse-updates [input]
  (let [updates (second input)]
    (map #(str/split % #",") updates)))

(defn update-is-valid? [rules, update]
  (->> (partition-all (count update) 1 update)
       (mapcat #(map (fn [x] [(first %) x]) (rest %)))
       (some #(contains? (get rules (second %)) (first %)))
       (not)))

(defn filter-valid-updates [rules, updates] 
  (filter #(update-is-valid? rules %) updates))

(defn find-middle-page [update]
  (let [update-length (count update)]
    (Integer/parseInt (nth update (quot update-length 2)))))

(defn find-valid-middle-pages-sum [input]
  (let [rules (parse-rules input)
        updates (parse-updates input)
        valid-updates (filter-valid-updates rules updates)]
    (reduce + (map find-middle-page valid-updates))))

(println (find-valid-middle-pages-sum (read-input "Day5/input.txt")))

(defn sort-update-with-rules [rules, update]
  (let [filtered-rules (select-keys rules update)]
    (loop [sorted-updates []
           remaining-updates (set update)
           remaining-rules filtered-rules]
      (if (empty? remaining-updates)
        sorted-updates
        (let [all-rule-vals (apply set/union (vals remaining-rules))
              next-element (first (apply disj remaining-updates all-rule-vals))]
          (recur (conj sorted-updates next-element) (disj remaining-updates next-element) (dissoc remaining-rules next-element)))))))

(defn filter-invalid-updates [rules, updates] 
  (filter #(not (update-is-valid? rules %)) updates))

(defn find-invalid-middle-pages-sum [input]
  (let [rules (parse-rules input)
        updates (parse-updates input)
        invalid-updates (filter-invalid-updates rules updates)]
    (->> (map #(sort-update-with-rules rules %) invalid-updates)
         (map #(find-middle-page %))
         (reduce +))))

(println (find-invalid-middle-pages-sum (read-input "Day5/input.txt")))
