(require '[clojure.string :as str])

(defn read-input [file]
  (->> (slurp file)
       (str/split-lines)
       (map #(str/split % #"\s+"))
       (map (fn [line] (map #(Integer/parseInt %) line)))))

(defn total-distance [file]
  (let [input (read-input file)]
    (reduce + (map abs (map - (sort (map first input)) (sort (map second input)))))))

(printf "total distance is: %s" (total-distance "Day1/input.txt"))
(newline)

(defn similarity [file]
  (let [input (read-input file)]
    (let [counts (group-by identity (map second input))]
      (reduce + (map #(* % (count (get counts % '()))) (map first input))))))

(printf "similarity is: %s" (similarity "Day1/input.txt"))
