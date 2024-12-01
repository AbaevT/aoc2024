(require '[clojure.string :as str])

(defn read-input [file]
  (->> (slurp file)
       (str/split-lines)
       (map #(str/split % #"\s+"))
       (map (fn [line] (map #(Integer/parseInt %) line)))))

(defn total-distance [file]
  (let [input (read-input file)]
    (reduce + (map abs (map - (sort (map first input)) (sort (map second input)))))))

(println (total-distance "1/input.txt"))
