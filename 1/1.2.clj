(require '[clojure.string :as str])

(defn read-input [file]
  (->> (slurp file)
       (str/split-lines)
       (map #(str/split % #"\s+"))
       (map (fn [line] (map #(Integer/parseInt %) line)))))

(defn similarity [file]
  (let [input (read-input file)]
    (let [counts (group-by identity (map second input))]
    (reduce + (map #(* % (count (get counts % '()))) (map first input))))))

(println (similarity "1/input.txt"))
