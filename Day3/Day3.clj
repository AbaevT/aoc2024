(require '[clojure.string :as str])

(def mul-pattern #"mul\((\d+),(\d+)\)")

(defn read-input [file] (slurp file))

(defn find-matches [input] (re-seq mul-pattern input))

(defn get-mult-sum [input]
    (->> (find-matches input)
         (map #(drop 1 %))
         (map #(map Integer/parseInt %))
         (map #(apply * %))
         (apply +)))

(printf "mult sum is: %s" (get-mult-sum (read-input "Day3/input.txt")))
(newline)

(def do-pattern #"(don't\(\)[\s\S]*?((do\(\))|(\z)))")

(printf "enabled mult sum is: %s" (get-mult-sum (str/replace (read-input "Day3/input.txt") do-pattern "")))
