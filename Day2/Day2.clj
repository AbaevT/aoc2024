(require '[clojure.string :as str])

(defn read-input [file]
  (->> (slurp file)
       (str/split-lines)
       (map #(str/split % #"\s+"))
       (map (fn [line] (map #(Integer/parseInt %) line)))))

(defn report-is-safe [report]
  (let [pairs (partition 2 1 report)]
    (or 
     (every? #(<= 1 (apply - %) 3) pairs)
     (every? #(<= 1 (apply - (reverse %)) 3) pairs))))

(defn count-safe-reports [file]
  (let [input (read-input file)]
    (count (filter report-is-safe input))))

(printf "count of safe reports is: %s" (count-safe-reports "Day2/input.txt"))
(newline)

(defn short-reports [report]
  (->> (range (count report))
       (map #(concat (take % report) (drop (inc %) report)))))

(defn count-short-safe-reports [file]
  (let [input (read-input file)]
    (count (filter #(some report-is-safe (short-reports %)) input))))

(printf "count of now safe reports is: %s" (count-short-safe-reports "Day2/input.txt"))
