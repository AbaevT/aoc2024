(require '[clojure.string :as str])

(defn read-input [file]
  (->> (slurp file)
       (str/split-lines)
       (map vec)
       (vec)))

(def directions 
  (list dec identity inc))
(def all-directions 
  (for [dv directions
        dh directions] 
    (list dv dh)))

(defn count-words-for-coord [input, word, x, y]
  (let [word-length (count word)]
    (->> all-directions
         (map #(list (take word-length (iterate (first %) x)) (take word-length (iterate (second %) y))))
         (map #(map (fn [x y] (get-in input [x y])) (first %) (second %)))
         (filter #(= word (apply str %)))
         (count))))

(defn count-xmas-words [input, word, counter]
  (let [x (count input)
        y (count (first input))]
    (reduce + (for [i (range x) j (range y)]
               (counter input word i j)))))

(printf "xmas words: %s" (count-xmas-words (read-input "Day4/input.txt") "XMAS" count-words-for-coord))
(newline)

(defn count-x-mas-for-coord [input, word, x, y]
  (let [words #{"MAS" "SAM"}]
    (if
     (and
      (->> [[(dec x) (dec y)] [x y] [(inc x) (inc y)]]
           (map #(get-in input [(first %) (second %)]))
           (apply str)
           (contains? words))
      (->> [[(dec x) (inc y)] [x y] [(inc x) (dec y)]]
           (map #(get-in input [(first %) (second %)]))
           (apply str)
           (contains? words))) 1 0)))

(printf "x-mas words: %s" (count-xmas-words (read-input "Day4/input.txt") "XMAS" count-x-mas-for-coord))
