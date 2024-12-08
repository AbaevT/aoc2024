(require '[clojure.string :as str])

(defn read-input [file]
  (->> (slurp file)
       (str/split-lines)
       (map vec)
       (vec)))

(def guard-direction {\v [inc identity] \< [identity dec] \^ [dec identity] \> [identity inc]})

(def rotate-guard {\v \< \< \^ \^ \> \> \v})

(defn find-guard [input]
  (->> (keep-indexed
        (fn [x row] (seq (keep-indexed
                          (fn [y symbol] (when (contains? guard-direction symbol) [x y symbol]))
                          row)))
        input)
       (flatten)))

(defn get-next-tile-coord [guard-x guard-y guard-symbol]
  (let [[fx fy] (guard-direction guard-symbol)]
    [(fx guard-x) (fy guard-y)]))

(defn move-guard [input guard-x guard-y guard-symbol]
  (let [[next-tile-x next-tile-y] (get-next-tile-coord guard-x guard-y guard-symbol)
        next-tile-value (get-in input [next-tile-x next-tile-y])]
    (cond
      (= next-tile-value nil) [(assoc-in input [guard-x guard-y] guard-symbol) nil nil nil]
      (= next-tile-value \#) (if (= (get-in input [guard-x guard-y]) (rotate-guard guard-symbol))
                               [nil nil nil nil]
                               [input guard-x guard-y (rotate-guard guard-symbol)])
      :else [(assoc-in input [guard-x guard-y] guard-symbol) next-tile-x next-tile-y guard-symbol])))

(defn move-guard-until-leaving [input]
  (loop [input input
         [guard-x guard-y guard-symbol] (find-guard input)]
    (if (= guard-symbol nil)
      input
      (let [[new-input new-guard-x new-guard-y new-guard-symbol] (move-guard input guard-x guard-y guard-symbol)]
        (recur new-input [new-guard-x new-guard-y new-guard-symbol])))))

(defn count-area [input]
  (->> (move-guard-until-leaving input)
       (flatten)
       (filter #(contains? guard-direction %))
       (count)))

(println (count-area (read-input "Day6/input.txt")))

(defn put-obstacle [input x y]
  (move-guard-until-leaving (assoc-in input [x y] \#)))

(defn count-obstacles [input]
  (let [guard-start (find-guard input)
        guard-input (move-guard-until-leaving input)]
    (reduce +
            (for [x (range (count input))
                  y (range (count (first input)))]
              (if
               (and
                (not
                 (and
                  (= x (first guard-start))
                  (= y (second guard-start))))
                (contains? guard-direction (get-in guard-input [x y])))
                (if (= nil (put-obstacle input x y)) 1 0)
                0)))))

(println (count-obstacles (read-input "Day6/input.txt")))
