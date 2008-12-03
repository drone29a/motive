(ns motive.data.fob
  (:use clojure.contrib.str-utils 
        clojure.contrib.duck-streams))

(defstruct sensor-moment :id :x :y :z :alpha :beta :gamma)

(defn parse-line
  "Parse a line of FoB data."
  [line]
  (let [split-vals (re-split #"\s" line)
        [id x y z alpha beta gamma]  (cons (Integer/parseInt (first split-vals)) 
                                           (map #(Float/parseFloat %) (rest split-vals)))]
    (struct sensor-moment id x y z alpha beta gamma)))

(defn load-data
  "Parse a stream of FoB data"
  [src]
  (let [rdr (reader src)]
    (map parse-line (take-while #(not (nil? %)) (repeatedly #(.readLine rdr))))))

(defn partition-by-sensor
  "Partition sensor moments by sensor id."
  [sensor-data]
  (let [ids (sort < (set (map #(:id %) sensor-data)))]
    (map (fn [id] (filter (fn [moment] (= (:id moment) id)) sensor-data)) 
         ids)))

(defn relative-angles
  "Sequence of angle differences."
  [moment-seq]
  (map (fn [{a1 :alpha b1 :beta g1 :gamma}
            {a2 :alpha b2 :beta g2 :gamma}]
         {:alpha (- a2 a1) :beta (- b2 b1) :gamma (- g2 g1)})
       moment-seq (rest moment-seq)))
