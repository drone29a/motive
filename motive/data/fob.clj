(ns motive.data.fob
  (:use clojure.contrib.str-utils 
        clojure.contrib.duck-streams
        clojure.contrib.import-static
        motive.util)
  (:import (Jama Matrix)))

(import-static java.lang.Math sin cos sqrt)

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

(defn euler->mat
  "Convert euler angles to rotation matrix."
  [{alpha :alpha beta :beta gamma :gamma}]
  (let [Az (apply (fn [] (let [Az (make-array Double/TYPE 3 3)]
                           (aset Az 0 0 (cos alpha))
                           (aset Az 0 1 (- (sin alpha)))
                           (aset Az 0 2 0)
                           (aset Az 1 0 (sin alpha))
                           (aset Az 1 1 (cos alpha))
                           (aset Az 1 2 0)
                           (aset Az 2 0 0)
                           (aset Az 2 1 0)
                           (aset Az 2 2 1)
                           Az)))
        Ay (apply (fn [] (let [Ay (make-array Double/TYPE 3 3)]
                           (aset Ay 0 0 (cos beta))
                           (aset Ay 0 1 0)
                           (aset Ay 0 2 (- (sin beta)))
                           (aset Ay 1 0 0)
                           (aset Ay 1 1 1)
                           (aset Ay 1 2 0)
                           (aset Ay 2 0 (sin beta))
                           (aset Ay 2 1 0)
                           (aset Ay 2 2 (cos beta))
                           Ay))) 
        Ax (apply (fn [] (let [Ax (make-array Double/TYPE 3 3)]
                           (aset Ax 0 0 1)
                           (aset Ax 0 1 0)
                           (aset Ax 0 2 0)
                           (aset Ax 1 0 0)
                           (aset Ax 1 1 (cos gamma))
                           (aset Ax 1 2 (- (sin gamma)))
                           (aset Ax 2 0 0)
                           (aset Ax 2 1 (sin gamma))
                           (aset Ax 2 2 (cos gamma))
                           Ax)))
        Rz (Matrix. Az 3 3)
        Ry (Matrix. Ay 3 3)
        Rx (Matrix. Ax 3 3)]
    (.times Rz (.times Ry Rx))))

(defn mat->quat
  "Convert rotation matrix to quaternion."
  [#^Matrix R]
  (let [qw (/ (sqrt (+ 1 (.get R 0 0) (.get R 1 1) (.get R 2 2))) 2)
        qx (/ (- (.get R 2 1) (.get R 1 2)) (* 4 qw))
        qy (/ (- (.get R 0 2) (.get R 2 0)) (* 4 qw))
        qz (/ (- (.get R 1 0) (.get R 0 1)) (* 4 qw))]
    {:w qw :x qx :y qy :z qz}))
