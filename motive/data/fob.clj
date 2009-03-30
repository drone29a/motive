(ns motive.data.fob
  (:use clojure.contrib.str-utils 
        clojure.contrib.duck-streams
        clojure.contrib.import-static
        motive.util)
  (:import (Jama Matrix)
           (com.jme.math Vector3f Matrix3f Quaternion)))

(import-static java.lang.Math sin cos sqrt atan2 asin)

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

;; Conversion for position in Flock of Birds system to jMonkeyEngine system.
(defmulti fob->jme (fn [& params] (let [param-count (count params)]
                                    (cond (= 1 param-count) :point
                                          (= 2 param-count) :vector
                                          :else :chain))))
(defmethod fob->jme :point
  [{:keys [x y z]}]
  (sorted-map :x y :y (- z) :z (- x)))
;; (defmethod fob->jme :vector
;;   [[a b]]
;;   (let [v (apply #(.subtract %1 %2) (map vec->Vector3f [b a]))]
;;     .))


(defn initial-poses
  "From a set of sensor positions ordered from heaviest to lightest, 
return the vector representing the starting poses of all limbs on a chain."
  [positions]
  (let [positions (map (fn [ps] (map #(% (fob->jme ps)) [:x :y :z])) positions)]
    (map (fn [start end] 
           (.normalize (.subtract (vec->Vector3f end) (vec->Vector3f start))))
         positions (rest positions))))

(defn euler->mat
  "Convert euler angles to rotation matrix."
  [{alpha :alpha beta :beta gamma :gamma}]
  (let [alpha (* alpha (/ Math/PI 180.0))
        beta (* beta (/ Math/PI 180.0))
        gamma (* gamma (/ Math/PI 180.0))
        Az (apply (fn [] (let [Az (make-array Float/TYPE 3 3)]
                           (aset Az 0 0 (float (cos alpha)))
                           (aset Az 0 1 (- (float (sin alpha))))
                           (aset Az 0 2 0)
                           (aset Az 1 0 (float (sin alpha)))
                           (aset Az 1 1 (float (cos alpha)))
                           (aset Az 1 2 0)
                           (aset Az 2 0 0)
                           (aset Az 2 1 0)
                           (aset Az 2 2 1)
                           Az)))
        Ay (apply (fn [] (let [Ay (make-array Float/TYPE 3 3)]
                           (aset Ay 0 0 (float (cos beta)))
                           (aset Ay 0 1 0)
                           (aset Ay 0 2 (float (sin beta)))
                           (aset Ay 1 0 0)
                           (aset Ay 1 1 1)
                           (aset Ay 1 2 0)
                           (aset Ay 2 0 (- (float (sin beta))))
                           (aset Ay 2 1 0)
                           (aset Ay 2 2 (float (cos beta)))
                           Ay))) 
        Ax (apply (fn [] (let [Ax (make-array Float/TYPE 3 3)]
                           (aset Ax 0 0 1)
                           (aset Ax 0 1 0)
                           (aset Ax 0 2 0)
                           (aset Ax 1 0 0)
                           (aset Ax 1 1 (float (cos gamma)))
                           (aset Ax 1 2 (- (float (sin gamma))))
                           (aset Ax 2 0 0)
                           (aset Ax 2 1 (float (sin gamma)))
                           (aset Ax 2 2 (float (cos gamma)))
                           Ax)))
        Rz (doto (Matrix3f.) (.set Az))
        Ry (doto (Matrix3f.) (.set Ay))
        Rx (doto (Matrix3f.) (.set Ax))]
    (.mult (.mult Rz Ry) Rx)))

(defn euler->mat*
  [{:keys [alpha beta gamma]}]
  (let [z (* alpha (/ Math/PI 180.0))
        y (* beta (/ Math/PI 180.0))
        x (* gamma (/ Math/PI 180.0))]
    (doto (Matrix3f.) 
      (.set 0 0 (* (cos y) (cos z)))
      (.set 0 1 (* (cos y) (sin z)))
      (.set 0 2 (- (sin y)))
      (.set 1 0 (+ (* (- (cos x)) (sin z)) (* (sin x) (sin y) (cos z))))
      (.set 1 1 (+ (* (cos x) (cos z)) (* (sin x) (sin y) (sin z))))
      (.set 1 2 (* (sin x) (cos y)))
      (.set 2 0 (+ (* (sin x) (sin z)) (* (cos x) (sin y) (cos z))))
      (.set 2 1 (+ (* (- (sin x)) (cos z)) (* (cos x) (sin y) (sin z))))
      (.set 2 2 (* (cos x) (cos y))))))

(defn mat->euler
  [m]
  {:alpha (atan2 (- (.get m 2 0)) (.get m 0 0))
   :beta (atan2 (- (.get m 1 2)) (.get m 1 1))
   :gamma (asin (.get m 1 0))})

(defn mat->euler*
  [m]
  {:beta (asin (.get m 2 0))})

(defn mat->quat
  "Convert rotation matrix to quaternion."
  [#^Matrix3f R]
  ;;   (let [qw (/ (sqrt (+ 1 (.get R 0 0) (.get R 1 1) (.get R 2 2))) 2)
  ;;         qx (/ (- (.get R 2 1) (.get R 1 2)) (* 4 qw))
  ;;         qy (/ (- (.get R 0 2) (.get R 2 0)) (* 4 qw))
  ;;         qz (/ (- (.get R 1 0) (.get R 0 1)) (* 4 qw))]
  ;;     {:w qw :x qx :y qy :z qz})
  (.fromRotationMatrix (Quaternion.) R))

(defn make-rotation-fn 
  "Takes a collection of rotation matrices and the sample rate for the data,
returns a function that will provide the rotation that occurs between a
start and end time."
  [rots sample-rate]
  (let [quats (map mat->quat rots)]
    (fn [start-time end-time]
      (let [start-idx (quot start-time sample-rate)
            start-interp-amt (/ (rem start-time sample-rate) 100.0)
            start-quat (.slerp (Quaternion.) 
                               (nth quats start-idx)
                               (nth quats (inc start-idx))
                               start-interp-amt)
            end-idx (quot end-time sample-rate)
            end-interp-amt (/ (rem end-time sample-rate) 100.0)
            end-quat (.slerp (Quaternion.) 
                               (nth quats end-idx)
                               (nth quats (inc end-idx))
                               end-interp-amt)]
        (.mult (.inverse start-quat) end-quat)))))

(defn make-rotation-generator
  "Takes a function that produces rotations and returns it wrapped
by a closure that will keep track of the start time for the next
rotation.  This closure takes a time step as its argument and returns
the rotation that occurs between start-time and start-time + time-step."
  [rot-fn]
  (let [start (atom 0)]
    (fn [step]
      (rot-fn @start (swap! start + step)))))

(defn make-rot-seq
  "Make a sequence of relative rotations for a joint given
orientations for the start and end sensors.

Use this to find the joint rotations from two sensors' rotations."
  [start-rots end-rots]
  (let [initial-rot-between (.mult (.invert (first end-rots)) (first start-rots))]
    (reduce (fn [abs-rots rel-rot]
              (conj abs-rots (.mult rel-rot (last abs-rots))))
            [(doto (Matrix3f.) .loadIdentity)]
            (map (fn [start-rot end-rot]
                   (let [rot-between (.mult (.invert end-rot) start-rot)
                         rot-diff (.mult (.invert initial-rot-between) rot-between)]
                     rot-diff))
                 start-rots end-rots))))

;;; Stuff for REPL
(def data (load-data "/Users/matt/src/motive/data/fob/2008_06_09/001.dat"))
(def sensor-a (map euler->mat (take-nth 3 data)))
(def sensor-b (map euler->mat (take-nth 3 (rest data))))
(def sensor-b (map euler->mat (take-nth 3 (rest (rest data)))))
