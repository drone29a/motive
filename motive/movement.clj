(ns motive.movement
  (:use [clojure.contrib.seq-utils :only [indexed]])
  (:import (com.jme.math Quaternion
                        Vector3f)
          (org.apache.commons.math.analysis SplineInterpolator
                                            UnivariateRealFunction)))

(defn axis 
  "Construct a Vector3f from x, y, z."
  [x y z]
  (Vector3f. (float x) (float y) (float z)))

(defn quat
  "Construct a jME quaternion from angle and axis."
  [angle #^Vector3f axis]
  (-> (Quaternion.) (.fromAngleAxis angle axis)))

(defn vel-linear
  [x]
  (identity x))

(defn vel-interp
  "From a sequence of accelerations build up a function that maps [0.0, 1.0] to
the amount of interpolation that should be applied for a joint rotation."
  [accel-pts]
  (let [vel-pts (reduce (fn [acc x]
                          (conj acc (max 0 (+ (last acc) x))))
                        [(first accel-pts)] 
                        (rest accel-pts))
        interp-pts (reduce (fn [acc x]
                             (conj acc (max 0 (+ (last acc) x))))
                           [(first vel-pts)] 
                           (rest vel-pts))
        scaled-pts (cons 0 (map #(/ % (last interp-pts)) interp-pts))
        ys (let [arr (make-array Double/TYPE (count scaled-pts))]
             (doseq [[idx y] (indexed scaled-pts)]
               (aset arr idx (double y)))
             arr)
        sample-pts (reduce (fn [acc x] (conj acc (+ (last acc) x)))
                                      [0]
                                      (take (dec (count scaled-pts)) (repeat (/ 1 (dec (count scaled-pts))))))
        xs (let [arr (make-array Double/TYPE (count sample-pts))] 
             (doseq [[idx x] (indexed sample-pts)]
               (aset arr idx (double x)))
             arr)
        interp-fn (-> (SplineInterpolator.) (.interpolate xs ys))]
    (fn [#^Double x]
      (.value interp-fn (min 1.0 x)))))

;; Primitive movement in rigid limb motion
(defstruct simple-movement :quaternion :velocity :duration)

(defn simple
  [q v d]
  #^:simple-movement (struct-map simple-movement
                       :quaternion q
                       :velocity v
                       :duration d))

;;; Combine operations
;;; All combine operations preserve the attributes of the first movement
;;; except for the attribute being blended with the second movement.

(defn combine-rotation
  "Multiply the first movement's quaternion by the second.
Not commutative!"
  [q1 q2]
  (.mult (:quaternion q1) 
         (:quaternion q2)))

(defn combine-velocity
  "Blend two velocity functions."
  [v1 v2]
  (fn [t] (/ (+ (v1 t) 
                (v2 t)) 
             2.0)))

(defn combine-duration
  "Mean of durations."
  [d1 d2]
  (/ (+ (:duration d1)
        (:duration d2))
     2.0))

(defstruct super-movement :simple-movements :time-starts)

(defn insert
  "Insert a simple-movement into a super-movement."
  [super simple time-start]
  (struct-map super-movement
    :simple-movements (conj (:simple-movements super)
                            simple)
    :time-starts (assoc (:time-starts super)
                   simple time-start)))

;;; Applying super movements by providing a super-movement and a map
;;; of simple-movements to joints for all simple-movements in the super-movement