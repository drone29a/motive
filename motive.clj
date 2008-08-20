(clojure/in-ns 'motive)
(clojure/refer 'clojure)

(defmacro init-array [type init-fn & dims]
  (let [idxs (map (fn [_] (gensym)) dims)
        ds (map (fn [_] (gensym)) dims)]
    `(let [a# (make-array ~type ~@dims)
           f# ~init-fn
           ~@(mapcat vector ds dims)]
       (dorun 
        (for ~(vec (mapcat #(list %1 (list `range %2)) idxs ds))
          (aset a# ~@idxs (f# ~@idxs))))
       a#)))

;; Quaternion
(defstruct quaternion :one :i :j :k)

;; Rotations are the primitive movement in rigid limb motion
(defstruct rotation :joint :axis :angle :duration :velocity)

(defn similar 
  "Returns true if the rotations are defined on the same joint and axis."
  [x y]
  (and (= (:joint x) (:joint y))
       (= (:axis x) (:axis y))))

(defn rotation-add 
  "Add rotations."
  [x y]
  (cond (or (nil? x) (nil? y))
        (or x y)
        (not (similar x y))
        (throw (Exception. (str "Incompatible joints or axes")))
        :else
        (struct-map rotation
          :joint (:joint x)
          :axis (:axis x)
          :angle (+ (:angle x) (:angle y)))))

(defn rotation-subtract
  "Subtract rotations."
  [x y]
  (if (not (similar x y))
    (throw (Exception. (str "Incompatible joints or axes")))
    (struct-map rotation
      :joint (:joint x)
      :axis (:axis x)
      :angle (- (:angle x) (:angle y)))))

;; Movement
;; A well-formed movement should have a set if rotations,
;; where no two rotations are similar.
(defstruct movement :rotations)

;; Joints
;; A name and orientation (alpha, beta, gamma).  Any constraints are handled externally.
(defstruct joint :name :orientation)

;; Limbs
;; Limbs are made up of rigid segments connected by joints.  They are placed in a coordinate space
;; and modified by movements.
(defstruct limb :joints :lengths :pos)

(defn limb-new
  "Convenience fn for defining a limb from a collection of alternating
   joint symbols and segment lengths."
  ([joints-lengths]
     (let [joints (take-nth 2 joints-lengths)
           lengths (take-nth 2 (rest joints-lengths))]
       (struct-map limb 
         :joints joints
         :lengths lengths)))
  ([joints-lengths pos]
     (assoc (limb-new joints-lengths) :pos pos)))

;; Scratch
(let [arm (limb-new '(:shoulder 5 :elbow 6 :wrist) [0 0 0])
      wave ()])