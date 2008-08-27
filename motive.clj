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

;; Quaternion as a + bj + ck + di
(defstruct quat :a :b :c :d)

(defn quat-new
  ([angle [x y z]]
     (let [a (. Math cos (/ angle 2))
           b (* x (. Math sin (/ angle 2)))
           c (* y (. Math sin (/ angle 2)))
           d (* z (. Math sin (/ angle 2)))]
       (struct quat a b c d))))

(defn quat-*
  "Multiply (combine) quaternions."
  ([q1 q2] 
     (let [a (+ (- (- (* (- (:b q1)) (:b q2))
                      (* (:c q1) (:c q2)))
                   (* (:d q1) (:d q2)))
                (* (:a q1) (:a q2)))
           b (+ (- (+ (* (:b q1) (:a q2))
                      (* (:c q1) (:d q2)))
                   (* (:d q1) (:c q2)))
                (* (:a q1) (:b q2)))
           c (+ (+ (+ (* (- (:b q1)) (:d q2))
                      (* (:c q1) (:a q2)))
                   (* (:d q1) (:b q2)))
                (* (:a q1) (:c q2)))
           d (+ (+ (- (* (:b q1) (:c q2))
                      (* (:c q1) (:b q2)))
                   (* (:d q1) (:a q2)))
                (* (:a q1) (:d q2)))]
       (struct quat a b c d))))

;; Rotations are the primitive movement in rigid limb motion
(defstruct rotation :joint :quat :duration :velocity)

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

;; Position
(defn next-joint-pos
  "Find the position of the next joint in the limb."
  [j]
  )

;; Scratch
(let [arm (limb-new '(:shoulder 5 :elbow 6 :wrist) [0 0 0])
      wave ()])

;; Operators
;; Still figuring this out but the notion is:
;; + :: simple x simple -> complex  where the new complex motion is two simple motions sequenced  (sequencing)
;; * :: simple x simple -> simple  quat multiplication, this is "blending" of motion  (blending)
;; + :: complex x simple -> complex  a new complex motion where the simple motion does not overlap an existing simple motion on the same joint  (compounding)
;; + :: complex x complex -> complex ??
;; * :: complex x complex -> complex ??