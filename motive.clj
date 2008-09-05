(ns motive)
(load-resources "utils.clj"
                "quat.clj")


;; Rotations are the primitive movement in rigid limb motion
(defstruct rotation :joint :quat :duration :velocity)

;; Movement
;; A set of rotations.
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
(defn joint-child-pos
  "Find the position of the next joint in the limb."
  [parent-joint parent-pos]
  
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