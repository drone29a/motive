(ns motive.quat)

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

(defn quat-norm
  "Normalize a quaternion."
  [q]
  (let [a (:a q)
        b (:b q)
        c (:c q)
        d (:d q)
        n (. Math sqrt (+  (* a a) 
                           (* b b)
                           (* c c)
                           (* d d)))]
    (struct quat 
            (/ a n)
            (/ b n)
            (/ c n)
            (/ d n))))

(defn quat-conj
  "Conjugate a quaternion."
  [q]
  (struct quat
          (:a q)
          (- (:b q))
          (- (:c q))
          (- (:d q))))

(defn quat-rotate 
  "Rotate a vector by the given quaternion."
  [q v]
  (let [a (:a q)
        b (:b q)
        c (:c q)
        d (:d q)
        t2 (* a b)
        t3 (* a c)
        t4 (* a d)
        t5 (* (- b) b)
        t6 (* b c)
        t7 (* b d)
        t8 (* (- c) c)
        t9 (* c d)
        t10 (* (- d) d)
        v1 (nth v 0)
        v2 (nth v 1)
        v3 (nth v 2)
        v1new (+ (* 2 (+ (* v1 (+ t8 t10))
                         (* v2 (- t6 t4))
                         (* v3 (+ t3 t7))))
                 v1)
        v2new (+ (* 2 (+ (* v1 (+ t4 t6))
                         (* v2 (+ t5 t10))
                         (* v3 (- t9 t2))))
                 v2)
        v3new (+ (* 2 (+ (* v1 (- t7 t3))
                         (* v2 (+ t2 t10))
                         (* v3 (+ t5 t8))))
                 v3)]
    [v1 v2 v3]))
