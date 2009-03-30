(ns motive.util
  (:import (com.jme.math Vector3f)))

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

(defn vec->Vector3f
  [[x y z]]
  (Vector3f. x y z))