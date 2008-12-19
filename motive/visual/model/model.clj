(ns motive.visual.model
  (:use motive
        motive.visual)
  (:import (com.jme.bounding BoundingBox)
           (com.jme.scene.shape Box Cylinder AxisRods)
           (com.jme.scene Node)
           (com.jme.math Vector3f Quaternion)))

(defn left-arm-model
  "Construct a torso with left arm."
  []
  (let [forearm (Box. "forearm" (Vector3f. 0 0 0) 10 1.5 1.5)
        upperarm (Box. "upperarm" (Vector3f. 0 0 0) 8 1.8 1.8)
        torso (Box. "torso" (Vector3f. 0 0 0) 15 5 5)
        forearm-node (AxisRods. "forearmNode")
        upperarm-node (AxisRods. "uppearNode")
        torso-node (AxisRods. "torsoNode")
        model {:torso torso-node
               :upperarm upperarm-node
               :forearm forearm-node}]
    (doseq [shape (vals model)]
      (.setModelBound shape (BoundingBox.))
      (.updateModelBound shape))

    (.attachChild forearm-node forearm)
    (.attachChild upperarm-node upperarm)
    (.attachChild upperarm-node forearm-node)
    (.attachChild torso-node torso)
    (.attachChild torso-node upperarm-node)
    
    model))