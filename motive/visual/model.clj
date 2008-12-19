(ns motive.visual.model
  (:use motive
        motive.visual)
  (:import (com.jme.bounding BoundingBox)
           (com.jme.scene.shape Cylinder AxisRods)
           (com.jme.scene Node)
           (com.jme.math Vector3f Quaternion)))

(defn left-arm-model
  "Construct a torso with left arm."
  []
  (let [forearm (Cylinder. "forearm" 32 32 1.5 10 true)
        upperarm (Cylinder. "upperarm" 32 32 1.8 8 true)
        torso (Cylinder. "torso" 32 32 5 15 true)
        elbow-joint (AxisRods. "elbow")
        shoulder-joint (AxisRods. "shoulder")
        torso-node (Node. "torso-node")
        upperarm-node (Node. "upperarm-node")
        model {:torso torso-node
               :upperarm upperarm-node
               :forearm forearm
               :elbow elbow-joint
               :shoulder shoulder-joint}]
    (doseq [shape (vals model)]
      (.setModelBound shape (BoundingBox.))
      (.updateModelBound shape))

    (.attachChild elbow-joint forearm)
    (.attachChild upperarm-node elbow-joint)
    (.attachChild upperarm-node upperarm)
    (.attachChild shoulder-joint upperarm)
    (.attachChild torso-node shoulder-joint)    
    (.attachChild torso-node torso)

    model))

(defn xyz->vector3f
  [[x y z]]
  (Vector3f. x y z))

(defn body-parts
  "Some assembly required."
  []
  (let [torso (Cylinder. "torso" 3 8 4 10 true)
        scales {:hips [0.8 0.8 0.5]
                :head [0.625 0.625 0.5]
                :right-upperarm [0.3 0.3 1]
                :left-upperarm [0.3 0.3 1]
                :right-forearm [0.25 0.25 0.9]
                :left-forearm [0.25 0.25 0.9]}
        parts (into {:torso torso} (map (fn [[name-kw scale-xyz]]
                                          (let [part-name (name name-kw)
                                                c (Cylinder. part-name 
                                                             (.getAxisSamples torso)
                                                             (.getRadialSamples torso)
                                                             (.getRadius torso)
                                                             (.getHeight torso)
                                                             true)]
                                            (.setLocalScale c (xyz->vector3f scale-xyz))
                                            [name-kw c])) scales))]
    ;; Flip 'em up
    (doseq [part (vals parts)] 
      (.setLocalRotation part (.fromAngleAxis (Quaternion.) (/ Math/PI 2) (Vector3f. 1 0 0)))
      (.updateModelBound part))
    parts))

(defn build-body
  "Assemble!"
  []
  (let [parts (body-parts)
        joints {:elbow (AxisRods. "elbow")
                :shoulder (AxisRods. "shoulder")}
        part-nodes {:torso-node (AxisRods. "torso-node")
                    :left-upperarm-node (AxisRods. "left-upperarm-node")
                    :right-upperarm-node (AxisRods. "right-upperarm-node")}]
    
    (.setLocalTranslation (:torso parts) (Vector3f. 0 10 0))
    (.setLocalTranslation (:hips parts) (Vector3f. 0 3 0))
    (.setLocalTranslation (:head parts) (Vector3f. 0 22 0))
    (.setLocalTranslation (:right-upperarm parts) (Vector3f. -7 10 0))
    (.setLocalTranslation (:left-upperarm parts) (Vector3f. 7 10 0))
    (.setLocalTranslation (:right-forearm parts) (Vector3f. -7 1.5 0))
    (.setLocalTranslation (:left-forearm parts) (Vector3f. 7 1.5 0))
    parts))
