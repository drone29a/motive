(ns motive.visual.model
  (:use motive
        motive.visual)
  (:import (com.jme.bounding BoundingBox)
           (com.jme.scene.shape Cylinder AxisRods)
           (com.jme.scene Node Spatial)
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
        joints {:left-elbow (AxisRods. "left-elbow")
                :right-elbow (AxisRods. "right-elbow")
                :left-shoulder (AxisRods. "left-shoulder")
                :right-shoulder (AxisRods. "right-shoulder")}
        hips-node (AxisRods. "hips-node")
        torso-node (AxisRods. "torso-node")
        left-upperarm-node (AxisRods. "left-upperarm-node")
        right-upperarm-node (AxisRods. "right-upperarm-node")]
    
    ;; Parts and space-holding nodes into position
    (.setLocalTranslation (:torso parts) (Vector3f. 0 0 0))
    (.setLocalTranslation (:hips parts) (Vector3f. 0 0 0))
    (.setLocalTranslation (:head parts) (Vector3f. 0 10 0))
    (.setLocalTranslation (:right-upperarm parts) (Vector3f. 0 0 0))
    (.setLocalTranslation (:left-upperarm parts) (Vector3f. 0 0 0))
    (.setLocalTranslation (:right-forearm parts) (Vector3f. 0 -5 0))
    (.setLocalTranslation (:left-forearm parts) (Vector3f. 0 -5 0))
    (.setLocalTranslation hips-node (Vector3f. 0 3 0))
    (.setLocalTranslation torso-node (Vector3f. 0 9 0))
    (.setLocalTranslation left-upperarm-node (Vector3f. 0 -5 0))
    (.setLocalTranslation right-upperarm-node (Vector3f. 0 -5 0))

    ;; Joints in right spot
    (.setLocalTranslation (:left-elbow joints) (Vector3f. 0 -5.5 0))
    (.setLocalTranslation (:right-elbow joints) (Vector3f. 0 -5.5 0))
    (.setLocalTranslation (:left-shoulder joints) (Vector3f. 7 5 0))
    (.setLocalTranslation (:right-shoulder joints) (Vector3f. -7 5 0))

    ;; It all starts at the hips
    (.attachChild hips-node (cast Spatial (:hips parts)))
    (.attachChild hips-node torso-node)
    (.attachChild torso-node (cast Spatial (:torso parts)))
    (.attachChild torso-node (cast Spatial (:head parts)))
    (.attachChild torso-node (:left-shoulder joints))
    (.attachChild (:left-shoulder joints) left-upperarm-node)
    (.attachChild left-upperarm-node (cast Spatial (:left-upperarm parts)))
    (.attachChild left-upperarm-node (:left-elbow joints))
    (.attachChild (:left-elbow joints) (cast Spatial (:left-forearm parts)))
    (.attachChild torso-node (:right-shoulder joints))
    (.attachChild (:right-shoulder joints) right-upperarm-node)
    (.attachChild right-upperarm-node (cast Spatial (:right-upperarm parts)))
    (.attachChild right-upperarm-node (:right-elbow joints))
    (.attachChild (:right-elbow joints) (cast Spatial (:right-forearm parts)))

    hips-node))
