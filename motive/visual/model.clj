(ns motive.visual.model
  (:use motive.visual)
  (:import (com.jme.bounding BoundingBox BoundingSphere)
           (com.jme.scene.shape Cylinder AxisRods)
           (com.jme.scene Node Spatial Spatial$LightCombineMode)
           (com.jme.math Vector3f Quaternion)))

(defstruct body
  :root
  :joints)

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
                                            (.setModelBound c (BoundingSphere.))
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
        joints {:left-elbow (doto (AxisRods. "left-elbow") (.setLightCombineMode Spatial$LightCombineMode/Inherit))
                :right-elbow (doto (AxisRods. "right-elbow") (.setLightCombineMode Spatial$LightCombineMode/Inherit))
                :left-shoulder (doto (AxisRods. "left-shoulder") (.setLightCombineMode Spatial$LightCombineMode/Inherit))
                :right-shoulder (doto (AxisRods. "right-shoulder") (.setLightCombineMode Spatial$LightCombineMode/Inherit))}
        hips-node (doto (AxisRods. "hips-node") (.setLightCombineMode Spatial$LightCombineMode/Inherit))
        torso-node (doto (AxisRods. "torso-node") (.setLightCombineMode Spatial$LightCombineMode/Inherit))
        left-upperarm-node (doto (AxisRods. "left-upperarm-node") (.setLightCombineMode Spatial$LightCombineMode/Inherit))
        right-upperarm-node (doto (AxisRods. "right-upperarm-node") (.setLightCombineMode Spatial$LightCombineMode/Inherit))]
    
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
    (.attachChild hips-node (:hips parts))
    (.attachChild hips-node torso-node)
    (.attachChild torso-node (:torso parts))
    (.attachChild torso-node (:head parts))
    (.attachChild torso-node (:left-shoulder joints))
    (.attachChild (:left-shoulder joints) left-upperarm-node)
    (.attachChild left-upperarm-node (:left-upperarm parts))
    (.attachChild left-upperarm-node (:left-elbow joints))
    (.attachChild (:left-elbow joints) (:left-forearm parts))
    (.attachChild torso-node (:right-shoulder joints))
    (.attachChild (:right-shoulder joints) right-upperarm-node)
    (.attachChild right-upperarm-node (:right-upperarm parts))
    (.attachChild right-upperarm-node (:right-elbow joints))
    (.attachChild (:right-elbow joints) (:right-forearm parts))
    
    (struct-map body
      :root hips-node
      :joints joints)))

(defn pose-arms
  "Modify the model's arms to match the pose defined by vectors."
  [model [right-upper right-fore] [left-upper left-fore]]
  (let [left-upper (.normalize left-upper)
        left-fore (.normalize left-fore)
        left-shoulder (.getChild model "left-shoulder")
        left-upper-angle (.angleBetween left-upper (Vector3f. 0 -1 0))
        left-upper-axis (.cross left-upper (Vector3f. 0 -1 0))
        left-upper-quat (.mult (Quaternion. (doto (make-array Float/TYPE 3) 
                                              (aset 0 (float 0))
                                              (aset 1 (float 0))
                                              (aset 2 (float 0)))) 
                               (.fromAngleAxis (Quaternion.) left-upper-angle left-upper-axis))
        left-elbow (.getChild model "left-elbow")
        left-fore-angle (.angleBetween left-fore (Vector3f. 0 -1 0))
        left-fore-axis (.cross left-fore (Vector3f. 0 -1 0))
        left-fore-quat (.mult (Quaternion. (doto (make-array Float/TYPE 3) 
                                             (aset 0 (float 0))
                                             (aset 1 (float 0))
                                             (aset 2 (float 0)))) 
                              (.fromAngleAxis (Quaternion.) left-fore-angle left-fore-axis))]
    (.setLocalRotation left-shoulder left-upper-quat)
    (.setLocalRotation left-elbow left-fore-quat)))

(defn make-update-fn 
  [data]
  (let [shoulder-eulers (take)]))

(comment 
  (defn prep-for-anim
    "Take the model and some FoB data.  Set the initial pose and build the frame updater generator thing.
Return both the model and the updater function."
    [body data]
    (pose-arms body [nil nil] (initial-poses (take 3 data)))))

(defn make-update-shoulders-fn
  [rot-gen]
  (fn [viewport body]
    (let [tpf (.getTpf viewport)
          left-shoulder (.getChild body "left-shoulder")
          left-shoulder-rot (-> (.getLocalRotation left-shoulder) (.mult (rot-gen tpf)))]
      (.setLocalRotation left-shoulder left-shoulder-rot))))