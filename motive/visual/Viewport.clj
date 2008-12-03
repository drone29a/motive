(ns motive.visual.Viewport
  (:import (com.jme.app SimpleGame)
           (com.jme.bounding BoundingBox
                             BoundingSphere)
           (com.jme.math Quaternion Vector3f)
           (com.jme.scene.shape Box Sphere))
  (:gen-class 
   :extends com.jme.app.SimpleGame
   :init ctor
   :constructors {[] []}
   :exposes {rootNode {:get getRootNode}
             display {:get getDisplay}}))

(defn -ctor []
  [[]])

(defn -simpleInitGame
  [this]
  (let [box (Box. "my box" (new Vector3f 0 0 0) 2 2 2)]
    (. box (setModelBound (new BoundingSphere)))
    (. box (updateModelBound))
    (. (.getRootNode this) (attachChild box))))

(defn -quit
  [this]
  (let [display (.getDisplay this)]
    (when display
      (.close display))))