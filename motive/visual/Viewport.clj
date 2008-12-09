(ns motive.visual.Viewport
  (:import (com.jme.app BaseSimpleGame)
           (com.jme.bounding BoundingBox
                             BoundingSphere)
           (com.jme.math Quaternion Vector3f)
           (com.jme.scene.shape Box Sphere))
  (:gen-class
   :extends com.jme.app.BaseSimpleGame
   :init ctor
   :constructors {[] []}
   :exposes {rootNode {:get getRootNode}
             display {:get getDisplay}
             tpf {:get getTpf}
             pause {:get getPause}}
   :super-methods {super-update [update [float] void]
                   super-render [render [float] void]}
   :methods [[add [com.jme.scene.Geometry clojure.lang.IFn] void]]
   :state state))

;; The objs map contains renderable objects mapped to their update function
(defn -ctor []
  [[] (atom {:objs {}})])

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

(defn -add 
  [this obj update-fn]
  (swap! (.state this) 
         (fn [s]
           (let [objs (:objs s)]
             (assoc objs obj update-fn))))
  (.attachChild (.getRootNode this) obj))

(defn -update 
  [this #^Float/TYPE interp]
  (. this super-update interp)
  
  (when (not (.getPause this))
    (.simpleUpdate this)
    (.updateGeometricState (.getRootNode this) (.getTpf this) true)))

(defn -render
  [this #^Float/TYPE interp]
  (. this super-render interp)

  (let [r (.getRenderer (.getDisplay this))]
    (.draw r (.getRootNode this))
    (.simpleRender this)))

(defn -main []
  (let [v (motive.visual.Viewport.)]
    (.start v)))