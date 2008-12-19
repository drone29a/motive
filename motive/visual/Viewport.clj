(ns motive.visual.Viewport
  (:import (com.jme.app BaseSimpleGame AbstractGame)
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
   :exposes-methods {update superUpdate
                     render superRender}
   :methods [[addWithUpdate [com.jme.scene.Spatial clojure.lang.IFn] void]
             [add [com.jme.scene.Spatial] void]
             [remove [com.jme.scene.Spatial] void]
             [removeAll [] void]]
   :state state))

;; The objs map contains renderable objects mapped to their update function
(defn -ctor []
  [[] (atom {:objs {}})])

(defn -simpleInitGame
  [this]
  )

(defn -quit
  [this]
  (let [display (.getDisplay this)]
    (when display
      (.close display))))

(defn -add
  [this obj]
  (swap! (.state this) 
         (fn [s]
           (let [objs (:objs s)]
             (assoc s :objs (assoc objs obj nil)))))
  (.attachChild (.getRootNode this) obj))

(defn -addWithUpdate
  [this obj update-fn]
  (swap! (.state this) 
         (fn [s]
           (let [objs (:objs s)]
             (assoc s :objs (assoc objs obj update-fn)))))
  (.attachChild (.getRootNode this) obj))

(defn -remove
  [this obj]
  (.detachChild (.getRootNode this) obj))

(defn -removeAll
  [this]
  (.detachAllChildren (.getRootNode this)))

(defn -update 
  [this #^Float/TYPE interp]
  (.superUpdate this interp)
  
  (doseq [[obj f] (:objs @(.state this))]
    (when f 
      (f this obj)
      (.updateModelBound obj)))

  (when (not (.getPause this))
    (.simpleUpdate this)
    (.updateGeometricState (.getRootNode this) (.getTpf this) true)))

(defn -render
  [this #^Float/TYPE interp]
  (.superRender this interp)

  (let [r (.getRenderer (.getDisplay this))]
    (.draw r (.getRootNode this))
    (.simpleRender this)))

(defn -main []
  (let [v (motive.visual.Viewport.)]
    (.start v)))