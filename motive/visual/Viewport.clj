(ns motive.visual.Viewport
  (:import (com.jme.app SimpleGame BaseSimpleGame AbstractGame)
           (com.jme.bounding BoundingBox
                             BoundingSphere)
           (com.jme.math Quaternion Vector3f)
           (com.jme.scene Node)
           (com.jme.scene.shape Box Sphere AxisRods)
           (com.jme.image Texture$RenderToTextureType)
           (com.jme.util.geom Debugger)
           (com.jme.light PointLight)
           (com.jme.renderer ColorRGBA))
  (:gen-class
   :extends com.jme.app.SimpleGame
   :init ctor
   :constructors {[com.jme.scene.Spatial] []}
   :exposes {rootNode {:get getRootNode}
             display {:get getDisplay}
             tpf {:get getTpf}
             pause {:get getPause}
             lightState {:get getLightState :set setLightState}
             statNode {:get getStatNode}
             showDepth {:get getShowDepth}}
   :exposes-methods {update superUpdate
                     render superRender
                     doDebug superDoDebug}
   :methods [[addWithUpdate [com.jme.scene.Spatial clojure.lang.IFn] void]
             [add [com.jme.scene.Spatial] void]
             [remove [com.jme.scene.Spatial] void]
             [removeAll [] void]]
   :state state))

;; The objs map contains renderable objects mapped to their update function
(defn -ctor 
  [#^Spatial body]
  [[] (atom {:body body :objs {}})])

(defn -simpleInitGame
  [this]
  (.setTitle (.getDisplay this) "Motive Viewport")
  
  (let [body (:body @(.state this))
        head (.getChild body "head")]
;;     (.setModelBound head (BoundingBox.))
;;     (.updateModelBound head)
;;     (.attachChild (.getRootNode this) head)
    (.setModelBound body (BoundingBox.))
    (.updateModelBound body)
    (.updateRenderState body)
    (.attachChild (.getRootNode this) body)
    (.setRenderState head (.getLightState this))
    (.updateRenderState head)
    (.updateRenderState (.getRootNode this))
    (.updateRenderState head)
)

;;   (let [b (Box. "box" (Vector3f. 1 1 1) 3 3 3)]
;;     (.setModelBound b (BoundingSphere.))
;;     (.updateModelBound b)
;;     (.attachChild (.getRootNode this) b))

;;   (.detachAll (.getLightState this))
;;   (.attach (.getLightState this) (doto (PointLight.)
;;                                    (.setDiffuse (ColorRGBA. 1.0 1.0 1.0 1.0))
;;                                    (.setAmbient (ColorRGBA. 0.9 0.9 0.9 1.0))
;;                                    (.setSpecular (ColorRGBA. 1.0 0.0 0.0 1.0))
;;                                    (.setLocation (Vector3f. 0 10 10))
;;                                    (.setEnabled true)))
  
;  (.setRenderState (.getRootNode this) (doto (-> (.getDisplay this) .getRenderer .createTextureState)
;                                         (.setEnabled true)))
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

;; (defn -simpleUpdate 
;;   [this]
  
;;   (doseq [[obj f] (:objs @(.state this))]
;;     (when f 
;;       (f this obj)
;;       (.updateModelBound obj))))

;; (defn -update 
;;   [this #^Float/TYPE interp]
;;   (.superUpdate this interp)
;;   (doto System/out (println "Updating."))
  
;;   (doseq [[obj f] (:objs @(.state this))]
;;     (when f 
;;       (f this obj)
;;       (.updateModelBound obj)))

;;   (when (not (.getPause this))
;;     (.simpleUpdate this)
;;     (.updateGeometricState (.getRootNode this) (.getTpf this) true)
;;     (.updateRenderState (.getRootNode this))
;;     (.updateGeometricState (.getStatNode this) (.getTpf this) true)
;;     (.updateRenderState (.getStatNode this))))

(defn -render
  [this #^Float/TYPE interp]
  (.superRender this interp)

  (let [r (.getRenderer (.getDisplay this))]
    (.draw r (.getRootNode this))
    (.simpleRender this)
    (.draw r (.getStatNode this))
    (.doDebug this r)))

(defn -doDebug
  [this #^com.jme.renderer.Renderer r]
  (.superDoDebug this r)

  (when (.getShowDepth this)
    (.renderQueue r)
    (.drawBuffer Debugger Texture$RenderToTextureType/Depth Debugger/NORTHEAST r)))