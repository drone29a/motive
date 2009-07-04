(ns motive.visual.view
  (:use clojure.contrib.seq-utils)
  (:import (com.jme.bounding BoundingBox
                             BoundingSphere)
           (com.jme.math Quaternion Vector3f)
           (com.jme.scene Node)
           (com.jme.scene.shape Box Sphere AxisRods)
           (com.jme.image Texture$RenderToTextureType)
           (com.jme.util.geom Debugger)
           (com.jme.light PointLight)
           (com.jme.renderer ColorRGBA)
           (com.jmex.game.state DebugGameState GameStateManager)
           (com.jmex.game StandardGame)
           (com.jme.util GameTaskQueueManager)
           (com.jmex.editors.swing.settings GameSettingsPanel)))

(def update-fns (ref []))

(defn clear-movement
  []
  (dosync (ref-set update-fns [])))

(defn add-movement
  "Add movement to the scene."
  [update-fn]
  (dosync
   (alter update-fns #(conj % update-fn))))

(defn remove-movement
  [update-fn]
  (dosync 
   (alter update-fns (fn [us] (vec (remove #(= update-fn %) us))))))

(defn start
  [body]
  (def game (StandardGame. "Motive Scene"))

  (.start game)

  (.update (GameTaskQueueManager/getManager)
           (fn []
             (let [state (DebugGameState.)]
               (.attachChild (.getRootNode state) body)
               (.setModelBound body (BoundingSphere.))
               (.updateModelBound body)
               (.updateRenderState body)
               
               (.attachChild (GameStateManager/getInstance) state)
               (.setActive state true)
               
               (let [update-game-state (proxy [com.jmex.game.state.BasicGameState] ["Joints"]
                                         (update [tpf] 
                                                 (doseq [f @update-fns]
                                                   (when (= (f tpf) :done)
                                                     (remove-movement f)))))]
                 
                 (.attachChild (GameStateManager/getInstance) update-game-state)
                 (.setActive update-game-state true)))
             nil)))

(defn make-move-update
  "The simple-movement's quaternion needs to be adjusted to be relative to
the current rotation of the joint."
  [joint simple-movement]
  (let [start-rot (memoize (fn [] (.getLocalRotation joint)))
        end-rot (memoize (fn [] (.mult (start-rot) (:quaternion simple-movement))))
        update-fn (let [tick (atom 0)]
                    (fn [#^Float tpf]
                      (if (< @tick (:duration simple-movement))
                        (.setLocalRotation joint (doto (Quaternion.)
                                                   (.slerp (start-rot) 
                                                           (end-rot)
                                                           ((:velocity simple-movement) (/ (swap! tick #(+ % tpf)) (:duration simple-movement))))))
                        :done)))]
    update-fn))

;;; Examples
(comment 
  ; Simple movement
  (move (-> body :joints :right-elbow) (simple (quat 1.5 (axis -1 0 0)) (interp [1000 0 10000 0 -500 -800 -1500 -4000]) 6))
  ; Super movement
  (move {(-> body :joints :right-elbow) [[0 (simple (quat 1.8 (axis -1 0 0)) (interp [1000 0 10000 0 -500 -800 -1500 -4000]) 6)]]
         (-> body :joints :right-shoulder) [[3 (simple (quat 1.7 (axis -0.5 1 0)) (interp [50000 -1000 -1000 -50000]) 4)]]})
  )
(defn move
  ([joint simple-movement]
     (add-movement (make-move-update joint
                                     simple-movement)))
  ([joints-movements]
     (let [moves (mapcat (fn [[j ms]]
                           (map (fn [[delay movement]]
                                  {:joint j :delay delay :movement movement})
                                (reduce (fn [acc [delay movement]]
                                          (let [[prev-delay prev-movement] (last acc)]
                                            (conj acc [(+ delay
                                                          prev-delay
                                                          (:duration prev-movement))
                                                       movement]))) 
                                        [(first ms)]
                                        (rest ms))))
                         joints-movements)
           moves-updates (zipmap moves (map (fn [m]
                                              (let [{:keys [joint movement]} m]
                                                (make-move-update joint movement)))
                                            moves))
           super-update-fn (let [super-tick (atom 0)
                                 update-fns (atom (vals moves-updates))
                                 delay-map (into {} (map (fn [[m u]] 
                                                           [u (:delay m)]) 
                                                         moves-updates))]
                             (fn [#^Float tpf]
                               (swap! super-tick #(+ % tpf))
                               (doseq [update-fn @update-fns]
                                 (let [delay (delay-map update-fn)]
                                   (when (>= @super-tick delay)
                                     (let [adj-tpf (if (< (- @super-tick tpf)
                                                          delay)
                                                     (- tpf (- delay 
                                                               @super-tick))
                                                     tpf)]
                                       (when (= (update-fn adj-tpf) :done)
                                         (swap! update-fns (fn [xs]
                                                             (remove #(= % update-fn) xs))))))))
                               (when (empty? @update-fns)
                                 :done)))]
       (add-movement super-update-fn))))

(defn reset
  "Reset body"
  [body]
  (doseq [j (-> body :joints vals)]
    (.setLocalRotation j (Quaternion.)))
  body)