(in-ns 'motive.visual)

(gen-and-load-class 'motive.visual.TestSimpleGame
                    :extends com.jme.app.SimpleGame
                    :exposes '{rootNode {:get getRootNode}})

(defn TestSimpleGame-simpleInitGame
  [this]
  (let [box (Box. "my box" (new Vector3f 0 0 0) 2 2 2)]
    (. box (setModelBound (new BoundingSphere)))
    (. box (updateModelBound))
    (. (.getRootNode this) (attachChild box))))

(defn start 
  []
  (let [game (new motive.visual.TestSimpleGame)]
    (. game start)))

;; (. game (setDialogBehaviour (.. SimpleGame ALWAYS_SHOW_PROPS_DIALOG)))