(ns motive
  (:use motive.movement
        motive.visual.view)
  (:require motive.visual.model))

(def body (motive.visual.model/build-body))

(-> (Thread. (fn [] (motive.visual.view/start (:root body)))) .start)
