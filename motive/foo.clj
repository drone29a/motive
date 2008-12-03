(ns motive.foo
  (:gen-class
   :init ctor
   :constructors {[String] []}
   :extends java.lang.Object
   :state state
   :methods [[getIt [] String]]))

(defn -ctor [s]
  [[] (ref {:s s})])

(defn -getIt [this]
  (:s @(.state this)))
