(ns motive.visual
    (:use motive)
    (:import (java.io File)
             (java.net URISyntaxException URL)
             (java.util.logging Logger)
             (com.jme.app SimpleGame)
             (com.jme.bounding BoundingBox
                               BoundingSphere)
             (com.jme.image.Texture)
             (com.jme.math Quaternion Vector3f)
             (com.jme.scene.shape Box Sphere)
             (com.jme.scene.state TextureState)
             (com.jme.util TextureManager)
             (com.jme.util.resource MultiFormatResourceLocator
                                    ResourceLocatorTool
                                    SimpleResourceLocator))
    (:load "window.clj"))