(ns fuzzbuster.core
  (:require [clojure.java.io :as j]
            clojure.main)
  (:use fuzzbuster.sobel)
  (:import (java.io File) (javax.imageio ImageIO) (java.awt.image BufferedImage))
  (:gen-class))

(set! *warn-on-reflection* true)


(defn sobel-file [f]
  (sobel-edge-detection (ImageIO/read (j/as-file f))))

(defn light-or-dark [^BufferedImage img x y]
  (if (< 127 (rgb-to-luminance (.getRGB img x y)))
    :light
    :dark))

(defn edge-count [^BufferedImage img place dir]
  (loop [results {:count 0 :width 0}
         pos 0
         last nil]
    (if (> (if (= dir :horiz)
             (.getWidth img)
             (.getHeight img))
           pos)
      (if-not (= last (if (= dir :horiz)
                        (light-or-dark img pos place)
                        (light-or-dark img place pos))) ; this is an edge
        (recur (-> results
                   (update-in [:count] + 1)
                   (update-in [:width] + (if (= :light last)
                                           1
                                           0)))
               (inc pos)
               (if (= :light last)
                 :dark
                 :light))
        (recur (if (= :dark last)
                 (update-in results [:width] + 1)
                 results)
               (inc pos)
               last))
      results)))

(defn edge-count-horiz [^BufferedImage img]
  (apply merge-with +
         (for [y (range (.getHeight img))]
           (edge-count img y :horiz))))

(defn edge-count-vert [^BufferedImage img]
  (apply merge-with +
         (for [x (range (.getWidth img))]
           (edge-count img x :vert))))

(defn edge-width-horiz [img]
  (let [{:keys [width count]} (edge-count-horiz img)]
    (/ width count)))

(defn edge-width-vert [img]
  (let [{:keys [width count]} (edge-count-vert img)]
    (/ width count)))

(defn edge-width-img [img]
  (/ (+ (edge-width-horiz img)
        (edge-width-vert img))
     2))

(defn edge-width-file [f]
  (edge-width-img (ImageIO/read (j/as-file f))))



(defn -main
  "I don't do a whole lot."
  [& args]
  (println "Hello, World!"))
