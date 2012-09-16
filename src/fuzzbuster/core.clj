(ns fuzzbuster.core
  (:require [clojure.java.io :as j]
            [fs.core :as f]
            [useful.parallel :as p]
            [useful.map :as m]
            clojure.main)
  (:use fuzzbuster.sobel
        [clojure.tools.cli :only [cli]])
  (:import (java.io File) (javax.imageio ImageIO) (java.awt.image BufferedImage))
  (:gen-class))

(set! *warn-on-reflection* true)


(defn sobel-file [f]
  (sobel-edge-detection (ImageIO/read (j/as-file f))))

(defn light-or-dark [^BufferedImage img x y]
  (try 
   (if (< 127 (rgb-to-luminance (.getRGB img x y)))
     :light
     :dark)
   (catch Exception _
     (throw (Exception. (str x " " y))))))

(defn edge-count 
  ([^BufferedImage img place dir]
     (edge-count img place dir 0 (.getWidth img) 0 (.getHeight img)))
  ([^BufferedImage img place dir start end]
     (loop [results {:count 0 :width 0}
            pos start
            last nil]
       (if (and (>= (if (= dir :horiz)
                      (min (.getWidth img) end)
                      (min (.getHeight img) end))
                    (inc pos)) ; the last px is one less than width/height
                (>= (if (= dir :horiz)
                     (.getHeight img)
                     (.getWidth img))
                   (inc place)))
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
         results))))

(defn edge-count-horiz 
  ([^BufferedImage img]
     (apply merge-with +
            (for [y (range (.getHeight img))]
              (edge-count img y :horiz))))
  ([^BufferedImage img start-x end-x start-y end-y]
     (apply merge-with +
            (for [y (range start-y end-y)]
              (edge-count img y :horiz start-x end-x)))))

(defn edge-count-vert
  ([^BufferedImage img]
     (apply merge-with +
            (for [y (range (.getWidth img))]
              (edge-count img y :vert))))
  ([^BufferedImage img start-x end-x start-y end-y]
     (apply merge-with +
            (for [x (range start-x end-x)]
              (edge-count img x :horiz start-y end-y)))))

(defn edge-width-horiz 
  ([^BufferedImage img]
     (edge-width-horiz img 0 (.getWidth img) 0 (.getHeight img)))
  ([img start-x end-x start-y end-y]
     (let [{:keys [width count]} (edge-count-horiz img start-x end-x start-y end-y)]
       (/ width (max count 1)))))

(defn edge-width-vert
  ([^BufferedImage img]
     (edge-width-vert img 0 (.getWidth img) 0 (.getHeight img)))
  ([img start-x end-x start-y end-y]
     (let [{:keys [width count]} (edge-count-vert img start-x end-x start-y end-y)]
       (/ width (max count 1)))))

(defn mean [& numbers]
  (/ (apply + numbers)
     (count numbers)))

(defn edge-width-img 
  ([img]
     (edge-width-img img 1))
  ([^BufferedImage img sectors]
     (let [sector-count sectors
           sector-width (Math/ceil (/ (.getWidth img) sector-count))
           sector-height (Math/ceil (/ (.getHeight img) sector-count))
           ]
       (for [x-mult (range sector-count)
             y-mult (range sector-count)]
         (mean (edge-width-horiz img
                                 (* sector-width x-mult) (* sector-width (inc x-mult))
                                 (* sector-height y-mult) (* sector-height (inc y-mult)))
               (edge-width-vert img
                                (* sector-width x-mult) (* sector-width (inc x-mult))
                                (* sector-height y-mult) (* sector-height (inc y-mult))))))))

(defn edge-width-file 
  ([f]
     (edge-width-file f 1))
  ([f sectors]
     (edge-width-img (ImageIO/read (j/as-file f)) sectors)))

(defn process-file [f sectors]
  (try [f (edge-width-file f sectors)]
       (catch Exception _ nil))) ; I hear it's bad style to filter out unprocessable files using exceptions. I don't care.

(defn process-dir [d sectors]
  (into {}
        (filter identity
                   (p/pcollect #(process-file (str d "/" %) sectors)
                               (f/list-dir d)))))

(defn sharp? [result threshold sectors]
  (>= (count (filter #(and (< % threshold)
                                  (pos? %)) result)) ; don't count sectors with no edges
      (/ (* sectors sectors) 4)))

(defn list-blurry-images [dir threshold sectors]
  (m/filter-keys-by-val #(sharp? % threshold sectors)
                        (process-dir dir)))

(defn -main
  "I don't do a whole lot."
  [& args]
  (let [[options args banner]
        (cli args
             ["-h" "--help" "Show help" :default false :flag true]
             ["-t" "--threshold"
              "Threshold to consider blurry (default 50)"
              :parse-fn #(Integer. %)
              :default 50]
             ["-s" "--sectors"
              "Square root of the number of sectors to split image in to (default 5)"
              :parse-fn #(Integer. %)
              :default 5])]
    (when (:help options)
      (println banner)
      (System/exit 0))
    (doall (map println (-> (list-blurry-images (if (f/directory? (first args))
                                                  (first args)
                                                  (str f/*cwd* "/" (first args)))
                                                (:threshold options)
                                                (:sectors options))
                            sort)))))
