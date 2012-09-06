(ns fuzzbuster.sobel
  (:import
   (java.awt Robot GraphicsEnvironment GraphicsDevice Rectangle Dimension)
   (java.awt.image BufferedImage MemoryImageSource)
   (javax.swing JPanel JFrame)))
 
(def robot (new Robot))
 
(def screen
     (.getDefaultScreenDevice
      (GraphicsEnvironment/getLocalGraphicsEnvironment)))
 
(defn grab-image
  "grab a snapshot of the desktop"
  []
  (let [dm (.getDisplayMode screen)]
    (.createScreenCapture robot
       (new Rectangle
            (.getWidth dm)
            (.getHeight dm)))))
 
(defn panel-to-frame
  "display the supplied JPanel in a JFrame"
  [panel]
  (doto (new JFrame)
    (.add panel)
    (.pack)
    ;(.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
    (.show)))
 
(defn image-to-frame
  "create a JFrame with the given image in it"
  [image]
  (let [panel
        (doto (proxy [JPanel] []
                (paintComponent [g]
                       (.drawImage g image 0 0 nil)))
          (.setPreferredSize
           (new Dimension
                (.getWidth image)
                (.getHeight image))))]
    (panel-to-frame panel)))
 
(defn luminance
  "Approximate luminance of an rgb triple"
  [r g b]
  (bit-shift-right
   (+ (+ (+ (+ (+ (+ (+ r r) r) b) g) g) g) g)
   3))
 
 
(defmacro luminance-inline
  [r g b]
  `(let [r# ~r g# ~g b# ~b]
     (bit-shift-right
      (+ (+ (+ (+ (+ (+ (+ r# r#) r#) b#) g#) g#) g#) g#)
      (int 3))))
 
(defn luminance-to-greyscale
  "convert a lumience level to a greyscale rgb"
  [level]
  (+
   (bit-shift-left level 16)
   (bit-shift-left level 8)
   level))
 
(defmacro luminance-to-greyscale-inline
  [level]
  `(let [level# ~level]
     (+
      (bit-shift-left level# (int 16))
      (bit-shift-left level# (int 8))
      level#)))
 
(defn rgb-to-luminance [rgb]
  (let [r (bit-shift-right (bit-and 0xff0000 rgb) 16)
        g (bit-shift-right (bit-and 0xff00 rgb) 8)
        b (bit-and 0xff rgb)]
    (luminance r g b)))
 
(defmacro rgb-to-luminance-inline [rgb]
  `(let [rgb# ~rgb
         r# (bit-shift-right (bit-and (int 0xff0000) rgb#) 16)
         g# (bit-shift-right (bit-and (int 0xff00) rgb#) 8)
         b# (bit-and (int 0xff) rgb#)]
     (luminance-inline r# g# b#)))
 
(defn rgb-to-greyscale
  "convert an rgb value to a greyscale value"
  [rgb]
  (luminance-to-greyscale (rgb-to-luminance rgb))
)
 
(defn clone-image
  "clone a BufferedImage"
  [image]
  (new BufferedImage
       (.getColorModel image)
       (.copyData image nil)
       (.isAlphaPremultiplied image)
       nil))
 
(defn image-to-greyscale
  "convert an image to greyscale"
  [#^BufferedImage image]
  (let [ #^BufferedImage ret (clone-image image)
        width ( int (.getWidth image))
        height ( int (.getHeight image))]
    (doseq [x (range width) y (range height)]
      (.setRGB ret x y
               (rgb-to-greyscale (.getRGB image x y))))
    ret))
 
(def sobel-convolution-matrix-x (int-array [-1 0 1 -2 0 2 -1 0 1]))
 
(def sobel-convolution-matrix-y (int-array [1 2 1 0 0 0 -1 -2 -1]))
 
(defmacro coords-to-offset [x y]
  `(+ (* (int 3) ~x) ~y))
 
;; evaluates arg more than once
(defmacro unsafe-abs [x]
  `(if (< ~x 0)
     (- ~x)
     ~x))
;; Assumes input image returns 24bit rgb from .getRGB
;; Essentially iterates through all the pixels
;; coverts pixel to greyscale level
;; does sobel convolution on the greyscale level
;; converts the resultant level back to greyscale and puts in in an output image
(defn sobel-edge-detection
  "creates an edge detected image from an input image using Sobel edge detection"
  [#^BufferedImage image]
  (let [width (.getWidth image)
        height (.getHeight image)
        convolution-x (int-array [-1 0 1 -2 0 2 -1 0 1])
        convolution-y (int-array [1 2 1 0 0 0 -1 -2 -1])
        #^BufferedImage ret (clone-image image)]
    (doseq [x (range width) y (range height)]
      (.setRGB
       ret x y
       (luminance-to-greyscale
        (if (or (= x 0) (= y 0) (= x (dec width)) (= y (dec height)))
          255
          (loop [i (int  -1) j (int -1) sumX (int 0) sumY (int 0)]
            (if (= i 2)
              (let [level  (+ (unsafe-abs sumX) (unsafe-abs sumY))
                    trunc (cond (< level 0) 0
                                (> level 255) 255
                                :else level)]
                (- 255 trunc))
              (if (= j 2)
                (recur (inc i) (int -1) sumX sumY)
                (let [luminance (rgb-to-luminance-inline (.getRGB image (+ x i) (+ y j)))
                      offset (coords-to-offset (inc i) (inc j))]
                  (recur
                   i
                   (inc j)
                   (+ sumX (* luminance (aget convolution-x offset)))
                   (+ sumY (* luminance (aget convolution-y offset))))))))))))
    ret))
 
(defn sobel-test [] (image-to-frame ( sobel-edge-detection (grab-image))))
 
