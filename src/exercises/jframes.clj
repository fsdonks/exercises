(ns exercises.jframes
  ;;T: We can use some of this stuff that I've already wrapped
  ;;in Spork.
  (:require [spork.graphics2d [image :as image]
                              [canvas :as canvas]]
            [spork.cljgui.components [swing :as swing]]
            ;;some plots useful for our stuff.
            [incanter [core :as i]
                      [charts :as c]
                      [stats :as s]])                           
  ;;T: Always put imports in the NS declaration if you can...
  ;;I moved yours here
  (:import [java.awt Frame Rectangle Robot Container Font Graphics2D]
           [java.awt.image BufferedImage]
           [java.io  File]
           [javax.imageio ImageIO]
           [javax.swing JFrame JLabel JPanel]
           ;;T: Some stuff from proc.core, native classes for charts.
           [org.jfree.chart ChartPanel]
           ))

;;aux functions
;;============

(defn ^ChartPanel chart!
  "Pulled this from proc.core .  Creates a JPanel wrapper around 
   a JFreeChart chart, which is what Incanter returns.  This allows 
   us to use combinators from spork.cljgui.components.swing to 
   embed charts in complicated ui layouts easily."
  [ch]  
  (ChartPanel. ch true true true true true))

;;Let's create a test function that renders something "like"
;;dwell-over-fill, i.e. two charts stacked on top of eachother...
(defn hm
  "Makes a sample heatmap, as per (doc heat-map) example.."
  []
  (let [data [[0 5 1 2]
              [0 10 1.9 1]
              [15 0 0.5 1.5]
              [18 10 4.5 2.1]]
          diffusion (fn [x y]
                      (i/sum (map #(s/pdf-normal (s/euclidean-distance [x y] (take 2 %))
                                                 :mean (nth % 2) :sd (last %))
                                  data)))]
    (c/heat-map diffusion -5 20 -5 20)))

(defn sample-chart
  "Creates our analogue for the dwell-over-fill chart."
  []
  (swing/stack (chart! (hm))
               (chart! (hm))))

;;If you plug this in at the REPL (minus the #_ comment reader macro,
;;you should get a JFrame with two identical,stacked charts showing...
#_(swing/->scrollable-view (sample-chart)) 

;;The problem is...we don't want to have an interactive chart pop up, we don't
;;want ANYTHING to pop up.  Remember, this should be programmatic.
;;Can we render this offscreen?
;;Sure....
;;  But it takes some doing.  You have to force layout to an invisible jframe
;;    and other fun stuff....
;;    Unless you've already got access to a library that wraps the crap
;;    for you (hint).
(defn save-the-chart [c]
  (image/shape->png c ".\\blah.png" :on-save println))

;; exercises.jframes> (save-the-chart (sample-chart))
;; Buffer saved to:.\blah.png
;; nil


;;With something akin to save-the-chart, you ought to be
;;able to quickly dump a bunch of PNGs for anything that's
;;shape-like (shapes according to spork.graphics2d.canvas,
;;which are extended to include JComponents (which should
;;cover JPanels and other subclassed stuff....)))


;; Returns dimensions of frame as Java Rectangle object
;; Used as arument for Java Robot 
(defn frame-rec [frame]
  (Rectangle.
   (.getX frame)
   (.getY frame)
   (.getWidth frame)
   (.getHeight frame)))


;; ==== OLD =======================================================
;; Function used for previous method of saving file

;;T: use doto for more idiomatic java object-smashing.
;; Focuses the frame on the screen, returns true when done
(defn focus-frame [frame]
  (doto frame
    (.setAlwaysOnTop  true) ;; put on top of all other windows
    (.setState  Frame/NORMAL) ;; un-minimizes/un-maximized
    (.toFront ) ;; brings to front
    (.requestFocus) 
    (.setVisible true) ;; makes visible on screen
    ))

;; Adds/Removes border from frame, returns true when done 
(defn frame-border [frame bool]
  (doto frame
    (.setVisible  false) ;; Removes from screen 
    (.removeNotify) ;; Makes frame undisplayaple
    (.setUndecorated (not bool)) ;;removes broder
    (.addNotify) ;;Makes frame displayable again
    (.setVisible true) ;; Sets frame visible on screen
  ))

;; Releases focus of the screen, returns true when done
(defn release-frame [frame]
  (doto frame
    (.setAlwaysOnTop false)  
    (.toBack) ;; pushed frame to back
    ))

;; Creates an image from screen capture and writes to file
(defn write-frame [frame filename]
  (let [buff (.createScreenCapture (Robot.) (frame-rec frame))
        file (File. filename)] ;; Buffered Image and File pointer 
    (javax.imageio.ImageIO/write buff "png" file))
  frame) ;; Writes to file 
;; ================================================================

;; Saves frame to file
(defn save-frame [frame filename] 
  (.setVisible frame false)
  (let [container  (.getContentPane frame)
        buff (BufferedImage.
              (.getWidth container)
              (.getHeight container)
              (BufferedImage/TYPE_INT_RGB))
        graf (.createGraphics buff)
        file (File. filename)]
    (.printAll container graf)
    (.dispose graf)
    (javax.imageio.ImageIO/write buff "png" file))
  frame) ;; returns frame 

;; Creates a JLabel with text and font
(defn make-label [text & font]
  (let [lab (JLabel. text)]
    (if font
      (.setFont lab font)
      (.setFont lab (Font. "Times New Roman" 1 24)))
    lab)) ;; returns the JLabel object

;; Still have to change font to bold for charts 
;; Creates a title label for dwell and fill charts 
(defn make-chart-label [run interest]
  ;; can change font/style later
  (let [font (Font. "Times New Roman" (Font/BOLD) 48)]
    (make-label (str "Run " run " - " interest))))

(defn make-font [name style size]
  ;; Style: Font/BOLD, Font/Italic, ect. (can add multiple styles together)
  (Font. name style size))

;;adds new component to frame 
(defn add-component [frame comp]
  (.add (.getContentPane frame) comp)
  frame) ;; returns updated frame

;; Sets the title of frame
(defn set-title [frame title]
  (.setTitle frame title)
  frame) ;; returns updated frame

;; Sets the taskbar-title with information about run and interest
(defn set-taskbar-title [frame run interest] 
  (set-title frame (str "Run" run "-" interest))
  frame) ;; returns updated frame

