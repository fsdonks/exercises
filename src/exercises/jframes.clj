(ns exercises.jframes)

(import java.awt.Frame)
(import java.awt.Rectangle)
(import java.awt.Robot)
(import java.io.File)
(import javax.imageio.ImageIO)
(import javax.swing.JFrame)
(import javax.swing.JLabel)
(import java.awt.Container)
(import javax.swing.JPanel)
(import java.awt.Font)
 
(comment 
(defn example-frame []
  (import 'javax.swing.JFrame)
  (def frame (JFrame. "Test Frame"))
  (.setSize frame 300 300)
  (.setVisible frame true))
)

;; Returns dimensions of frame as Java Rectangle object
;; Used as arument for Java Robot 
(defn frame-rec [frame]
  (Rectangle.
   (.getX frame)
   (.getY frame)
   (.getWidth frame)
   (.getHeight frame)))

;; Focuses the frame on the screen, returns true when done
(defn focus-frame [frame]
  (do
    (.setAlwaysOnTop frame true) ;; put on top of all other windows
    (.setState frame Frame/NORMAL) ;; un-minimizes/un-maximized
    (.toFront frame) ;; brings to front
    (.requestFocus frame) 
    (.setVisible frame true) ;; makes visible on screen
    true))

;; Adds/Removes border from frame, returns true when done 
(defn frame-border [frame bool] 
  (.setVisible frame false) ;; Removes from screen 
  (.removeNotify frame) ;; Makes frame undisplayaple
  (.setUndecorated frame (not bool)) ;;removes broder
  (.addNotify frame) ;;Makes frame displayable again
  (.setVisible frame true) ;; Sets frame visible on screen
  true)

;; Releases focus of the screen, returns true when done
(defn release-frame [frame]
  (.setAlwaysOnTop frame false)  
  (.toBack frame) ;; pushed frame to back
  true)

;; Creates an image from screen capture and writes to file
(defn write-frame [frame filename]
  (let [buff (.createScreenCapture (Robot.) (frame-rec frame))
        file (File. filename)] ;; Buffered Image and File pointer 
    (javax.imageio.ImageIO/write buff "jpg" file))) ;; Writes to file 

;; Focuses the frame, writes the frame, then releases focus
(defn save-frame [frame filename]
  (while (not (focus-frame frame))) ;; waits for value to be returned
  (while (not (frame-border frame false))) ;; waits for value to be returned
  (write-frame frame filename)
  (while (not (frame-border frame true))) ;; waits for value to be returned 
  (while (not (release-frame frame)))) ;; waits for value to be returned 

(defn make-label [text & font]
  (let [lab (JLabel. text)
        font (.getFont lab)]
    (if font
      (.setFont lab font)
      (.setFont lab (Font. "Times New Roman" 1 12)))
    lab))

(defn make-font [name style size]
  ;; Style: Font/BOLD, Font/Italic, ect. (can add multiple styles together)
  (Font. name style size))

(defn add-component [frame panel comp]
  (.add panel comp)
  (.add frame panel)
  (.pack frame))
  
  


