;q;A quick namespace for exploring ideas about using
;;the POI libs for munging PPTX presentations programatically.

(ns exercises.powerpoint 
   (:import [org.apache.poi.xslf.usermodel 
             XMLSlideShow XSLFSlide XSLFPictureData] ;any more?
            [java.io ;maybe unnecessary, used in demo.
             FileInputStream FileOutputStream File]
            [java.nio.file Files Paths]         
            )) 

;;Port examples here...
(comment
(defn example-pptx []
  (let [out (FileOutputStream. (File. "test.pptx"))
        ppt (XMLSlideShow.)]
    (doto ppt
      (.createSlide)
      (.write out))))
)

;; Saves ppt obj to file
(defn save-ppt [ppt filename]
  (let [out (FileOutputStream. (File. filename))]
    (.write ppt out)))

;; reads picture data and returns java byte array
(defn picture->data [filename] ;; String (filename) -> java byte[]
  (let [path (.toPath (File. filename))]
    (Files/readAllBytes path)))

;; given a slide, adds picture file to ppt 
(defn add-picture [slide filename]
  (let [data (.addPicture ppt
                          (picture->data filename) XSLFPictureData/PICTURE_TYPE_PNG)]
    (.createPicture slide data)))
    ;;Returns a java XSLFPictureShape obj, not used for anything

;; Given a ppt, creates new slide and adds picture from file for each file
(defn add-pictures [ppt filenames]
  (doseq [file filenames]
    (add-picture (.createSlide ppt) file)) ppt) ;; returns ppt

