;;A quick namespace for exploring ideas about using
;;the POI libs for munging PPTX presentations programatically.

(ns exercises.powerpoint 
   (:import [org.apache.poi.xslf.usermodel 
             XMLSlideShow XSLFSlide XSLFPictureData XSLFTheme
             XSLFSlideLayout XSLFSlideMaster SlideLayout] ;any more?
            [java.io ;maybe unnecessary, used in demo.
             FileInputStream FileOutputStream File]
            [java.nio.file Files Paths]         
            )) 

(defn ^XMLSlideShow new-ppt []
  (^XMLSlideShow XMLSlideShow.))

(defn ^XSLFSlide new-slide [ppt]
  (^XSLFSlide .createSlide ppt))

;; Saves ppt obj to file
(defn ^XMLSlideShow save-ppt [ppt filename]
  (let [out (^FileOutputStream FileOutputStream. (^File File. filename))]
    (.write ppt out)) ppt)

;; reads picture data and returns java byte array
(defn picture->data [filename] ;; String (filename) -> java byte[]
  (let [path (^Path .toPath (^File File. filename))]
    (Files/readAllBytes path)))

;; Returns java XSLFPictureData.PictureType (int)  from string type-name
(defn get-picturedata-type [type-name] ;; String -> Int 
  (let [m {"BMP" XSLFPictureData/PICTURE_TYPE_BMP 
           "DIB" XSLFPictureData/PICTURE_TYPE_DIB 
           "EMF" XSLFPictureData/PICTURE_TYPE_EMF 
           "EPS" XSLFPictureData/PICTURE_TYPE_EPS 
           "GIF" XSLFPictureData/PICTURE_TYPE_GIF 
           "JPEG" XSLFPictureData/PICTURE_TYPE_JPEG
           "PICT" XSLFPictureData/PICTURE_TYPE_PICT 
           "PNG" XSLFPictureData/PICTURE_TYPE_PNG 
           "TIFF" XSLFPictureData/PICTURE_TYPE_TIFF 
           ;;"WDP" XSLFPictureData/PICTURE_TYPE_WDP - no longer suported
           "WMF" XSLFPictureData/PICTURE_TYPE_WMF 
           "WPG" XSLFPictureData/PICTURE_TYPE_WPG}]
    (get m type-name)))

;; given a slide, adds picture file to ppt 
(defn ^XSLFSlide add-picture [ppt slide filename & type]
  (let [data (.addPicture ppt
                          (picture->data filename) XSLFPictureData/PICTURE_TYPE_PNG)]
    (.createPicture slide data)) slide) ;; returns slide

;; Given a ppt, creates new slide and adds picture from file for each file
(defn ^XMLSlideShow add-pictures [ppt filenames]
  (doseq [file filenames]
    (add-picture ppt (^XSLFSlide .createSlide ppt) file)) ppt) ;; returns ppt

;; Reads ppt from file and returns a ppt object 
(defn ^XMLSlideShow read-ppt [filename]
  (^XMLSlideShow XMLSlideShow. (^FileInputStream FileInputStream. filename)))

;; Returns java SlideLayout obj from type 
  (defn get-slidelayout-type [type-name]
  (let [m {"BLANK" SlideLayout/BLANK
           "CHART" SlideLayout/CHART 
           "CHART_AND_TEXT" SlideLayout/CHART_AND_TEXT
           "CLIP_ART_AND_TEXT" SlideLayout/CLIP_ART_AND_TEXT 
           "CLIP_ART_AND_VERT_TX" SlideLayout/CLIP_ART_AND_VERT_TX  
           "CUST" SlideLayout/CUST
           "DGM" SlideLayout/DGM
           "FOUR_OBJ" SlideLayout/FOUR_OBJ  
           "MEDIDA_AND_TX" SlideLayout/MEDIA_AND_TX  
           "OBJ_AND_TWO_OBJ" SlideLayout/OBJ_AND_TWO_OBJ  
           "OBJ_AND_TX" SlideLayout/OBJ_AND_TX  
           "OBJ_ONLY" SlideLayout/OBJ_ONLY  
           "OBJ_OVER_TX"   SlideLayout/OBJ_OVER_TX   
           "OBJ_TX"  SlideLayout/OBJ_TX
           "PIC_TX"  SlideLayout/PIC_TX
           "SECTION_HEADER" SlideLayout/SECTION_HEADER
           "TBL"  SlideLayout/TBL
           "TEXT" SlideLayout/TEXT
           "TEXT_AND_CHART" SlideLayout/TEXT_AND_CHART  
           "TITLE" SlideLayout/TITLE
           "TITLE_AND_CONTENT" SlideLayout/TITLE_AND_CONTENT
           "TITLE_ONLY" SlideLayout/TITLE_ONLY
           "TWO_COL_TX"  SlideLayout/TWO_COL_TX
           "TWO_OBJ"  SlideLayout/TWO_OBJ
           "TWO_OBJ_AND_OBJ"  SlideLayout/TWO_OBJ_AND_OBJ
           "TWO_OBJ_AND_TX"  SlideLayout/TWO_OBJ_AND_TX
           "TWO_OBJ_OVER_TX"  SlideLayout/TWO_OBJ_OVER_TX
           "TWO_TX_TWO_OBJ"  SlideLayout/TWO_TX_TWO_OBJ
           "TX_AND_CLIP_ART"  SlideLayout/TX_AND_CLIP_ART
           "TX_AND_MEDIA"  SlideLayout/TX_AND_MEDIA
           "TX_AND_OBJ"  SlideLayout/TX_AND_OBJ
           "TX_AND_TWO_OBJ"  SlideLayout/TX_AND_TWO_OBJ
           "TX_OVER_OBJ"  SlideLayout/TX_OVER_OBJ
           "VERT_TITLE_AND_TX"  SlideLayout/VERT_TITLE_AND_TX
           "VERT_TITLE_AND_TX_OVER_CHART" SlideLayout/VERT_TITLE_AND_TX_OVER_CHART 
           "VERT_TX" SlideLayout/VERT_TX}]
    (get m type-name)))

;; Creates a new slide on new-ppt with layout from template file
(defn ^XMLSlideShow format-layout [template-file new-ppt]
  (let [template (read-ppt template-file) 
        slide-master (first (.getSlideMasters template))
        layout (.getLayout slide-master SlideLayout/TITLE_AND_CONTENT )]
    (^XSLFSlide .createSlide new-ppt layout)) new-ppt)

;; Creates a new slide on new-ppt with layout from template file
;; where slide type determined by type arg (integer - from SlideLayout vars)
(defn ^XMLSlideShow format-layout-type [template-file new-ppt type]
  (let [template (read-ppt template-file)
        slide-master (first (.getSlideMasters template))
        layout (.getLayout slide-master type)]
    (^XSLFSlide .createSlide new-ppt layout)) new-ppt)

;; Copy nth slide from source-file and appends it to current-ppt 
(defn ^XSLFSlide copy-remote-slide [source-file n current-ppt]
  (let [template (read-ppt source-file)
        info (nth (^XSLFSlide .getSlides template) n)
        slide (^XSLFSlide .createSlide current-ppt)]
    (^XSLFSlide .importContent slide info))) ;;returns new-slide

;; Prints out all layouts that are available in ppt
(defn print-layouts [ppt] ;; Java ppt obj -> nil (Standard Out) 
  (println "Available slide layouts: ")
  (doseq [m (.getSlideMasters ppt)]
    (doseq [l (.getSlideLayouts m)]
      (println (.getType l))))) 

;; Returns java theme object from slide
(defn ^XSLFTheme get-theme [slide] ;; Java slide obj -> java theme obj
  (^XSLFTheme .getTheme slide))

;; Returns java theme object from slide from [filename].pptx
(defn ^XSLFTheme get-remote-theme [filename]
  (let [ppt (read-ppt filename)]
    (get-theme (new-slide ppt))))

;; Returns true if filename is specified as a png file
(defn png? [filename]
  (re-matches #"[\d\w_-]*\.png" filename))

;; Returns a seq of filenames for png files is the current/given directory
(defn find-images [& dir] ;; Optional - alternative directory to look in
  (let [dir  (if dir (first dir) ".")]
    (filter png? (vec  (^String .list (^File File. dir))))))

;; Returns a seq of filenames for files of given type in current/given directory
(defn filter-filetype [type & dir] ;; String (file type), Optional alternative directory
  (let [dir  (if dir (first dir) ".")]
    (filter #(re-matches (re-pattern (str "[\\d\\w_-]*." type)) %)
            (^String .list (^File File. dir)))))
