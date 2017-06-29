;;A quick namespace for exploring ideas about using
;;the POI libs for munging PPTX presentations programatically.
(ns exercises.powerpoint
  (:require 
             [clojure.java [io :as io]]
             [exercises.util :as util]
             [spork.util [io :refer [list-files fpath fname fext]]])
   (:import [org.apache.poi.xslf.usermodel 
             XMLSlideShow XSLFSlide XSLFPictureData XSLFTheme
             XSLFSlideLayout XSLFSlideMaster SlideLayout] ;any more?
            ;[java.io ;maybe unnecessary, used in demo.
            ; FileInputStream FileOutputStream File]
            ))

;;TODO: Fix reflection warnings.
;; Added wrapper functions to fix reflection warnings
(set! *warn-on-reflection* true)
;;constants and such.
;;==================
;;A map of typename to PictureData enumerated type values (ints),
;;scrapped programtically via reflection.
(def picturedata-types
  (into {} (util/get-enums XSLFPictureData PictureType)))

(defn get-picturedata-type
  "Returns java XSLFPictureData.PictureType (int)  from string type-name"
  [type-name] ;; String -> Int
  (if (integer? type-name) type-name
      (util/get-or-err picturedata-types type-name)))

;note: doing this programatically, ended up catching a
;;fat finger mistake in the original map:
;;"MEDIDA_AND_TX"
(def slidelayout-types (into {} (util/get-constants SlideLayout)))

(defn ^SlideLayout get-slidelayout-type
  "Returns java SlideLayout obj from type."
  [type-name]
  (util/get-or-err slidelayout-types type-name))

;;PPT stuff
;;=========
;; Reads ppt from file and returns a ppt object 
(defn ^XMLSlideShow ->pptx
  "Creates a new Apache POI pptx object.  If a file is specified, 
   coerces the file to a PPTX object."
  ([](XMLSlideShow.))
  ([filename] (XMLSlideShow. (io/input-stream filename))))

(defn ^XSLFSlide ->slide
  "Mutably adds a slide to existing pptx presentation, returns the newly added 
   slide."
  ([^XMLSlideShow ppt] (.createSlide ppt))
  ([^XMLSlideShow ppt layout] (.createSlide ppt (int layout))))

;; Saves ppt obj to file
;;Using with-open to manage resources better...
(defn ^XMLSlideShow save-ppt [^XMLSlideShow ppt filename]
  (with-open [out (io/output-stream filename)]
    (.write ppt out))
  ppt)

;; reads picture data and returns java byte array
(defn ^bytes picture->data [filename] ;; String (filename) -> java byte[]
  (util/file->bytes filename))

;;given a slide, adds picture file to ppt
(defn ^XSLFSlide add-picture
  [^XMLSlideShow ppt ^XSLFSlide slide filename &
   {:keys [format] :or {format "PNG"}}]
  (let [data (.addPicture ppt
                          (picture->data filename)
                          (get-picturedata-type format))]
    (doto slide (.createPicture data)))) ;; returns slide

(defn ^XMLSlideShow add-pictures
  "Given a ppt, creates new slide and adds picture from file for each file"
  [ppt filenames]
  (doseq [file filenames]
    (add-picture ppt (->slide ppt) file))
  ppt) ;; returns pp

;; Wrapper function for getSlideMasters method 
;; Takes a XMLSlideShow ppt and returns an array of XSLFSlideMaster objects
(defn get-slide-masters [^XMLSlideShow ppt]
  (vec (.getSlideMasters ppt)))

;; Wrapper function for getLayout method
;; Takes a XSLFSlideMaster and optional type (int) and returns a XSLFSlideLayout
(defn ^XSLFSlideLayout get-layout [^XSLFSlideMaster slide-master &type]
  (let [type (if type (first type) SlideLayout/TITLE_AND_CONTENT)]
    (.getLayout slide-master type)))

(defn ^XMLSlideShow format-layout
  "Creates a new slide on new-ppt with layout from template file"
  [template-file new-ppt]
  (let [template (->pptx template-file) 
        slide-master  (first (get-slide-masters template))
        layout   (get-layout slide-master SlideLayout/TITLE_AND_CONTENT)
        _        (->slide new-ppt layout)]
    new-ppt))

(defn ^XMLSlideShow format-layout-type
  "Creates a new slide on new-ppt with layout from template file
   where slide type determined by type arg (integer - from SlideLayout vars)"
  [template-file new-ppt type]
  (let [template (->pptx template-file)
        slide-master (first (get-slide-masters  template))
        layout (get-layout slide-master type)
        _      (->slide new-ppt layout)]
    new-ppt))

;; Copy nth slide from source-file and appends it to current-ppt 
(defn ^XSLFSlide copy-remote-slide [source-file n current-ppt]
  (let [template (->pptx source-file)
        info (nth (.getSlides template) n)]
    (doto (->slide current-ppt)
      (.importContent  ^XSLFSlide info)))) ;;returns ->slide

;; Wraper function for getSlideLayouts : takes a XSLFSlide master obj and returns
(defn  ;;array of XSLFSlideLayouts 
  get-slide-layouts [^XSLFSlideMaster m]
  (vec (.getSlideLayouts m)))

;; Wrapper function for get-type : takes a XSLFSlideLayout obj and returns a SlideLayout
(defn ^SlideLayout get-type [^XSLFSlideLayout layout]
  (.getType layout))

(defn print-layouts
  "Prints out all layouts that are available in ppt"
  [ppt] ;; Java ppt obj -> nil (Standard Out) 
  (println "Available slide layouts: ")
  (doseq [m (get-slide-masters ppt)]
    (doseq [l (get-slide-layouts m)]
      (println  (get-type l))))) 

(defn ^XSLFTheme get-theme
  "Returns java theme object from slide, or if a file is 
   specified, from the pptx at the file."
  [tgt] ;; Java slide obj | String -> java theme obj
  (let [^XSLFSlide slide (if (string? tgt)  
                             (->slide (->pptx tgt))
                             tgt)]
    (.getTheme slide)))

;;File filtering and other IO stuff...
;;====================================

(defn png?
  "Returns true if filename is specified as a png file"
  [filename]
  (re-matches #"[\d\w_-]*\.png" filename))

(defn file-names [p]
  (map fname (list-files p)))

(defn find-images
  "Returns a seq of filenames for png files is the current/given directory."
  [& dir] ;; Optional - alternative directory to look in
  (let [dir  (if dir (first dir) ".")]
    (filter png? (file-names dir))))

(defn filter-filetype
  "Returns a seq of filenames for files of given type in current/given directory"
  [type & dir] ;; String (file type), Optional alternative directory
  (let [dir  (if dir (first dir) ".")]
    (filter #(re-matches (re-pattern (str "[\\d\\w_-]*." type)) %)
            (file-names  dir))))
