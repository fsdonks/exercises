"Namespace for function that interact with the System's native clipboard for copying and pasting file and images"

(ns exercises.clipboard
  (:import
   [java.awt Toolkit]
   [java.awt.datatransfer Transferable DataFlavor]
   [java.io File])) 

"Class used override 'Transferable' method and pass information to the clipboard"
(gen-class
 :name exercises.clipboard.clip
 :state state
 :init init
 :prefix "-"
 :implements [java.awt.datatransfer.Transferable])

(comment
"Class used as an instance of a 'ClipboardOwner'. Used to notify when ownership of data in clipboard is lost." 
(gen-class
 :name exercises.clipboard.own
 :init init
 :prefix "_"
 :implements [java.awt.datatransfer.ClipboardOwner])
)
;;(compile 'exercises.clipboard) ;; Need to compile so that classes exist


;; Sets the value of a class variable 
(defn set-field [this key value] (swap! (.state this) into {key value}))

;; Returns the value of a class variable  
(defn get-field [this key] (@(.state this) key))

;; ===== Clipboard Owner Implementation =================================
;; Can alternativly remove this class and just give null argeument
;; in constructor for 'transferable' instance
(comment
(defn _init [] )  
(defn _lostOwnership [this cb conts]
  (println "clipboard lost ownership.")))
;; ====================================================================

(defn get-files [this] (get-field this :files)) ;; gets current files in 'transferable'
(defn add-file [this file] ;; adds a file to 'transferable' 
  (set-field this :files (conj (get-field this :files) (File. file))))
(defn add-all-files [this files] (doseq [file files] (add-file this file))) ;adds all 

;; =====================================================================


;; ===== "Transferable" implementation for files =============================
(defn -init [] [[] (atom {:files []})])

;; All methods override how the methods are implemented in the 'transferable' interface

;; Used to allow java to tell the native system how to handel the data being sent to
;; the system's native clipboard; list of files being copied to clipboard
(defn -getTransferDataFlavors [this] 
  (into-array DataFlavor [DataFlavor/javaFileListFlavor]))

;; always return true, no need for error checking, only ever using one DataFlavor type
(defn -isDataFlavorSupported [this ^DataFlavor flavor] true) 

;; Returns the data stored in the transfer proxy 
(defn -getTransferData [this ^DataFlavor flavor] (get-files this))
  
;; =====================================================================

;; creates a temp file which is deleted on exit
(defn temp-file [filename]
  (let [f (java.io.File. filename)]
    (.deleteOnExit f) f))

;; creates new 'transfer proxy' with files added to data to be copied
(defn new-file-trans [files]
  (let [ft (exercises.clipboard.clip.)]
    (add-all-files ft files) ft))

;; Copies all files to the system clipboard to be used by external programs 
(defn copy-file-to-clip [files]
  (let [ft (new-file-trans files)
        cb (.getSystemClipboard
            (java.awt.Toolkit/getDefaultToolkit))]
    (.setContents cb ft (proxy [java.awt.datatransfer.ClipboardOwner] []
                          (lostOwnership [cb conts]
                            (println "Proxy override for lost ownership"))))))

    ;;(exercises.clipboard.own.))))

;; Creates a temp file from buffered image and copies it to the system clipboard
(defn image-to-temp [buff]
  (let [filename (str ".\\resources\\~" (gensym) ".jpg")]
    (javax.imageio.ImageIO/write buff, "jpg", (File. filename))
    filename))

;; Copies all buffered images to system clipboard (iterates over image-to-temp)
(defn copy-images-to-clip [imgs]
  (copy-file-to-clip (for [i imgs] (image-to-temp i))))
    
        



