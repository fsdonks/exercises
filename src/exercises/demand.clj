(ns exercises.demand
  (:require 
   [clojure.java [io :as io]]
   [exercises.util :as util]
   [spork.util [io :refer [list-files fpath fname fext write! writeln!]]])
  (:import [java.io File]))

(set! *warn-on-reflection* true)

;; ===== READING FILE =========================================================

;;Reads lines from csv file 
(defn csv->lines [filename]
  (let [v (transient [])]
    (with-open [rdr (clojure.java.io/reader filename)]
      (doseq [line (line-seq rdr) :let [data (clojure.string/split line #",")]]
        (conj! v data)))
    (persistent! v)))

;;Given a function to format key, creats a map from lines with function called on first value in line
(defn lines->map [lines fn-key]
  (reduce conj (for [val lines] {(fn-key (first val)) val})))

;; Reads the data from a csv file and creates a map using optional fn to format key
(defn file->map [filename & fn-key]
  (lines->map (csv->lines filename) (if fn-key (first fn-key) identity)))

;; If first value of val not unique, use multiple vals as keys
(defn lines->vecmap [lines fn1 fn2]
  (reduce conj (for [val lines] {[(fn1 val) (fn2 val)] val})))

(defn file->vecmap [filename fn1 fn2]
  (lines->vecmap (csv->lines filename) fn1 fn2))
;; ========================================================================         

;; ===== MAP FORMATS AND CONSTANTS ============================================== 
;; multiplers used to convert notation to days
(def quart-day-mult 91)
(def oct-data-mult 8)

;;Builds index map from ordered header collection
(defn build-index [ks]
  (reduce conj
          (for [r (range (count ks))] (assoc {} (keyword (nth ks r)) r))))

(def v-map 
  (let [keys ["force-code" "vignette" "att2" "att3" "time-start"]]
    (build-index keys)))
(def v-cons 
  (let [keys ["force-code" "src2" "src" "title" "str" "quantity" "title_10" "non-rot"]]
    (build-index keys)))

(def forge
  (let [keys ["src" "title" "str" "branch-code" "branch-label" "service" "time-start"]]
    (build-index keys)))
;; ========================================================================  

;; ===== HELPER FUNCTIONS FOR FORMATTING TIME DATA ==================================

;; Removes empty time values from list
(defn filter-times [line time-start] 
  (let [times  (zipmap (for [i (range (- (count line) time-start))] i)
                       (drop time-start line))
        nil-times (filter #(= "" (get times %)) (keys times))]
    (let [t (transient times)]
      (doseq [n nil-times] (dissoc! t n))
      (persistent! t))))

;;Helper function -  Returns the first continous sequence in x 
(defn cont-seq [[x :as s]]
  (take-while identity (map #(#{%1} %2) s (iterate inc x))))

;; Helper function - splits x by first cont. seq
(defn split-by-seq [x]
  (split-at (+ 1 (.indexOf ^java.util.List  x (last (cont-seq x)))) x))

;; Returns a list of sequences that are continous 
(defn get-seqs [x & seqs]
  (let [seqs (if seqs (first seqs) {})]
    (if (= () x)
      (vals seqs)
      (let [s (split-by-seq x)]
        (get-seqs (last s) (assoc seqs (gensym) (first s)))))))

;;(defn const-seq? [sq-m] 
;;  (count (filter #(= false %)
;;                 (map #(= (get sq-m (first (keys sq-m))) (get sq-m %)) (keys sq-m)))))

;; Returns a map made from the keys and values in map m
(defn sub-map [m ks]
  (zipmap ks (map #(get m %) ks)))

;; Helper function for determining continous sequences
(defn partition-keys [m val]
  (partition-by #(= val (get m %)) (keys m)))

;; Helper function for determining continous sequences
(defn unique-vals [m]
  (vec (into #{} (map #(get m %) (keys m)))))

;; Could possible change to use iterate instead of recursion
;; Returns a nestest collection of keys for continous sequences
(defn ->split [m]
  (let [vals (unique-vals m)]
    (if (= 1 (count vals)) [m]
        (do (let [par (partition-keys m (first (unique-vals m)))]
              (flatten (conj []
                             (->split (sub-map m (first (next par))))
                             (->split (sub-map m (first par))))))))))

;; Given a line read from csv file and constant map (forge or v-map), returns a values fommated into map
;; Only used for vignette map and forge data, don't use to build vignette consolidate.
(defn tdata->map [line m]
  (let [sm (filter-times line (:time-start m)) seqs (get-seqs (sort (keys sm)))]
    (when (<= (:time-start m) (count line))
      (->
       (reduce conj (for [k (keys m)] (assoc {} k (nth line (k m)))))
       (assoc :time-start (:time-start m))
       (assoc :times
              (flatten
               (for [q seqs]
                 (for [t (->split (sub-map sm q)) :let [ks (sort (keys t))]]
                   {:start (first ks) :duration (- (last ks) (first ks))
                    :quantity (get t (first ks))}))))))))

;; Given map m, formats time data for all lines in file (used for forge and vignette map data)
(defn tmap->data [filename m] 
  (let [data (file->map filename)]
    (filter #(identity %) (for [line (keys data)]
                            (tdata->map (get data line) m)))))

;; Builds vignette map from file and returns data formmated into map
(defn vmap->data [filename]
  (let [tm (tmap->data filename v-map)]
    (reduce conj
            (for [m tm] (assoc {} (:force-code m) m)))))
;; ========================================================================

;; ===== HELPER FUNCTIONS SPECIFIC TO FORCE CODES ===================================
(defn check-fc [data c]
  (= c (str (first (:force-code data)))))

;; Checks if is vignette/scenario based on force-code value
(defn vignette? [data] (check-fc data "V"))
(defn scenario? [data] (check-fc data "S"))

;; Filters out classification labels ((U) or (C), ect) when used infront force-code label 
(defn filter-fc [x]
  (first (filter #(not= "" %) (clojure.string/split x #"\([A-Z]\) "))))
;; =======================================================================

;; ====== FUNCTIONS TO BUILD VIGNETTE CONSOLIDATED DATA ============================

;; Helper function: takes line of considate data and formats into map
(defn vcons->map [cline]
  (when (>= (count cline) (count v-cons))
    (reduce conj
            (for [k (keys v-cons) :let [data (if (zero? (k v-cons))
                                               (filter-fc (nth cline (k v-cons)))
                                               (nth cline (k v-cons)))]]
              (assoc {} k data)))))

;; Builds map from vignette consolidate file 
(defn vcons->data [filename]
  (let [data (file->vecmap filename
                           #(filter-fc (nth % (:force-code v-cons)))
                           #(nth % (:src v-cons)))]
    (for [cline (keys data)]
      (vcons->map (get data cline)))))
;; =======================================================================

;; ====== FUNCTIONS TO BUILLD FORGE DATA ========================================

;; Function to get force code from file name for forge data 
(defn forge-file->fc [forge-file]
  (first (clojure.string/split
          (last (clojure.string/split forge-file
                                      #"FORGE_")) #".csv")))

;; Function to get forge-file based on force code and standard naming convention
(defn fc->forge-file [fc & root]
  (str (if root  (str (first root) "\\") "") "FORGE_" fc ".csv"))


;; Function to get force code from filename for forge data, may need to change naming convention ...
;;(defn filename->fc [filename]
;;  "S - g")
;;  ;;"FORCE-CODE from file")

;;(defn fc->filename [fc]
;;  ;;ffile)
;;  "Filename that corresponds to FORCE-CODE")

;; Reads forge file and puts data into map and adds force-code
(defn forge->data [filename] 
  (let [fc (forge-file->fc filename) data (tmap->data filename forge)]
    (filter #(number? (read-string (:branch-code % " ")))
            (for [m data] (assoc m :force-code fc)))))
;; =======================================================================

;; ===== FUNCTIONS TO FORMAT MERGE DATA FROM DIFFERNT FILES TOGETHER ===================

;; Merges vignette data and forge/vignette consilidated data, forge data requires force-code arg to be passed
(defn merge-vignette [vm-data m & fc]
  (let [fc (if fc (first fc) (:force-code m))]
    (merge (get vm-data fc) m)))

;; Unwraps nested times map and does additional formatting based on data type
(defn expand-vignette [vmerged]
  (for [time (:times vmerged) :let [mult (if (vignette? vmerged) quart-day-mult oct-data-mult)]]
    [(if (vignette? vmerged)
       (read-string (:quantity vmerged)) 
       (read-string (:quantity time))) 
     (inc (* mult (:start time)))
     (* mult (inc (:duration time)))
     (:src vmerged)
     (:force-code vmerged)
     (:vignette vmerged)
     (:title_10 vmerged)
     (:title vmerged)]))

;; Using filter would be faster, O(N),  instead of sorting O(N log N)
;; but keeping this here as example for how to use partition by
(defn key-type [ks pred]
  (->>
   (sort-by pred ks)
   (partition-by pred)
   (first)))
;; =========================================================================

;; ===== FUNCTIONS TO WRITE MERGED DATA TO FILE =====================================

;; Converts vignette consolidate data to list of formatted vectors
(defn vcons->lines [v vcons]
  (reduce into 
          (for [c vcons]
            (expand-vignette (merge-vignette v c)))))

;; Converts forge data to list of formatted vectors
(defn forge->lines [v forges fc]
  (reduce into
          (for [f forges]
            (expand-vignette
             (assoc
              (merge-vignette v f fc)
              :times (:times f))))))

;; Uses vignette map to create list of demands from forge files
(defn vmap->forge-demands [vm & root]
  (reduce into 
          (for [fc (filter #(= "S" (str (first %))) (keys vm)) 
                :let [fdata (forge->data (fc->forge-file fc (if root (first root))))]]
            (forge->lines vm fdata fc))))

;; Uses vignette map to create list of demands from vignette consolidated data
(defn vmap->vignette-demands [vm cons-filename]
  (let [vcons (filter #(= "V" (str (first (:force-code %)))) (vcons->data cons-filename))]
    (vcons->lines vm vcons)))

;; Builds list of all demands from vignette file and vignette consolidate file
(defn build-demand [vfile cfile root]
  (let [vm (vmap->data vfile)]
    (into (vmap->forge-demands vm root) (vmap->vignette-demands vm cfile))))

;; Writes list of demands to outfile 
(defn demands->file [demands outfile]
  (with-open [w (io/writer outfile)]
    (doseq [line (into [["Quantity" "Start" "Duration" "SRC" "Vignette" "Operation" "Title_10" "IO_Title"]] demands)]
      ;;(println line)
      (doseq [d line]
        ;;(println d)
        (write! w (str d)) (write! w ","))
      (writeln! w ""))))

;; ==========================================================================

;; ===== FUNCTIONS TO AUTOMATE MAKING DEMAND FILES GIVEN THE ROOT DIR ====================

;; Returns true when file is filetype 
(defn is-filetype? [filename filetype]
  (= 2 (count (clojure.string/split filename (re-pattern filetype)))))

(defn vcons-file? [filename] (is-filetype? filename "CONSOLIDATED_"))
(defn vmap-file? [filename] (is-filetype? filename "MAP_"))
(defn forge-file? [filename] (is-filetype? filename "FORGE_"))

;; Gets filenames of filetype from root dir 
(defn root->filetype [root fn-type]
  (filter fn-type (map #(.getName ^java.io.File %) (.listFiles (clojure.java.io/file root)))))

;; Creates Demand records from files in the root directory
;; If more than 1 vignette map or vignette consolidated file is in the directory, only the first one is used
(defn root->demand-file [root & outfile]
  (let [outfile (if outfile (first outfile) (str (last (clojure.string/split root #"[\\ | /]")) "_DEMAND.csv"))
        ;; If multiple maps or consolidate files, filll only use the first one
        vfile (str root "\\" (first (root->filetype root vmap-file?)))
        cfile (str root "\\" (first (root->filetype root vcons-file?)))]
    (demands->file (build-demand vfile cfile root) (str root "/" outfile))))
    

