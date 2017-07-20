(ns exercises.demand
  (:require 
   [clojure.java [io :as io]]
   [exercises.util :as util]
   [spork.util [io :refer [list-files fpath fname fext]]])
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
  ;;(let [max-count (reduce max (for [data lines] (count data)))]
    (reduce conj (for [val lines] {(fn-key (first val)) val})))

;; Reads the data from a csv file and creates a map using optional fn to format key
(defn file->map [filename & fn-key]
  (lines->map (csv->lines filename) (if fn-key (first fn-key) identity)))
  
(defn lines->vecmap [lines fn1 fn2]
  (reduce conj (for [val lines] {[(fn1 val) (fn2 val)] val})))

(defn file->vecmap [filename fn1 fn2]
  (lines->vecmap (csv->lines filename) fn1 fn2))


;; ========================================================================         

;; ===== MAP FORMATS AND CONSTANTS ============================================== 
;; multiplers used to convert notation to days
(def quart-day-mult 91)
(def oct-data-mult 45)

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

(def pfile "C:\\Users\\michael.m.pavlak\\Desktop\\phases.csv")
(def phases
  (let [p (csv->lines pfile)]
    (reduce conj
            (for [i (range (count (first p)))]
              (assoc {} (nth (first p) i) (read-string (nth (last p) i)))))))

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

;;Given m, formats time data for line in file (used for forge and vignette map data)
;;(defn tdata->map [line m]
;;  (when (<= (:time-start m) (count line))
;;    (->
;;     (reduce conj (for [k (keys m)] (assoc {} k (nth line (k m)))))
;;     (assoc :time-start (:time-start m))
;;     (assoc :times
;;            (for [q (get-seqs (sort (keys (filter-times line (:time-start m)))))]
;;              {:start (first q) :duration (- (last q) (first q))})))))

(defn const-seq? [sq-m]
  (count (filter #(= false %)
                 (map #(= (get sq-m (first (keys sq-m))) (get sq-m %)) (keys sq-m)))))

(defn sub-map [m ks]
  (zipmap ks (map #(get m %) ks)))

(defn partition-keys [m val]
  (partition-by #(= val (get m %)) (keys m)))

(defn unique-vals [m]
  (vec (into #{} (map #(get m %) (keys m)))))

;; Could possible change to use iterate instead of recursion
(defn ->split [m]
  (let [vals (unique-vals m)]
    (if (= 1 (count vals)) [m]
        (do (let [par (partition-keys m (first (unique-vals m)))]
              (flatten (conj []
                             (->split (sub-map m (first (next par))))
                             (->split (sub-map m (first par))))))))))

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
                    :quantity (get t (first ks))}
                           ;; (get-seqs (sort (keys (filter-times line (:time-start m)))))]
                ;;(for [t (->split (sub-map (filter-times line (:time-start m)
                  ;;{:start (first q) :duration (- (last q) (first q))}
                  ))))))))


;; Given map m, formats time data for all lines in file (used for forge and vignette map data)
(defn tmap->data [filename m] 
  (let [data (file->map filename)]
    (filter #(identity %) (for [line (keys data)]
                            (tdata->map (get data line) m)))))

(defn vmap->data [filename]
  (let [tm (tmap->data filename v-map)]
    (reduce conj
            (for [m tm] (assoc {} (:force-code m) m)))))
            



;; ========================================================================

;; ===== HELPER FUNCTIONS SPECIFIC TO FORCE CODES ===================================

(defn check-fc [data c]
  (= c (str (first (:force-code data)))))

;; Checks if is vignette based on force-code value
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


;; Reads vignette considate file and put data into map
;;(defn vcons->data [filename]
;;  (let [data (file->map filename filter-fc)]
;;    (reduce conj
;;            (for [cline (keys data) :let [m (vcons->map (get data cline))]]
;;              (assoc {} (:force-code m)  m)))))

(defn vcons->data [filename]
  (let [data (file->vecmap filename
                           #(filter-fc (nth % (:force-code v-cons)))
                           #(nth % (:src v-cons)))]
    (for [cline (keys data)]
      (vcons->map (get data cline)))))
             

;; =======================================================================


;; ====== FUNCTIONS TO BUILLD FORGE DATA ========================================

;; Function to get force code from filename for forge data, may need to change naming convention ...
(defn filename->fc [filename]
  "S - g")
  ;;"FORCE-CODE from file")

(defn fc->filename [fc]
  "Filename that corresponds to FORCE-CODE")

;; Reads forge file and puts data into map and adds force-code
(defn forge->data [filename] 
  (let [fc (filename->fc filename) data (tmap->data filename forge)]
    (for [m data]
      (assoc m :force-code fc))))

;; All forge maps from the same file will have the same force code

;; =======================================================================

(defn merge-vignette [vm-data m & times]
  (merge m (get vm-data (:force-code m))))   

(defn expand-vignette [vmerged] ;; add argument to format times (quarterly/8 day periods -> days)
  (for [time (:times vmerged)]
    [(read-string (:quantity time)) ;; will format times to days here
     (:start time)
     (:duration time)
     (:src vmerged)
     (:force-code vmerged)
     (:vignette vmerged)
     (:title_10 vmerged)
     (:title vmerged)]))

(defn vcons->lines [v vcons]
  (reduce into (for [c vcons]
    (expand-vignette (merge-vignette v c)))))

(defn forge->lines [v forges]
  (reduce into
          (for [f forges]
            (expand-vignette
             (assoc
              (merge-vignette v f (:times f))
              :times (:times f))))))

;; Need to find a way to filter out header information
;; lines->file [outfile]

;; (partition-by #(= "V" (str (first %))) (keys v))





;;(defn merge-scenarios [vd s]

;; For testing only 
(def vfile "C:\\Users\\michael.m.pavlak\\Desktop\\test-data.csv")
(def cfile "C:\\Users\\michael.m.pavlak\\Desktop\\test-consolidated.csv")
(def ffile "C:\\Users\\michael.m.pavlak\\Desktop\\test-forge.csv")
   

