(ns exercises.causes)
;;Taken from the popular "causes" problem that swept the internet
;;years back...

;; We define a “friend” of a source word as any candidate word in the
;; dictionary that may be found altering the source word by a single
;; character.  Alterations include substituting letters, removing
;; letters, or adding a letters.  We can define the “social network” of a
;; source word as any candidate word that is a friend of the source word,
;; or a friend of the candidate word.  Given the provided set of words,
;; what is the size of the “social network” for the word “causes” ?

;;[you can find the word data in ./resources/words.list] 

;; File path
;;"C:\\Users\\michael.m.pavlak\\Workspace\\exercises\\resources\\word.list"

;;(def source-word "causes")

;; Reads in all words from file and returns a vector
(defn read-words [filename] ;; String (filename) -> Vector
  (with-open [r (clojure.java.io/reader filename)]
    (reduce conj [] (line-seq r))))

;; Filters out any word with length difference more than 1.
(defn filter-by-length [s words] ;; String, Collection -> filtered coll
  (filter #(if (<= (Math/abs (- (count s) (count %))) 1) true false) words))


;; Assumes word is only 1 different in length
;; Returns true when the source word s is only different than
;; word by 1 charactor (subsituted charactor case)
(defn filter-substitution [s word] ;; String, String -> Boolean
  (let [num-false  (->>
                    (map = (seq s) (seq word))
                    (filter #(= false %))
                    (count))]
    (= 1 num-false)))

;; Assumes word is only 1 different in length 
;; Returns true when the souce word s is only different than
;; word by 1 charactor (additional charactor or removed)
(defn filter-count [s word] ;; String, String -> Boolean
  (->>
   (map = (seq s) (seq word))
   (every? #(= true %))))

;; Filters the entire list of words for "friends" of source words s
(defn filter-source [s words] ;; String, coll Strings -> col Strings
  (let [count-s (count s) wordsf (filter-by-length s words)]
    ;; wordsf is a coll of words that different by less than 1 
    (filter #(if (= count-s (count %)) 
               (filter-substitution s %) ;;if same length, check for substitution
               (filter-count s %)) wordsf))) ;;otherwise check for add/sub

(defn social-network [s words]

(reduce (fn [acc s]
          (concat acc (filter-source s wds))) '() (filter-source "causes" wds))
