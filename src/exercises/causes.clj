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
    (reduce conj #{} (line-seq r))))

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

;; =================================================================
(comment ;;Sorting words first - this method won't work, does not consider
  ;;new cases after initial update of network
(defn difference-count [s other]
  (+   (Math/abs (- (count s) (count other)))
       (count (filter #(not %) (map = (seq s) (seq other))))))
    
(defn sort-by-diffs [s words]
  (sort-by #(difference-count s %) words))

(defn in-network? [network candidate]
  (->>
   (map #(difference-count candidate %) network)
   (some #(<= % 1))))

(defn grow-network [network candidate]
  (if (in-network? network candidate)
    (conj network candidate)
    nil))

(defn find-network [s words network  i]
  (let [net (grow-network network (nth words i))]
    (if (not net)
      network
      (recur s words net (inc i)))))
) ;;===============================================================


;; Used in replace and insert functions
(def alpha ["a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q"
            "r" "s" "t" "u" "v" "w" "x" "y" "z"])

;; Replaces (substitutes) the value r at index i in string s
(defn replace-string [s r i] ;;String -> String
  (str (subs s 0 i) r (subs s (unchecked-inc i))))

;; Inserts the value r at index i in string s
(defn insert-string [s r i] ;; String -> String
  (str (subs s 0 i) r (subs s i)))

;; Removes the value at index i in string s
(defn remove-string [s i] ;; String -> String
  (str (subs s 0 i) (subs s (unchecked-inc i))))

;; Finds all words in "words" that can be made by substituting values in word s 
(defn substitutions [s words] ;; String, Set -> Set
  (into #{} (for [i (range (count s)) a alpha 
                  :let [cand (replace-string s a i)] 
                  :when (get words cand)]
              cand)))

;; Finds all words in "words" that can be made by
;; adding/inserting a single value in word s  
(defn additions [s words]  ;; String, Set -> Set
  (into #{} (for [i (range (count s)) a alpha 
                  :let [cand (insert-string s a i)]
                  :when (get words cand)]
              cand)))

;; Finds all words in "words" that can be made by
;; removing a single value in word s
(defn removals [s words] ;; String, Set -> Set
  (into #{} (for [i (range (count s))
                  :let [cand (remove-string s i)]
                  :when (get words cand)]
              cand)))

;; Returns a set of words that are "friends" with the string s given words 
(defn friends-of-word [s words] ;; String, Set -> Set
  (clojure.set/union
   (substitutions s words)
   (additions s words)
   (removals s words)))

;; Returns result of calls to friends of words for each string in ss 
(defn multiple-friends [ss words] ;; Coll, Set -> Set
  (clojure.set/union ss
                     (reduce clojure.set/union
                             (for [s ss]
                               (friends-of-word s words)))))
  
;; Returns multiple-friends of friends of source word s
(defn grow [s words] ;; String, Set -> Set
  (let [g (friends-of-word s words)]
    (if (zero? (count g))
      g
      (multiple-friends g words))))

(defn solve [s words]
  (let [g (multiple-friends (friends-of-word s words) words)]
    (last (take-while #(identity %)
                      (iterate (fn [{:keys [net size] :as m}]
                                 (let [next (multiple-friends net words)]
                                   (if (= (count next) size)
                                     nil
                                     {:net next
                                      :size (count next)})))
                               {:net g :size (count g)})))))


;; =================================================================
(comment ;; Brute force method
  (defn build-network [s words used friends]
    {:friends  (clojure.set/difference (into friends (filter-source s words))
                                       (conj used s))
     :used (conj used s)})

  (defn social-network [s words used friends]
    (let [net (time (build-network s words used friends))]
      (if (zero? (count (get net :friends)))
        (get net :used)
        (recur (first (get net :friends)) words
                                    (get net :used) (get net :friends)))))
  ) ;;=============================================================


