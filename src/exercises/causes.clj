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

(def source-word "causes")

(def file "C:\\Users\\michael.m.pavlak\\Workspace\\exercises\\resources\\word.list") 

(defn read-words [filename]
  (with-open [r (clojure.java.io/reader filename)]
    (count (reduce conj [] (line-seq r)))))

