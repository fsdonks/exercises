;;A list of useful Project Euler problems for
;;introductory learning...
(ns exercises.eulers)
;;Euler 102, 107

;; Triangles files path:
;; C:\Users\michael.m.pavlak\Workspace\exercises\resources\triangles.txt
;; "C:\\Users\\michael.m.pavlak\\Workspace\\exercises\\resources\\triangles.txt"

;;(def file  "C:\\Users\\michael.m.pavlak\\Workspace\\exercises\\resources\\triangles.txt")

;; Returns true when a  real number n is in the range [0 1]
(defn between01? [n]  ;; real number -> Boolean
  (and (<= 0 n) (<= n 1))) ;; n can be fraction or real number

;; Takes a line of 6 comma separated points
;; Converts into map representation of triangle 
(defn line-to-tri [line] ;; String -> Map
  (let [pts (map read-string (clojure.string/split line #","))
        k [:x1 :y1 :x2 :y2 :x3 :y3]]
    (zipmap k pts)))

;;T: Are you using barycentric coordinates?

;; Contains zero if paramatrized points t1 and t2 have the properties:
;; 0<=t1<=1 and 0<=t2<=t and t1+t2<=1
(defn tri-contains-zero? [tri] ;; Map -> Boolean
  (let [x1 (get tri :x1) y1 (get tri :y1) ;;unwraps map into x,y points
        x2 (get tri :x2) y2 (get tri :y2)
        x3 (get tri :x3) y3 (get tri :y3)
        ;;denominator =  x1*(y2-y3) + y1*(x3-x2) + x2*y3 - y2*x3
        d (- (+ (* x1 (- y2 y3)) (* y1 (- x3 x2)) (* x2 y3)) (* y2 x3))
        ;; t1 = -x1*y3 + y1*x3 / d
        t1 (/ (+ (* (* -1 x1) y3) (* y1 x3)) d)
        ;; t2 = -x1*y2 + y1*x2 / -d
        t2 (/ (+ (* (* -1 x1) y2) (* y1 x2)) (* -1 d))]
    (and (between01? t1) (between01? t2) (<= (+ t1 t2) 1))))

;;T: alternately..
;; (let [{:keys [x1 x2 x3 y1 y2 y3]} tri
;;       ... ]
;;   )

;; Counts the number of triangle in the file which contain the point (0,0)
(defn count-zero-triangles [filename] ;; String (filename) -> Number
  (with-open [r (clojure.java.io/reader filename)]
     (count (filter #(tri-contains-zero? (line-to-tri %))
                    (reduce conj [] (line-seq r))))))

