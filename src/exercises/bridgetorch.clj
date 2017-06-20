(ns exercises.bridgetorch)
;; A second problem (classic):

;; Four people come to a river in the night.
;; There is a narrow bridge, but it can only hold two people at a time.
;; They have one torch and, because it's night, the torch has to be used when 
;; crossing the bridge.

;; Person A can cross the bridge in one minute, B in two minutes, C in five 
;; minutes, and D in eight minutes.

;; When two people cross the bridge together, they must move at the slower 
;; person's pace.

;; The question is, can they all get across the bridge in 15 minutes or less?

(def people {:a 1 :b 2 :c 5 :d 8})
(def state {:time 0 :start people :crossed {}})

(defn get-speed [x y] ;;Gets the slowest speed of the group 
  (cond
    (and (not x) (not y)) 0
    (and  x (not y)) (get people x)
    (and y (not x))  (get people y)
    (and x y) (max (get people x) (get people  y))))


(defn rmv [val col]
  (filter #(not= val %) col))

(defn cross [x y state]
  (try
    (let [new-start (dissoc (dissoc (get state :start) x) y)
          new-crossed  (merge {x (get people x) y (get people y)}
                              (get state :crossed))
          new-time (+ (get-speed x y) (get state :time))] (->
                                                           (assoc state :crossed new-crossed)
                                                           (assoc :start new-start)
                                                           (assoc :time new-time)))
    (catch IllegalArgumentException e)))
  
(defn cross-back [x state]
  (let [new-crossed (dissoc (get state :crossed x) x)
        new-start (merge (get state :start)  (array-map x (get people x)))
        new-time (+ (get-speed x x) (get state :time))] (->
     (assoc state :crossed new-crossed)
     (assoc :start new-start)
     (assoc :time new-time))))

