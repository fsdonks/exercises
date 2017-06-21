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

;;Returns the slowest speed in the group (Can handle null arguments)
(defn get-speed [x y] ;;Gets the slowest speed of the group 
  (cond 
    (and (not x) (not y)) 0
    (and  x (not y)) (get people x)
    (and y (not x))  (get people y)
    (and x y) (max (get people x) (get people  y))))

(defn rmv [val col]
  (filter #(not= val %) col))

;;Returns new "state" with x y moved from start to crossed and adds time
(defn cross [x y state]
  (try
    (let [new-start (dissoc (dissoc (get state :start) x) y)
          new-crossed  (merge {x (get people x) y (get people y)}
                              (get state :crossed))
          new-time (+ (get-speed x y) (get state :time))] (->
                                                           ;; Makes new "state" with updated values
                                                           (assoc state :crossed new-crossed)
                                                           (assoc :start new-start)
                                                           (assoc :time new-time)))
    ;; Ignores case when given duplicate keys 
    (catch IllegalArgumentException e)))

;;Returns new "state" with x moved from crossed to start and adds time
(defn cross-back [x state]
  (let [new-crossed (dissoc (get state :crossed x) x)
        new-start (merge (get state :start)  (array-map x (get people x)))
        new-time (+ (get-speed x x) (get state :time))] (->
                                                         (assoc state :crossed new-crossed)
                                                         (assoc :start new-start)
                                                         (assoc :time new-time))))

;;Finds the minimal (fastest) pairing of people in the supplied set
(defn minimal-pair [people]
  (let [pairs (into [] (reduce into  #{} 
                               (for [p (keys people)]
                                 (for [q (rmv p (keys people))]
                                   (into [] #{p q})))))] ;;Sets used to ignore duplicate pairings
    (first (sort-by #(get-speed (first %) (last %)) pairs))))

;; Finds the fastest single person in people
(defn fastest-person [people]
  (first (sort-by #(get-speed % %) (keys people))))

;; take min(time(x,y)+2min(x,y) , time(x,f) + time (y,f) + f)
;; -> Will always be optimal to cross back with fastest person 
(comment 
;;Returns the fastest combination on pairs
(defn minimize-time [f x y] ;; f is fastest person in whole group
  (let [timeXY (+ (get-speed x y) (* 2 (min (get-speed x x) (get-speed y y))))
        timeF  (+ (get-speed f x) (get-speed f y) (get-speed f f))]
    (if (< timeXY timeF)
      [x y]
      [[x f] [y f]])))) ;;this would be used if restraints of problems change

;;Returns the next state
(defn next-state [state f]
  (let [start (get state :start) crossed (get state :crossed)]
  (cond
    (zero? (count start)) ;;Base Case, when no one at start, then done.
    nil
    (contains? crossed f)
    (cross-back f state) ;; If not done and fastest has crossed, cross back 
    (contains? start f) ;;Otherwise, cross next person with fastest peorson
    (let [pair (minimal-pair start)]
      (cross (first pair) (last pair) state)))))

;; Iterates through next states until eveyone has crossed
(defn solve [state] 
  (let [f (fastest-person (get state :start))] 
    (take-while #(not (not %)) (iterate #(next-state % f) state))))
      
(pprint (solve state))


;;T: Uh oh, I think this is wrong bro! Looks like we can do it in 15!
