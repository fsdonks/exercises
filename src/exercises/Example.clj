(ns Example)

;;Generates primes upto x 
(defn generate-primes [x]
  (loop [primes {} num 2]
    (if (<= x num)
      (sort (reduce into (vals primes)))
      (recur (update-primes primes num) (inc num)))))

(defn nth-prime [n] ;; scaled distance between primes up to 10^26 is ~2.3
  (last (take n (generate-primes (int (Math/ceil (* (count (str n)) 2.3 n)))))))

(defn gcd [a b]
  (if (zero? b) a (recur b (mod a b))))

(defn abs [a]
  (if (< a 0) (* -1 a) a))

(defn rho [n] ;;Can sometimes fail (Pollard's rho algorithm implementation)
  (let [gx (fn [x] (mod (+ (*' x x) 1) n))] ;; Does not automatically convert to BigInt
    (get (last (take-while #(not (not %))
                           (iterate
                            (fn [{:keys [x y d] :as m}]
                              (if (= d 1)
                                {:x (gx (get m :x))
                                 :y (gx (gx (get m :y)))
                                 :d (gcd (abs (- (get m :x) (get m :y))) n)}
                                nil))
                            {:x (gx 2) :y (gx (gx 2)) :d 1}))) :d)))

(defn prime-factors [n & factors]
  (if (= n 1)
    factors
    (recur (/ n (rho n)) (map int (conj (vec factors) (rho n))))))

