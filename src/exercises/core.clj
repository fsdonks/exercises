(ns exercises.core)

(set! *warn-on-reflection* true)
;;Tom Observations:
;;Why are we using *' ?  That's arbitrary-precision multiplication,
;;i.e. BigInt, which carries a non-trivial performance cost to gain
;;use the ability to work with potentially massive numbers.
;;We typically don't ever need to use this, since the numeric tower
;;in clojure will automatically promote our stuff to bigints if we
;;overflow.  Stick with normal multiplication via * unless you
;;actually need bigint-specific arithmetic (highly rare).
;;Additional notes are prepended with T: ....

;;http://clojuredocs.org/clojure.core/bigint


;======= Old implementation using loops ===========
(defn divides? [n p]
  (= 0 (mod n p)))

;;T: the zero? predicate is actually faster in practice due to
;;some specialization behind the scenes... 
;(defn divides? [n p] (zero? (mod n p)))

;;T: This implementation takes a candidate n, and a seq of
;;known primes, and seeks to prove if the candidate divides?
;;any known prime.
(defn prime? [n & [primes]]
  (if primes ;;T: we know some primes....
    (not (some #(divides? n %) primes))
    (loop [i 3]
      (cond
        (< n 2) false
        (= n 2) true
        (>= i n) true
        (even? n) false
        (= 0 (mod n i)) false
        (<= (mod n i)) (recur (+ i 2))))))

;;T: compute the next prime from a current candidate n....
;;Note: we rebuild our primes EVERY time we call this naively and
;;use prime? .
(defn next-prime [n]
  (loop [i n]
    (if (and (prime? i) (not (= n i))) ;;T: calls to prime? are inefficient.
      i
      (recur (if (even? i)
               (inc i)
               (+ i 2))))))

;;T: looks like it adds to our seq of primes, computing
;;the next from the last-known prime...
;;Problem: We always have to traverse to the last prime if
;;primes is a list/sequence, else if vector we're okay.
;;More importantly, every time we call next-prime, we
;;ignore all previous primes....and thus end up recomputing them
;;over and over again.  Inefficient algorithm?
(defn grow-primes [primes]
  (conj primes (next-prime (last primes))))

(defn first-n-primes [n]
  (loop [i 0 ;;T: I prefer to separate loop bindings by line....
         primes [2]]
    (if-not (< i (- n 1))
      primes
      (recur (inc i) (grow-primes primes)))))

;;T: this appears to be the primary workhorse, along with
;;prime? 
(defn primes-upto-n [n & [primes]]
  (loop [i 0 primes (if primes
                      primes
                      [2])]
    (if (< n (next-prime (last primes)))
      primes
      (recur (inc i) (grow-primes primes)))))

;;T: uses primes-upto-n as a workhorse to generate all primes,
;;then naively filters between min and max.....why the call to vec?
(defn primes-between [min max]
  (vec (filter #(< min %) (primes-upto-n max))))

;;T: looks like a thread-macro reimagination of the earlier
;;primes-between.  why the call to vec?
(defn primes-between [min max]
  (->>
   (primes-upto-n max)
   (filter #(< min %))
   (vec)))

;;T: dunno where this is used, appears to be a random function.
(defn get-factor [n primes]
  (loop [i (last primes)]
    (if (and (divides? n i) (<= 1 i) (prime? i))
      i
      (recur (dec i)))))

;;===New implementation =======================
;;T: this is out of order.  primes-upto-n needs to be forward-declared
;;or this comes later.  As it stands, we're using the old implementation of
;;primte-upto-n here, looping and recomputing primes and all.
(defn prime? [n] (= n (last (primes-upto-n n))))
;;T: good use of composition, but inefficient execution.  Relies on next-prime, which we know will continually re-compute all primes (memoryless, no caching of previous primes....)
(defn nth-prime [n] (last (take n (iterate next-prime 2))))

;;Tom: Why is this sorted?
;;Also, why are you using a hash-set for the accumulator to grow-primes?
;;you're going to create a couple of problems that way, particularly using
;;calls to (last ...) on an unordered container like a hash-set.  Rethink
;;this...
(defn primes-upto-n [n]
  (sort (filter #(<= % n) (reduce grow-primes #{2}
                                  (take n (iterate #(+ % 2) 3))))))
;;Again, uses prime? 
(defn next-prime [n]
  (let [x (inc n)]
    (if (not (prime? x))
      (next-prime (inc n))
      x)))

(defn primes-upto-n [n]
  (loop [nums (range 2 n) ;;T: I like my bindings on separate lines...
         primes []]
    (if (> (*' (first nums) (first nums)) n) ;;T: Why the bignum multiply?
      (into primes nums)
      (recur
       (filter #(not= 0 (mod % (first nums))) nums)
       (conj primes (first nums))))))

;;=====================
(defn number-to-vector [n] (into [] (seq (str n))))
(defn vector-to-number [v] (Integer. (reduce str v)))
(defn rotate-vec [v] (into (vector (last v)) (pop v)))
(defn rotate-digits [n] (->>
                         (number-to-vector n) (rotate-vec) (vector-to-number)))
(defn digits [n] (count (str n)))
(defn rotations [n] (take (digits n) (iterate rotate-digits n)))
(defn circular-prime? [n] (not (some #(not (prime? %)) (rotations n))))

;;=========Euler problems==================================
(defn ! [n] ;;Factorial of N
  (reduce *' (map inc (range n))))

(defn swap [v i & j]
  (if j
    (replace {(nth v (mod i (count v))) (nth v (mod (first j) (count v)))
              (nth v (mod (first j) (count v))) (nth v (mod i (count v)))} v)
    (replace {(nth v (mod i (count v))) (nth v (mod (inc i) (count v)))
              (nth v (mod (inc i) (count v))) (nth v (mod i (count v)))} v)))
(defn swap-item [v item index]
  (try
    (replace {item (nth v (mod index (count v)))
              (nth v (mod index (count v))) item} v)
    (catch IllegalArgumentException e v))) ;;Ignore case where index of item = index

;;=========File IO=============

;;T:Regarding java interop and file I/O, by using these classes
;;instead of the baked in functions from clojure.java.io, you're going
;;to incur a serious hit in terms of Reflection costs.  That is, the
;;JVM can't infer what the type of the args are and which method to
;;dispatch based on for your objects, so it'll reflect, on EACH call.
;;This ends up being a canonical and preventable speed hit in
;;clojure....one of the first things you want to do is avoid
;;reflection entirely.  Fortunately, the clojure libraries already do
;;this for you, but if you end up doing interop with java libraries or
;;things that aren't already wrapped in clojure, you should generally:
;;(set! *warn-on-reflection* true) This will give you warnings when
;;the clojure compiler has to use reflection.

;;T: importing in the middle of function is bad form.  Do it at the
;;beginning of the file in the (ns ...) declaration.
(defn read-file [filename]
  (import java.util.Scanner java.io.File)
  (let [scan (Scanner. (File. filename))]
    (.useDelimiter scan "\r|\n")
    (filter #(not (= % ""))
            (take-while #(not (not %)) (repeatedly #(if (.hasNext scan)
                                                      (.next scan) nil))))))

;;T: This is a common idiom.  Also look at
;;spork.util.general/line-reducer and other utilities for getting
;;lines from files.  Note: line-seq is lazy, while reduce is eager,
;;thus reduce forces the line-sequence to be traversed and realized to
;;build up the resulting vector...
(defn read-file [filename]
  (with-open [r (clojure.java.io/reader filename)]
    (reduce conj [] (line-seq r))))

;;T: importing in the middle of function is bad form.  Do it at the
;;beginning of the file in the (ns ...) declaration.  You're going to get
;;slammed on reflection here as well...
(defn write-file [filename data]
  (import java.io.PrintWriter java.io.File)
  (let [w (java.io.PrintWriter. (File. filename))]
    (doseq [d data] (.println w d))
    (.close w)))

;;T: version with type-hinting...using with-open to eliminate the need for
;;manual resource management.  When wrapping java objects, it's common to
;;look up the api via javadocs ala
;;https://docs.oracle.com/javase/7/docs/api/java/io/BufferedWriter.html
(defn hinted-write-file [filename data]
  (with-open [^java.io.BufferedWriter w (clojure.java.io/writer  filename)]
    (doseq [d data]
      (.write w (str d))
      (.newLine w))))

(defn read-CSV [filename]
  (let [lines (read-file filename)]
    (for [l lines :let [data (clojure.string/split l #",")]] data)))

(defn read-delimited [filename del]
  (let [lines (read-file filename)]
    (for [l lines :let [data (clojure.string/split l (re-pattern del))]] data)))

;;T: fixed mo betta version.
(defn read-delimited2 [filename del]
  (let [lines (read-file filename)
        pat   (re-pattern del)]
    (for [l lines
          :let [data (clojure.string/split l pat)]]
      data)))

(defn read-delimited3 [filename del]
  (let [pat   (re-pattern del)]
    (for [l   (clojure.string/split-lines (slurp filename))
          :let [data (clojure.string/split l pat)]]
      data)))

(defn write-2d [filename data del1 del2]
  (import java.io.PrintWriter java.io.File)
  (let [w (java.io.PrintWriter. (File. filename))]
    (doseq [line data]
      (doseq [d line] (.print w (str d)) (.print w (str del1)))
      (.print w del2))
    (.close w)))
