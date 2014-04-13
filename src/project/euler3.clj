(ns project.euler
  (:use [clojure.contrib.lazy-seqs :only (primes)]
        [clojure.contrib.math]
        [clojure.contrib.combinatorics]))

(defn extenso [n]
  (cond
   (= n 0) ""
   (= n 1) "one"
   (= n 2) "two"
   (= n 3) "three"
   (= n 4) "four"
   (= n 5) "five"
   (= n 6) "six"
   (= n 7) "seven"
   (= n 8) "eight"
   (= n 9) "nine"
   (= n 10) "ten"
   (= n 11) "eleven"
   (= n 12) "twelve"
   (= n 13) "thirteen"
   (= n 14) "fourteen"
   (= n 15) "fifteen"
   (= n 16) "sixteen"
   (= n 17) "seventeen"
   (= n 18) "eighteen"
   (= n 19) "nineteen"
   (< n 30) "twenty"
   (< n 40) "thirty"
   (< n 50) "forty"
   (< n 60) "fifty"
   (< n 70) "sixty"
   (< n 80) "seventy"
   (< n 90) "eighty"
   (< n 100) "ninety"
   (< n 1000) "hundred"
   (= n 1000) "one thousand"))

(defn dezena [n]
  (let [m (quot n 10)
        resto (- n (* m 10))]
    (if (< n 20)
      (str (extenso n))
      (str (extenso n) " " (extenso resto)))))

(defn numero-por-extenso [n]
  (if (= n 1000)
  (extenso n)
  (let [m (quot n 100)
        resto (- n (* m 100))]
    (if (> m 0)
      (str (extenso m) "-hundred"
        (if (> resto 0)
          (str " and " (dezena resto))))
      (dezena n)))))

(defn somente-caracteres? [char]
  (some #(= char %) "abcdefghijklmnopqrstuvwxyz"))

(defn euler-17 []
  (count (filter somente-caracteres? (apply concat (map numero-por-extenso (range 1 1001))))))

;(euler-17)

(numero-por-extenso 342)

(def triangle [[75]
               [95 64]
               [17 47 82]
               [18 35 87 10]
               [20  4 82 47 65]
               [19  1 23 75  3 34]
               [88  2 77 73  7 63 67]
               [99 65  4 28  6 16 70 92]
               [41 41 26 56 83 40 80 70 33]
               [41 48 72 33 47 32 37 16 94 29]
               [53 71 44 65 25 43 91 52 97 51 14]
               [70 11 33 28 77 73 17 78 39 68 17 57]
               [91 71 52 38 17 14 91 43 58 50 27 29 48]
               [63 66  4 68 89 53 67 30 73 16 69 87 40 31]
               [04 62 98 27 23  9 70 98 73 93 38 53 60  4 23]])

(defn merge-rows[a b]
  (map + (map #(apply max %) (partition 2 1 a)) b))

(reduce merge-rows (reverse triangle))

(def days-per-mo      [31 28 31 30 31 30 31 31 30 31 30 31])
(def days-per-mo-leap [31 29 31 30 31 30 31 31 30 31 30 31])

(defn leap-year? [y]
  (or (zero? (mod y 400))
      (and (zero? (mod y 4))
           (not (zero? (mod y 100))))))

(defn accum [s]
  "Returns a sequence of the sums of elements up to each element in seq s. "
  (loop [e (first s)
         r (rest s)
         sums [0]]
    (if (nil? e)
      (rest sums)
      (recur (first r) (rest r) (conj sums (+ e (last sums)))))))

;; Any number with a mod 7 must be a monday, if we start on a monday.
(defn euler-19 []
  (count (filter #(= 0 (mod % 7))
                 (accum (concat [366] ;; First skip the year 1900
                                (mapcat #(if (leap-year? %)
                                           days-per-mo-leap
                                           days-per-mo)
                                        (range 1901 2001)))))))

(defn factorial  [n] (reduce *' (range 1 (inc n))))
(defn sum-digits [s] (reduce + (map #(Integer/parseInt (str %)) s)))
(defn euler-20   []  (sum-digits (str (factorial 100))))

;(euler-20)

(defn list-divisors [n]
  (filter #(= (rem n %) 0) (range 1 n)))

(defn d [n] (reduce + (list-divisors n)))

(defn amicable? [n]
  (let [a (d n)
        b (d a)]
    (and (= b n) (not (= a b)))))

(defn euler-21 []
  (reduce + (filter amicable? (range 1 10000))))

;(euler-21)

;euler-22
(defn real-str [s]
  (first (re-seq #"\w+" s)))

(defn char-value [c]
    (inc (- (int c) (int \A))))

(defn alphabetical-value [s]
    (apply + (map char-value s)))

(def names-list
  (map real-str (sort (clojure.string/split (slurp "resources/names.txt") #","))))

(take 10 names-list)

(def list-score
  (map #(* (inc %) (alphabetical-value (nth names-list %))) (range 0 (count names-list))))

(defn euler-22 [] (apply + list-score))

;euler-23
(defn abundant? [n]
  (> (apply + (list-divisors n)) n))

(defn list-abundant-less-than [n]
  (filter abundant? (range 1 n)))

(defn max-abundant [] (list-abundant-less-than 28124))

(defn list-sum-of-abundant []
  (map #(apply + %) (cartesian-product (max-abundant) (max-abundant))))

(defn euler-23 []
  (apply + (clojure.set/difference (set (range 1 28124)) (set (list-sum-of-abundant)))))

;(euler-23)

;euler-24
(defn euler-24 []
  (nth (clojure.contrib.combinatorics/lex-permutations [0 1 2 3 4 5 6 7 8 9]), 999999))

;(euler-24)

;euler-25
(defn fib [a b]
  (cons a (lazy-seq (fib b (+' a b)))))

(defn digits [n]
  (count (str n)))

(defn euler-25 [n]
  (loop [b 0]
    (let [len (digits (nth (fib 1 1) b))]
      (if (= len n)
         (inc b)
         (recur (inc b))))))


;(euler-25 1000)

;euler-2 http://mathworld.wolfram.com/DecimalExpansion.html6
(defn pow [base expoente]
  (if (= 0 expoente)
    1
    (reduce *' (repeat expoente base))))

(defn mod-pow [base expoente modulo]
  (mod (pow base expoente) modulo))

(defn order [a n]
  (first (filter #(= 1N (mod-pow (bigint a) % (bigint n)))
         (map bigint (iterate inc 1)))))

(defn decimal-period [n]
  (cond (= 1 n) 0
    (zero? (rem n 2)) (decimal-period (/ n 2))
    (zero? (rem n 5)) (decimal-period (/ n 5))
    true (order 10 n)))

(defn euler-26 [n]
  (let [nums (range 1 n)
        periods (map decimal-period nums)]
    ((zipmap periods nums) (apply max periods))))

;euler-27
(defn gen-primes-until [n]
  (take-while #(<= % n) primes))

(defn prime? [n]
  (not-any? #(zero? (rem n %)) (take-while #(<= (* % %) n) primes)))

(defn count-primes-using [a b]
  (count (take-while #(prime? %) (filter #(> % 0) (map (fn [n] (+ (* n n) (* a n) b)) (iterate inc 0))))))


(defn odd-list [n]
  (filter odd? (range (* -1 n) n)))

(defn hash-list [n]
  (for [a (odd-list n)
        b (gen-primes-until n)] (hash-map :a a :b b :prod (* a b) :quant (count-primes-using a b))))

(defn euler-27 [n]
  (last (sort-by #(vec (map % [:quant :prod :a :b])) (hash-list n))))

;(euler-27 1000)

;euler-28
(defn odd-positive-list [n]
  (filter odd? (range 3 (inc n))))

(defn quadratic [n]
  (* n n))

(defn gen-diagonal-sum [n]
  (let [q (quadratic n)
        p (inc (- q n))
        r (inc (- p n))
        s (inc (- r n))]
    (+ q p r s)))

(defn euler-28 [n]
  (inc (apply + (map gen-diagonal-sum (odd-positive-list n)))))

;(euler-28 1001)

;euler-29

(defn euler-29 [m n]
  (count (into #{}
        (for [a (range 2 (inc m))
              b (range 2 (inc n))]
      (pow a b)))))

;(euler-29 100 100)

;euler-30
(defn calc-str-pow [word power]
  (loop [a word acum 0]
     (if (empty? a)
       acum
       (recur (rest a) (+' (pow (read-string (str (first a))) power) acum)))))

(defn euler-30 [power]
  (apply + (filter #(= % (calc-str-pow (str %) power)) (range 10 (pow 10 (+ power 1))))))

;(euler-30 5)

(def coins [200 100 50 20 10 5 2 1])

(defn coin-combos [sum goal maxcoin]
  (let [valid (filter #(and (>= maxcoin %)
                            (>= (- goal sum) %)) coins)]
    (if (empty? valid)
      (if (= sum goal) 1 0)
      (reduce + (map #(coin-combos (+ sum %) goal %) valid)))))

(defn euler-31 []
  (coin-combos 0 200 200))

;(euler-31)

(defn pandigital-multiplication? [a b]
  (= "123456789" (apply str (sort (rest (.split (str a b (* a b)) ""))))))

(defn euler-32[]
  (reduce + (distinct (for [a (range 2 5000)
                            b (range a (/ 9999 a))
                            :when (pandigital-multiplication? a b)]
                        (* a b)))))

(euler-32)
