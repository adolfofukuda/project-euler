(ns project.euler
  (:gen-class)
  (:use [clojure.contrib.lazy-seqs :only (primes)])
  (:use [clojure.contrib.math] ))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (println "Hello, World!"))


; Ex1
(reduce + (filter (fn [x] (or  (= (mod x 3) 0) (= (mod x 5) 0))) (range 1000)))


; Ex2
(defn fib [n]
  (loop [ x [1 2]]
    (if (< (count x) n)
      (recur (conj x (+ (last x) (nth x (- (count x) 2)))))
      x)))

(defn less-than-4mi? [x] (< x 4000000))


(defn ex2 [] (reduce + (filter less-than-4mi? (filter even? (fib 50)))))

;Ex3
(defn greatest-prime-of [number]
  (reduce max (filter #(zero? (mod number %))
                      (take-while #(< % (sqrt number)) primes))))

;Ex4
(defn palindrome? [word]
  (= word (apply str (vec (reverse word)))))

(defn largest-palindrome [x]
  (let [palindrome-number (fn [m n]
      (let [numero (* m n)]
      (cond (< n 900) (recur (dec m) 999)
         (palindrome? (str numero)) numero
         :else (recur m (dec n)))))]
  (palindrome-number x x)))

;Ex5
(defn lcm-of-a-list [last-element]
  (reduce lcm (range 2 (inc last-element))))

;(lcm-of-a-list 20)

;Ex6
(defn difference-from-squares [max-element]
  (- (expt (reduce + (range 1 (inc max-element))) 2) (reduce + (for [x (range 1 (inc max-element))] (expt x 2)))))

;(difference-from-squares 100)

;Ex7 - 10001st prime number
(defn ex7 [] (nth primes 10000))
(ex7)

;Ex8
(def arquivo "resources/ex8.txt")

(defn digit? [c] 1 (Character/isDigit c))

(def conteudo (filter digit? (slurp arquivo)))

(defn map-to-int [coll]
  (map #(Integer/parseInt (str %)) coll))

(defn ex8 []
  (let [mapa (map map-to-int (partition 5 1 conteudo))]
    (let [lista (for [n (range 0 (count mapa))]
      (apply * (nth mapa n)))]
      (first (sort > lista)))))

;Ex 9
(defn pythagorean-triple-for [m n]
  (let [mm (Math/pow m 2)
        nn (Math/pow n 2)]
        (sort [(int (- mm nn)) (* 2 m n) (int (+ mm nn))])))


(defn pythagorean-triples
  ([] (cons (pythagorean-triple-for 2 1) (pythagorean-triples 2 2)))
  ([m n]
      (if (< n m)
        (lazy-seq (cons (pythagorean-triple-for m n) (pythagorean-triples m (inc n))))
        (recur (inc m) 1))))

(defn sum [coll] (reduce + coll))

(defn problem9 []
   (reduce * (first (filter #(= (sum %) 1000) (pythagorean-triples)))))

;Ex 10
(defn prime-max [maximo]
  (take-while #(< % maximo) primes))

(reduce + (prime-max 2000000))