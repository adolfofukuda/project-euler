
(defn fib [n]
  (loop [ant 0N atual 1N contador 1]
    (if (< n contador)
      atual
      (let [m (+ ant atual)]
        (recur atual m (inc contador))))))

(defn fibonacci [] (map fib (iterate inc 1)))

(nth (fibonacci) 89)

(defn fat [n]
  (loop [m n f 1]
    (if (zero? m)
      f
      (recur (dec m) (* f m)))))

(fat 3)

(nth (primes) 234)

