(ns sicp.chapter-1)

;; ex 1.11
(defn e1-11-r [n]
  (cond (< n 3) n
        (>= n 3) (+ (e1-11-r (- n 1))
                    (* 2 (e1-11-r (- n 2)))
                    (* 3 (e1-11-r (- n 3))))))

(defn f [a b c] (+ a (* 2 b) (* 3 c)))
(defn e1-11-i [n]
  (loop [counter 0
         one 0
         two 0
         three 0]
    (cond (= n counter) one
          (< counter 2) (recur (inc counter) (inc one) one two)
          true (recur (inc counter) (f one two three) one two))))

;; ex 1.12
(defn f [s i] (if (contains? (vec (range (count s)))  i) (nth s i) 0))
(defn e1-12 [n]
  (cond (= n 1) [1]
        true (map #(+ (f (e1-12 (- n 1)) (- % 1))
                      (f (e1-12 (- n 1)) %))
                  (range n))))

;; ex 1.16
(defn e1-16 [b n]
  (loop [a 1
         y 0]
    (cond (= n y) a
          (= n (+ y 1)) (* a b)
          true (recur (* a b b) (+ y 2)))))

;; ex 1.17
(defn f [x] (* 2 x))
(defn g [x] (/ x 2))
(defn e1-17 [x y]
  (cond (= y 0) 0
        (even? y) (f (e1-17 x (g y)))
        (odd? y) (+ x (e1-17 x (- y 1)))))

;; ex 1.18
(defn f [x] (* 2 x))
(defn e1-18 [x y]
  (loop [a 0
         b 0]
    (cond (= y b) a
          (= y (+ b 1)) (+ a x)
          true (recur (+ a (f x)) (+ b 2)))))

;; ex 1.21
(defn f [n t]
  (cond (> (* t t) n) n
        (= (rem n t) 0) t
        true (f n (inc t))))
(defn e1-21 [x] (f x 2))

;; ex 1.22
(defn g? [x] (= x (e1-21 x)))
(defn nxt [x]
  (loop [a (inc x)]
    (cond (g? a) a
          (even? a) (recur (inc a))
          (odd? a) (recur (+ 2 a)))))
(defn e1-22 [x]
  (loop [a []
         b (inc x)]
    (cond (= 3 (count a)) a
          true (recur (conj a (nxt b)) (inc (nxt b))))))

;; ex 1.23
(defn f [n t]
  (loop [a t]
    (cond (> (* a a) n) n
          (= (rem n a) 0) a
          (= 2 a) (recur 3)
          true (recur (+ a 2)))))
(defn g? [x] (= x (f x 2)))
(defn nxt [x]
  (loop [a (inc x)]
    (cond (g? a) a
          (even? a) (recur (inc a))
          (odd? a) (recur (+ 2 a)))))
(defn e1-23 [x]
  (loop [a []
         b (inc x)]
    (cond (= 3 (count a)) a
          true (recur (conj a (nxt b)) (inc (nxt b))))))

;; ex 1.29
(defn integral [f a b n]
  (let [h (/ (- b a) n)
        y #(f (+ a (* % h)))]
  (loop [k 0
         ans 0]
    (cond (= k 0) (recur (inc k) (+ ans (y k)))
          (= k n) (* (/ h 3) (+ ans (y k)))
          (odd? k) (recur (inc k) (+ ans (* 4 (y k))))
          (even? k) (recur (inc k) (+ ans (* 2 (y k))))))))
(def e1-29 (integral #(* % % %) 0 1 500))

;; ex 1.30
(defn sum [f a nxt b]
  (loop [x a
         r 0]
    (cond (= x b) (+ r (f x))
          true (recur (nxt x) (+ r (f x))))))

;; ex 1.31
(defn prod [f a nxt b]
  (loop [x a
         r 1]
    (cond (= x b) (* r (f x))
          true (recur (nxt x) (* r (f x))))))
(defn fac [x] (prod #(+ 0 %) x dec 1))
(defn mypi [n]
  (double (* 2 (/ (*
                   (* 2 n)
                   (prod #(* % %) (- (* 2 n) 2) #(- % 2) 2))
                  (prod #(* % %) (- (* 2 n) 1) #(- % 2) 3)))))

;; ex 1.32
(defn accumulate [combiner null-value f a nxt b]
  (loop [x a
         r null-value]
    (cond (= x b) (combiner r (f x))
          true (recur (nxt x) (combiner r (f x))))))

;; ex 1.33
(defn filter-accumulate [filt combiner null-value f a nxt b]
  (loop [x a
         r null-value]
    (cond (= x (nxt b)) r
          (filt x) (recur (nxt x) (combiner r (f x)))
          true (recur (nxt x) r))))
(defn sq [x] (* x x))
(defn lcd [n t]
  (loop [a t]
    (cond (> (* a a) n) n
          (= (rem n a) 0) a
          (= 2 a) (recur 3)
          true (recur (+ a 2)))))
(defn gcd [a b]
  (loop [x (min a b)]
    (cond (= 0 (mod b x) (mod a x)) x
          true (recur (dec x)))))
(defn prime? [x] (and (not= x 1) (= x (lcd x 2))))
(defn sumsqprime [x] (filter-accumulate prime? + 0 sq 1 inc x))
(defn relprime? [a b] (= (gcd a b) 1))
(defn b [x] (filter-accumulate #(relprime? % x) * 1 identity x dec 1))

;; ex 1.34
(defn fixed-point [f init tol]
  (loop [g init
         n (f g)]
    (cond (< (Math/abs (- g n)) tol) n
          true (recur n (f n)))))
(defn avg [L] (/ (reduce + L) (count L)))
(defn sqrt-fp [x] (fixed-point #(avg [% (/ x %)]) 1.0 0.0001))

;; ex 1.35
(defn golden-fp [x] (fixed-point #(+ 1 (/ 1 %) ) 1.0 0.0001))

;; ex 1.36
(defn fp [f init tol]
  (loop [g init
         n (f g)]
    (prn g)
    (cond (< (Math/abs (- g n)) tol) n
          true (recur n (f n)))))
(defn xx-fp [x] (fp #(/ (Math/log10 1000) (Math/log10 %)) 2 0.0001))

;; ex 1.37
(defn cont-frac [n d k]
  (loop [i k
         x (+ (d (dec i)) (/ (n i) (d i)))]
    (cond (= i 2) (/ (n i) x)
          true (recur (dec i) (+ (d (- i 2)) (/ (n (dec i)) x))))))
(double (letfn [(f [x] 1)] (cont-frac f f 10))) 

;; ex 1.38
(defn f [x]
  (loop [i 1
         ans 1]
    (cond (= x 1) 1
          (= x 2) 2
          (and (= x i) (< (mod i 3) 2)) 1
          (= x i) (* 2 ans)
          (= 2 (mod i 3)) (recur (inc i) (inc ans))
          true (recur (inc i) ans))))
(double (letfn [(n [x] 1)] (+ 2 (cont-frac n f 10))))

;; ex 1.39
(defn odds [x] (- (* 2 x) 1))
(defn sq-r [x r] (if (= x 1) r (* r r)))
(defn tan-cf [r k] (cont-frac #(sq-r % r) odds  k))

;; ex 1.40
(defn cubic [a b c] (fn [x] (+ (* x x x)
                              (* a x x)
                              (* b x)
                              c)))

;; ex 1.41
(defn dble [g] (fn [x] (g (g x))))

;; ex 1.42
(defn compose [g h] (fn [x] (g (h x))))

;; ex 1.43
(defn repeated [g n]
  (loop [i 0
         ans identity]
    (cond (= i n) ans
          true (recur (inc i) (compose g ans)))))

;; ex 1.44
(defn smooth [g dx] (fn [x] (avg [(g x)
                                (g (- x dx))
                                (g (+ x dx))])))
(defn n-smooth [n g dx] (repeated (smooth g dx) n))

;; ex 1.45
(defn fixed-point [f init tol]
  (loop [g init
         n (f g)]
    (cond (< (Math/abs (- g n)) tol) n
          true (recur n (f n)))))
(defn avg [L] (/ (reduce + L) (count L)))
(defn average-damp [g] (fn [x] (avg [x (g x)])))
(defn nth-root [x n] (fn [y] (/ x (Math/pow y (- n 1)))))
(defn nth-fp [x n i]
  (fixed-point (repeated (average-damp (nth-root x n)) i) 1.0 0.0001))

;; ex 1.46
(defn iterative-improve [checkFn? improveFn]
  (fn [x] (loop [ans x]
           (cond (checkFn? ans) ans
                 true (recur (improveFn ans))))))
(defn sqrt-ii [x]
  (iterative-improve #(< (Math/abs (double (- x (* % %)))) 0.001)
                     #(avg [% (/ x %)])))
(defn fp-ii [f]
  (iterative-improve #(< (Math/abs (double (- % (f %)))) 0.001)
                     f))


