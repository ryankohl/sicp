(ns sicp.core)

(defn e1-11-r [n]
  (cond (< n 3) n
        (>= n 3) (+ (e11-r (- n 1))
                    (* 2 (e11-r (- n 2)))
                    (* 3 (e11-r (- n 3))))))

(defn f [a b c] (+ a (* 2 b) (* 3 c)))
(defn e1-11-i [n]
  (loop [counter 0
         one 0
         two 0
         three 0]
    (cond (= n counter) one
          (< counter 2) (recur (inc counter) (inc one) one two)
          true (recur (inc counter) (f one two three) one two))))

(defn f [s i] (if (contains? (vec (range (count s)))  i) (nth s i) 0))
(defn e1-12 [n]
  (cond (= n 1) [1]
        true (map #(+ (f (e12 (- n 1)) (- % 1)) (f (e12 (- n 1)) %)) (range n))))

(defn e1-16 [b n]
  (loop [a 1
         y 0]
    (cond (= n y) a
          (= n (+ y 1)) (* a b)
          true (recur (* a b b) (+ y 2)))))

(defn f [x] (* 2 x))
(defn g [x] (/ x 2))
(defn e1-17 [x y]
  (cond (= y 0) 0
        (even? y) (f (e17 x (g y)))
        (odd? y) (+ x (e17 x (- y 1)))))

(defn f [x] (* 2 x))
(defn e1-18 [x y]
  (loop [a 0
         b 0]
    (cond (= y b) a
          (= y (+ b 1)) (+ a x)
          true (recur (+ a (f x)) (+ b 2)))))

(defn f [n t]
  (cond (> (* t t) n) n
        (= (rem n t) 0) t
        true (f n (inc t))))
(defn e1-21 [x] (f x 2))

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
