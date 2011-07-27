(ns sicp.chapter-2
  (:require [clojure.contrib [math :as math]]))

(defn avg [x & args] (/ (reduce + (conj args x)) (count (conj args x))))
(defn sq [x] (* x x))

;; ex 2.1
(defn make-rat [n d]
  (let [g (math/gcd n d)]
    (cond (> 0 (* n d)) (list (* -1 (math/abs (/ n g))) (math/abs (/ d g)))
          true (list (math/abs (/ n g)) (math/abs (/ d g)))
          )))

;; ex 2.2
(defn make-point [x y] (list x y))
(defn x-point [p] (first p))
(defn y-point [p] (last p))
(defn print-point [p] (str "(" (x-point p) "," (y-point p) ")"))

(defn make-segment [b e] (list b e))
(defn start-segment [s] (first s))
(defn end-segment [s] (last s))

(defn midpoint-segment [s]
  (list (avg (x-point (start-segment s)) (x-point (end-segment s)))
        (avg (y-point (start-segment s)) (y-point (end-segment s)))))

;; ex 2.3
(defn get-length [s]
  (math/sqrt (+ 
              (sq (- (x-point (start-segment s)) (x-point (end-segment s))))
              (sq (- (y-point (start-segment s)) (y-point (end-segment s)))))))

(defn make-rect [p1 p2 p3] (list (make-segment p1 p2) (make-segment p2 p3)))
(defn make-rect-2 [s1 s2] (list s1 s2))

(defn get-perimeter [rect]
  (* 2 (+ (get-length (first rect)) (get-length (last rect)))))
(defn get-area [rect] (* (get-length (first rect)) (get-length (last rect))))

;; ex 2.4
(defn cons- [x y] (fn [m] (m x y)))
(defn car- [x] (x (fn [p q] p)))
(defn cdr- [x] (x (fn [p q] q)))

;; ex 2.5
(defn factor [n base]
  (loop [ans 0
         num n]
    (cond (= 0 (mod num base)) (recur (inc ans) (quot num base))
          true ans)))
(defn cons-- [x y] (* (math/expt 2 x) (math/expt 3 y)))
(defn car-- [x] (factor x 2))
(defn cdr-- [x] (factor x 3))

;; ex 2.6
(defn zero [succ] (fn [base] base))
(defn one [succ] (fn [base] (succ base)))
(defn two [succ] (fn [base] (succ (succ base))))
(defn add-1 [n] (fn [succ] (fn [base] (succ ((n succ) base)))))
(defn add [m n] (fn [succ] (fn [base] ((m succ) ((n succ) base)))))
;; (((add one two) inc) 0)

;; ex 2.7
(defn make-interval [a b] (list a b))
(defn lower-bound [x] (first x))
(defn upper-bound [x] (last x))

;; ex 2.8
(defn sub-interval [x y]
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

(defn add-interval [x y]
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(defn mul-interval [x y]
  (let [a (* (lower-bound x) (lower-bound y))
        b (* (lower-bound x) (upper-bound y))
        c (* (upper-bound x) (lower-bound y))
        d (* (upper-bound x) (upper-bound y))]
    (make-interval (min a b c d) (max a b c d))))
(defn div-interval [x y]
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

;; ex 2.9
(defn width [x] (/ (- (upper-bound x) (lower-bound x)) 2))
(defn width-add [x y] (+ (width x) (width y)))
(defn width-sub [x y] (- (width x) (width y)))
(defn width-mul [x y]
  (let [a (* (lower-bound x) (lower-bound y))
        b (* (lower-bound x) (upper-bound y))
        c (* (upper-bound x) (lower-bound y))
        d (* (upper-bound x) (upper-bound y))]
    (/ (- (max a b c d) (min a b c d)) 2)))
(def a (make-interval 5 10))
(def b (make-interval 8 12))
(= (width (add-interval a b)) (width-add a b))
(= (width (sub-interval b a)) (width-sub b a))
(= (width (mul-interval a b)) (width-mul a b))

;; ex 2.10
(defn div-interval [x y]
  (cond
   (>= 0 (* (upper-bound y) (lower-bound y))) (str "nonsense")
   true (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y))))))

;; ex 2.11
(defn mult-interval [x y]
  (let [xa (lower-bound x)
        xb (upper-bound x)
        ya (lower-bound y)
        yb (upper-bound y)
        aa (* xa ya)
        bb (* xb yb)]
    (cond (and (neg? xb) (pos? ya)) (make-interval aa bb)
          (and (neg? xb) (zero? ya)) (make-interval aa bb)
          (and (zero? xb) (pos? ya)) (make-interval aa bb)
          (and (zero? xb) (zero? ya)) (make-interval aa bb)
          (and (neg? yb) (pos? xa)) (make-interval aa bb)
          (and (neg? yb) (zero? xa)) (make-interval aa bb)
          (and (zero? yb) (pos? xa)) (make-interval aa bb)
          (and (zero? yb) (zero? xa)) (make-interval aa bb)
          true (mul-interval x y))))

;; ex 2.12
(defn make-center-percent [c p]
  (make-interval (- c (* c (/ p 100))) (+ c (* c (/ p 100)))))
(defn center [i] (/ (+ (lower-bound i) (upper-bound i)) 2))
(defn percent [i] (* 100 (/ (- (upper-bound i) (center i)) (center i))))

;; ex 2.13
(defn approx-mult [a b] (+ (percent a) (percent b)))

;; ex 2.14
(defn par1 [r1 r2]
  (div-interval (mult-interval r1 r2)
                (add-interval r1 r2)))
(defn par2 [r1 r2]
  (let [one (make-interval 1 1)]
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))
