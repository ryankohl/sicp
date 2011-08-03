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

;; ex 2.17
(defn last-pair [L]
  (loop [l L]
    (cond (empty? (rest l)) l
          true (recur (rest l)))))

;; ex 2.18
(defn reverse [L]
  (loop [orig L
         ans '()]
    (cond (empty? orig) ans
          true (recur (rest orig) (cons (first orig) ans)))))

;; ex 2.19 
(defn first-denom [L] (first L))
(defn except-first-denom [L] (rest L))
(defn no-more? [L] (empty? L))
(defn cc [amount coin-values]
  (cond (= amount 0) 1
        (or (< amount 0) (no-more? coin-values)) 0
        true (+ (cc amount (except-first-denom coin-values))
                (cc (- amount (first-denom coin-values)) coin-values))))
(def us-coins (list 50 25 10 5 1))
(def su-coins (list 1 5 10 25 50))

;; ex 2.20
(defn same-parity [a & bs]
  (cond (even? a) (cons a (filter even? bs))
        (odd? a) (cons a (filter odd? bs))))

;; ex 2.21
(defn square-list [L]
  (if (empty? L)
    '()
    (cons (* (first L) (first L)) (square-list (rest L)))))
(defn square-list [L] (map #(* % %) L))

;; ex 2.23
(defn for-each [f L]
  (loop [l L]
    (f (first l))
    (cond (empty? (rest l)) true
          true (recur (rest l)))))

;; ex 2.27
(defn deep-reverse [L]
  (loop [orig L
         ans '()]
    (cond (empty? orig) ans
          (coll? (first orig)) (recur (rest orig)
                                      (cons (deep-reverse (first orig)) ans))
          true (recur (rest orig)
                      (cons (first orig) ans)))))

(defn append [x y]
  (if (empty? x)
    y
    (cons (first x) (append (rest x) y))))

;; ex 2.28
(defn fringe [L]
  (loop [l L
         ans []]
    (cond (empty? l) (reverse ans)
          (coll? (first l)) (recur (rest l) (append
                                             (reverse (fringe (first l))) ans))
          true (recur (rest l) (cons (first l) ans)))))

;; ex 2.29
(defn make-mobile [left right] (list left right))
(defn make-branch [length structure] (list length structure))
(defn left-branch [mobile] (first mobile))
(defn right-branch [mobile] (last mobile))
(defn branch-length [branch] (first branch))
(defn branch-structure [branch] (last branch))
(defn branch-weight [b]
  (let [x (branch-structure b)]
  (loop [ans 0]
    (cond (number? x) x
          (coll? x) (+ (branch-weight (left-branch x))
                       (branch-weight (right-branch x)))))))
(defn total-weight [m]
  (+ (branch-weight (left-branch m))
     (branch-weight (right-branch m))))
(defn torque [b] (* (branch-length b) (branch-weight b)))
(defn balanced? [m]
  (= (torque (left-branch m))
     (torque (right-branch m))))
(def mob (make-mobile
          (make-branch 3 10)
          (make-branch 6
                       (make-mobile
                        (make-branch 2 5)
                        (make-branch 8 (make-mobile
                                        (make-branch 1 7)
                                        (make-branch 2 4)))))))
(def mobb (make-mobile
           (make-branch 1 (make-mobile
                            (make-branch 5 7)
                            (make-branch 5 7)))
           (make-branch 1 (make-mobile
                            (make-branch 5 7)
                            (make-branch 5 7)))))

;; ex 2.30
(defn square-tree [x]
  (map #(if (coll? %) (square-tree %) (* % %)) x))
(def x [1 [2 [3 4] 5] [6 7]])

;; ex 2.31
(defn tree-map [f t]
  (map #(if (coll? %) (tree-map f %) (f %)) t))

;; ex 2.32
(defn subsets [s]
  (if (empty? s)
    (list nil)
    (let [r (subsets (rest s))]
      (append r (map #(cons (first s) %) r)))))

;; ex 2.33
(defn map- [p s] (reduce (fn [x y] (conj x (p y))) [] s))
(defn append- [a b] (reduce (fn [x y] (conj x y)) a b)) ; for vectors
(defn length- [s] (reduce (fn [x y] (inc x)) 0 s))

;; ex 2.34
(defn horner-eval [x coeff]
  (/ (reduce (fn [a b] (* (+ a b) x)) 
          0
          (reverse coeff))
     x))
; need to divide by x to correct for the fact that a0 shouldn't be
; multiplied by x

;; ex 2.35
(defn count-leaves [t]
  (reduce + (map count (map fringe t))))

;; ex 2.36
(defn reduce-n [op init seqs]
  (if (empty? (first seqs))
              nil
              (cons (reduce op init (map first seqs))
                    (reduce-n op init (map rest seqs)))))
(def s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

;; ex 2.37
(def m [[1 2 3 4] [4 5 6 6] [6 7 8 9] [5 3 9 1]])
(def v [1 2 3 4])
(defn dot-product [v w] (reduce + 0 (map * v w)))
(defn matrix-*-vector [m v] (map (fn [x] (dot-product x v)) m))
(defn transpose [m] (reduce-n conj [] m))
(defn matrix-*-matrix [m n]
  (let [cols (transpose n)]
    (map (fn [x] (matrix-*-vector cols x)) m)))

;; ex 2.39
(defn reverse-right [s] (reduce (fn [a b] (cons b a)) [] s))
(defn reverse-left [s] (reduce (fn [a b] (conj a b)) [] (reverse s)))

;; ex 2.40
(defn flatmap [f s] (reduce append (list) (map f s)))
(defn unique-pairs [n]
  (flatmap (fn [x] (map #(list % (+ x 1))
                   (range 1 (+ x 1))))
       (range 1 n)))
(defn lcd [n t]
  (loop [a t]
    (cond (> (* a a) n) n
          (= (rem n a) 0) a
          (= 2 a) (recur 3)
          true (recur (+ a 2)))))
(defn prime? [x] (and (not= x 1) (= x (lcd x 2))))
(defn prime-sum? [p]
  (prime? (+ (first p) (last p))))
(defn make-pair-sum [p]
  (list (first p) (last p) (+ (first p) (last p))))
(defn prime-sum-pairs [n]
  (map make-pair-sum (filter prime-sum? (unique-pairs n))))

;; ex 2.41
(defn unique-triples [n]
  (flatmap (fn [p]
             (map #(cons % p) (range 1 (first p))))
           (unique-pairs n)))
(defn sum-to-x? [x] (fn [[a b c]] (= x (+ a b c))))
(defn make-triple-sum [[a b c]] (list a b c (+ a b c)))
(defn x-sum-triples [n x]
  (map make-triple-sum (filter (sum-to-x? x) (unique-triples n))))

;; ex 2.42
(defn adjoin-position [new-row k rest-of-queens]
  (let [head (filter #(< 0 %) rest-of-queens)
        tail (rest (filter zero? rest-of-queens))]
    (append (conj head new-row) tail)))
;; seq of seqs (row index starts at 1, 0 is placeholder): [1 2 2 4 0 0 0 0]
;; row safe: is any number repeated? (row is the value)
;; col safe: auto
;; dgn safe: are the row/col values unique?
(defn empty-board [board-size]
  (loop [s board-size
         ans []]
    (cond (= s 0) (list ans)
          true (recur (dec s) (conj ans 0)))))
(defn transp [x]
  (let [a (zipmap x (range 1 (inc (count x))))
        b (into {} (for [[k v] a] [v k]))]
    (loop [ans []
           i 0]
      (cond (= i (count a)) ans
            true (recur (cons (b (inc i)) ans) (inc i))))))
(defn safe? [pos]
  (let [z    (fn [s] (filter #(< 0 %) s))
        spos (set (z pos))
        diag (fn [s] (map (fn [x y] (/ x y))
                         (filter #(< 0 %) s)
                         (range 1 (inc (count (filter #(< 0 %) s))))))]
    (cond
     (not= (count (z pos)) (count spos)) false
     (not= (count (diag (z pos))) (count (set (diag (z pos))))) false
     (not= (count (diag (transp (z pos)))) (count (set (diag (transp (z pos)))))) false
     true true)))
     
(defn queen [board-size]
  (letfn [(queen-cols [k]
            (if (= k 0)
              (empty-board board-size)
              (filter (fn [pos] (safe? pos))
                      (flatmap (fn [rest-of-queens]
                                 (map (fn [new-row] (adjoin-position new-row
                                                                    board-size
                                                                    rest-of-queens))
                                      (range 1 (inc board-size))))
                               (queen-cols (dec k))))))]
    (queen-cols board-size))) 
