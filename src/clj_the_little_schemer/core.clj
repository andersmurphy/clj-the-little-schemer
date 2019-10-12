(ns clj-the-little-schemer.core)

(defn atom? [l]
  (not (coll? l)))

(comment
  (= true
     (atom? "foo"))

  (= false
     (atom? '())))

(defn lat? [l]
  (cond (empty? l)        true
        (atom? (first l)) (recur (rest l))
        :else             false))

(comment
  ;; Non recursive version
  (defn lat? [l]
    (every? atom? l)))

(comment
  (= true
     (lat? '(jack sprat could eat no chicken fat)))

  (= false
     (lat? '(jack (sprat) could eat no chicken fat))))

(defn member? [a lat]
  (when-let [[x & xs] (seq lat)]
    (cond (= x a) true
          :else   (recur a xs))))

(comment
  ;; Non recursive version
  (defn member? [a lat]
    (boolean (some #{a} lat))))

(comment
  (= true
     (member? 'fat '(jack sprat could eat no chicken fat)))

  (nil? (member? 'fat '(jack could eat no chicken))))

(defn rember [a lat]
  (lazy-seq
   (when-let [[x & xs] (seq lat)]
     (cond (= a x) xs
           :else   (cons x (rember a xs))))))

(comment
  ;; Non recursive version
  (defn rember [a lat]
    (let [[x y] (split-with #(not= a %) lat)]
      (concat x (rest y)))))

(comment
  (= '(b c d f g a)
     (rember 'a '(b c d a f g a))))

(defn firsts [l]
  (lazy-seq
   (when-let [[x & xs] (seq l)]
     (cons (first x)
           (firsts xs)))))

(comment
  ;; Non recursive version
  (defn firsts [l]
    (map first l)))

(comment
  (= '(a c e)
     (firsts '((a b) (c d) (e f)))))

(defn insertR [new old lat]
  (lazy-seq
   (when-let [[x & xs] (seq lat)]
     (cond (= x old) (cons old (cons new xs))
           :else     (cons x (insertR new old xs))))))

(comment
  ;; Non recursive version
  (defn insertR [new old lat]
    (let [[x y] (split-with #(not= old %) lat)]
      (concat x [(first y) new] (rest y)))))

(comment
  (= '(ice cream with fudge topping for desert)
     (insertR 'topping 'fudge '(ice cream with fudge for desert))))

(defn insertL [new old lat]
  (lazy-seq
   (when-let [[x & xs] (seq lat)]
     (cond (= x old) (cons new lat)
           :else     (cons x (insertL new old xs))))))

(comment
  ;; Non recursive version
  (defn insertL [new old lat]
    (let [[x y] (split-with #(not= old %) lat)]
      (concat x [new] y))))

(comment
  (= '(ice cream with topping fudge for desert)
     (insertL 'topping 'fudge '(ice cream with fudge for desert))))

(defn subst [new old lat]
  (lazy-seq
   (when-let [[x & xs] (seq lat)]
     (cond (= x old) (cons new xs)
           :else     (cons x (subst new old xs))))))

(comment
  ;; Non recursive version
  (defn subst [new old lat]
    (let [[x y] (split-with #(not= old %) lat)]
      (concat x [new] (rest y)))))

(comment
  (= '(ice cream with topping for desert)
     (subst 'topping 'fudge '(ice cream with fudge for desert))))

(defn subst2 [new o1 o2 lat]
  (lazy-seq
   (when-let [[x & xs] (seq lat)]
     (cond (#{o1 o2} x) (cons new xs)
           :else        (cons x (subst2 new o1 o2 xs))))))

(comment
  ;; Non recursive version
  (defn subst2 [new o1 o2 lat]
    (let [[x y] (split-with #(not (#{o1 o2} %)) lat)]
      (concat x [new] (rest y)))))

(comment
  (= '(ice cream with banana for desert)
     (subst2 'banana 'fudge 'desert '(ice cream with fudge for desert)))

  (= '(ice banana with fudge for desert)
     (subst2 'banana 'fudge 'cream '(ice cream with fudge for desert))))

(defn multirember [a lat]
  (lazy-seq
   (when-let [[x & xs] (seq lat)]
     (cond (= a x) (multirember a xs)
           :else   (cons x (multirember a xs))))))

(comment
  ;; Non recursive version
  (defn multirember [a lat]
    (remove #{a} lat)))

(comment
  (= '(ice cream with fudge for desert)
     (multirember 'banana '(banana ice cream with banana fudge for desert))))

(defn multiinsertR [new old lat]
  (lazy-seq
   (when-let [[x & xs] (seq lat)]
     (cond (= x old) (cons old (cons new (multiinsertR new old xs)))
           :else     (cons x (multiinsertR new old xs))))))

(comment
  ;; Non recursive version
  (defn multiinsertR [new old lat]
    (mapcat #(if (= % old) [% new] [%]) lat)))

(comment
  (= '(vanilla ice cream with banana ice cream for desert)
     (multiinsertR 'cream 'ice '(vanilla ice with banana ice for desert))))

(defn multiinsertL [new old lat]
  (lazy-seq
   (when-let [[x & xs] (seq lat)]
     (cond (= x old) (cons new (cons old (multiinsertL new old xs)))
           :else     (cons x (multiinsertL new old xs))))))

(comment
  ;; Non recursive version
  (defn multiinsertL [new old lat]
    (mapcat #(if (= % old) [new %] [%]) lat)))

(comment
  (= '(vanilla ice cream with banana ice cream for desert)
     (multiinsertL 'ice 'cream  '(vanilla cream with banana cream for desert))))

(defn multisubst [new old lat]
  (lazy-seq
   (when-let [[x & xs] (seq lat)]
     (cond (= x old) (cons new (multisubst new old xs))
           :else     (cons x (multisubst new old xs))))))

(comment
  ;; Non recursive version
  (defn multisubst [new old lat]
    (map #(if (= % old) new %) lat)))

(comment
  (= '(ice cream with topping for desert)
     (multisubst 'topping 'fudge '(ice cream with fudge for desert))))

(defn o+ [n m]
  (if (zero? m)
    n
    (recur (inc n) (dec m))))

(comment
  ;; Non recursive version
  (defn o+ [n m]
    (+ n m)))

(comment
  (= 10000001
     (o+ 1 10000000)))

(defn o- [n m]
  (if (zero? m)
    n
    (recur (dec n) (dec m))))

(comment
  ;; Non recursive version
  (defn o- [n m]
    (- n m)))

(comment
  (= -9999999
     (o- 1 10000000)))

(defn tup? [t]
  (or (empty? t) (every? number? t)))

(comment
  (= false
     (tup? [1 2 3 [2 3]]))
  (= true
     (tup? [1 2 3 ])))

(defn addtup [tup]
  (loop [acc      0
         [x & xs] (seq tup)]
    (if-not x
      acc
      (recur (+ acc x) xs))))

(comment
  ;; Non recursive versions
  (defn addtup [tup]
    (apply + tup))
  (defn addtup [tup]
    (reduce + tup)))

(comment
  (= 18
     (addtup [3 5 2 8])))

(defn o* [n m]
  (loop [acc 0
         m   m]
    (if (zero? m)
      acc
      (recur (+ acc n) (dec m)))))

(comment
  ;; Non recursive versions
  (defn o* [n m]
    (* n m)))

(comment
  (= 25 (o* 5 5))
  (= 5 (o* 5 1))
  (= 0 (o* 5 0))
  (= 0 (o* 0 4)))

(defn tup+ [tup1 tup2]
  (lazy-seq
   (let [[x & xs] (seq tup1)
         [y & ys] (seq tup2)]
     (cond (empty? tup1) tup2
           (empty? tup2) tup1
           :else         (cons (+ x y) (tup+ xs ys))))))

(comment
  ;; Non recursive versions
  (defn tup+ [tup1 tup2]
    (let [[shortest longest] (sort-by count [tup1 tup2])]
      (map + longest (concat shortest (repeat 0)))))

  ;; Var args
  (defn tup+ [& tups]
    (let [[longest & rest] (sort-by count > tups)]
      (apply map + (conj (map #(concat % (repeat 0)) rest) longest)))))

(comment
  (= [2 4 6 1] (tup+ [1 2 3] [1 2 3 1]))
  (= [2 4 6 1] (tup+ [1 2 3 1]  [1 2 3])))

(defn o> [n m]
  (cond (zero? n) false
        (zero? m) true
        :else     (recur (dec n) (dec m))))

(comment
  ;; Non recursive versions
  (defn o> [n m]
    (> n m)))

(comment
  (o> 4 3)
  (o> 1333 8000)
  (o> 3 3))

(defn o< [n m]
  (cond (zero? m) false
        (zero? n) true
        :else     (recur (dec n) (dec m))))

(comment
  ;; Non recursive versions
  (defn o< [n m]
    (< n m)))

(comment
  (o< 3 4)
  (o< 8000 1333)
  (o< 3 3))

(defn o= [n m]
  (cond (zero? m) (zero? n)
        (zero? n) false
        :else     (recur (dec n) (dec m))))

(comment
  ;; Non recursive versions
  (defn o= [n m]
    (= n m)))

(comment
  (o= 3 4)
  (o= 8000 1333)
  (o= 3 3))

(defn expt [n m]
  (loop [acc 1
         m   m]
    (if (zero? m)
      acc
      (recur (* acc n) (dec m)))))

(comment
  ;; Non recursive versions
  (defn expt [n m]
    (reduce * (repeat m n))))

(comment
  (= 25 (expt 5 2))
  (= 32 (expt 2 5)))

(defn divide [n m]
  (loop [acc 0
         n   n]
    (if (< n m)
      acc
      (recur (inc acc) (- n m)))))

(comment
  ;; Non recursive versions
  (defn divide [n m]
    (/ n m)))

(comment
  (= 5 (divide 10 2))
  (= 5 (divide 30 6)))

(defn length [lat]
  (loop [acc 0
         lat lat]
    (if (empty? lat)
      acc
      (recur (inc acc) (pop lat)))))

(comment
  ;; Non recursive versions
  (defn length [lat]
    (count lat)))

(comment
  (= 3 (length '(hotdogs with mustard))))

(defn pick [n [x & xs]]
  (if (zero? (dec n))
    x
    (recur (dec n) xs)))

(comment
  ;; Non recursive versions
  (defn pick [n lat]
    (nth lat (dec n))))

(comment
  (= 'hot (pick 3 '(hotdogs with hot mustard))))

(defn rempick [n lat]
  (lazy-seq
   (when-let [[x & xs] (seq lat)]
     (cond (zero? (dec n)) xs
           :else           (cons x (rempick (dec n) xs))))))

(comment
  ;; Non recursive versions
  (defn rempick [n lat]
    (let [v (vec lat)]
      (concat (subvec v 0 (dec n))
              (subvec v n)))))

(comment
  (= '(hotdogs with mustard) (rempick 3 '(hotdogs with hot mustard))))

(defn no-nums [lat]
  (lazy-seq
   (when-let [[x & xs] (seq lat)]
     (cond (number? x) (no-nums xs)
           :else       (cons x (no-nums xs))))))

(comment
  ;; Non recursive versions
  (defn no-nums [lat]
    (remove number? lat)))

(comment
  (= '(hot mustard) (no-nums '(5 hot 3 mustard))))

(defn all-nums [lat]
  (lazy-seq
   (when-let [[x & xs] (seq lat)]
     (cond (number? x) (cons x (all-nums xs))
           :else       (all-nums xs)))))

(comment
  ;; Non recursive versions
  (defn all-nums [lat]
    (filter number? lat)))

(comment
  (= '(5 3) (all-nums '(5 hot 3 mustard))))

(defn occur [a lat]
  (loop [acc      0
         [x & xs] (seq lat)]
    (cond (nil? x) acc
          (= x a)  (recur (inc acc) xs)
          :else    (recur acc xs))))

(comment
  ;; Non recursive versions
  (defn occur [a lat]
    ((frequencies lat) a))

  (defn occur [a lat]
    (count (filter #{a} lat))))

(comment
  (= 4 (occur 'a '(a b c a b a a))))

(defn one? [n]
  (= n 1))

(comment
  (true? (one? 1))
  (false? (one? 2)))

(defn rember* [a l]
  (lazy-seq
   (when-let [[x & xs] (seq l)]
     (cond (= a x)         (rember* a xs)
           (sequential? x) (cons (rember* a x) (rember* a xs))
           :else           (cons x (rember* a xs))))))

(comment
  ;; Non recursive versions
  (defn rember* [a l]
    (clojure.walk/prewalk
     (fn [x]
       (if (sequential? x)
         (remove #{a} x)
         x))
     l)))

(comment
  (= '(((a))
       ((c))
       (d ((e))))
     (rember* 'b '(((a b))
                   ((c) b)
                   (d ((e)) b)))))

(defn insertR* [new old l]
  (lazy-seq
   (when-let [[x & xs] (seq l)]
     (cond (= old x)       (cons old (cons new (insertR* new old xs)))
           (sequential? x) (cons (insertR* new old x) (insertR* new old xs))
           :else           (cons x (insertR* new old xs))))))

(comment
  ;; Non recursive versions
  (defn insertR* [new old l]
    (clojure.walk/prewalk
     (fn [x]
       (if (sequential? x)
         (mapcat #(if (= % old) [% new] [%]) x)
         x))
     l)))

(comment
  (= '(((b z a b z))
       ((c) b z)
       (d ((e)) b z))
     (insertR* 'z 'b '(((b a b))
                       ((c) b)
                       (d ((e)) b)))))

(defn occur* [a l]
  (letfn [(occur* [a l]
            (lazy-seq
             (when-let [[x & xs] (seq l)]
               (cond (= a x)         (cons 1 (occur* a xs))
                     (sequential? x) (concat (occur* a x) (occur* a xs))
                     :else           (occur* a xs)))))]
    (count (occur* a l))))

(comment
  ;; Non recursive versions
  (defn occur* [a l]
    (->> (flatten l)
         frequencies
         a))

  (defn occur* [a l]
    (->> (flatten l)
         (filter #{a})
         count)))

(comment
  (= 6 (occur* 'a '(a (b (a)) (((c) a)) (a b a) a))))

(defn subst* [new old l]
  (lazy-seq
   (when-let [[x & xs] (seq l)]
     (cond (= old x)       (cons new (subst* new old xs))
           (sequential? x) (cons (subst* new old x) (subst* new old xs))
           :else           (cons x (subst* new old xs))))))

(comment
  ;; Non recursive versions
  (defn subst* [new old l]
    (clojure.walk/prewalk
     (fn [x]
       (if (sequential? x)
         (map #(if (= old %) new %) x)
         x))
     l)))

(comment
  (= '(((a z))
       ((c) z)
       (d ((e)) z))
     (subst* 'z 'b '(((a b))
                     ((c) b)
                     (d ((e)) b)))))

(defn insertL* [new old l]
  (lazy-seq
   (when-let [[x & xs] (seq l)]
     (cond (= old x)       (cons new (cons old (insertL* new old xs)))
           (sequential? x) (cons (insertL* new old x) (insertL* new old xs))
           :else           (cons x (insertL* new old xs))))))

(comment
  ;; Non recursive versions
  (defn insertL* [new old l]
    (clojure.walk/prewalk
     (fn [x]
       (if (sequential? x)
         (mapcat #(if (= % old) [new %] [%]) x)
         x))
     l)))

(comment
  (= '(((z b a z b))
       ((c) z b)
       (d ((e)) z b))
     (insertL* 'z 'b '(((b a b))
                       ((c) b)
                       (d ((e)) b)))))

(defn member* [a l]
  (letfn [(member* [a l]
            (lazy-seq
             (when-let [[x & xs] (seq l)]
               (cond
                 (sequential? x) (concat (member* a x) (member* a xs))
                 (= a x)         (list true)
                 :else           (member* a xs)))))]
    (first (member* a l))))

(comment
  ;; Non recursive versions
  (defn member* [a l]
    (->> (flatten l)
         (some #{a})
         boolean)))

(comment
  (= true
     (member* 'c '(((b a b))
                   ((c) b)
                   (d ((e)) b)))))

(defn leftmost [[x]]
  (cond (not (sequential? x)) x
        :else                 (recur x)))

(comment
  ;; Non recursive versions
  (defn leftmost [l]
    (->> (flatten l)
         first)))

(comment
  (= 'b
     (leftmost '(((b a b))
                 ((c) b)
                 (d ((e)) b)))))

(defn numbered? [aexp]
  (letfn [(numbered? [aexp]
            (lazy-seq
             (cond
               (and (not (coll? aexp))) (list (number? aexp))
               :else                    (concat
                                         (numbered? (first aexp))
                                         (numbered?
                                          (first (rest (rest aexp))))))))]
    ;; Conceptual equivalent of apply and
    (every? true? (numbered? aexp))))

(comment
  ;; Non recursive versions
  (defn numbered? [aexp]
    (->> (flatten [aexp])
         (partition-all 2)
         (map first)
         (every? number?))))

(comment
  (= false (numbered? '(3 + (4 x 'bar))))
  (= true (numbered? '(3 + (4 x 5))))
  (= true (numbered? 3))
  (= false (numbered? 'bar)))
