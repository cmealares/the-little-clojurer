(ns the-little-clojurer.core
  (:require [clojure.test :refer :all]
            [the-little-clojurer.core :refer :all]))

;; ---------------------------------------------------------------------------
;; Preface
;; ---------------------------------------------------------------------------

(defn atom? [x]
  (not (coll? x)))

;; ---------------------------------------------------------------------------
;; 1. Toys
;; ---------------------------------------------------------------------------

; car is first
; cdr is rest
; null? is empty?
; eq? is =

;; ---------------------------------------------------------------------------
;; 2. Do It, Do It Again, and Again, and Again...
;; ---------------------------------------------------------------------------

(defn lat?
  "Test if l is a list of atoms"
  [l]
  (cond
    (empty? l) true
    (atom? (first l)) (lat? (rest l))
    :else false))

(defn member?
  [a lat]
  (cond
    (empty? lat) false
    :else (or (= a (first lat))
              (member? a (rest lat)))))

(deftest chap2test
  (testing "lat?"
    (is (= true (lat? '(Jack Sprat could eat no chicken fat))))
    (is (= false (lat? '((Jack) Sprat could eat no chicken fat))))
    (is (= true (lat? ()))))

  (testing "member?"
    (is (= false (member? 'tea ())))
    (is (= true (member? 'tea '(coffee tea or milk))))
    (is (= false (member? 'poached '(fried eggs and scrambled eggs))))))

;; ---------------------------------------------------------------------------
;; 3. Cons the Magnificent
;; ---------------------------------------------------------------------------

(defn rember
  "Removes the first occurence of the atom a from lat"
  [a lat]
  (cond
    (empty? lat) ()
    (= a (first lat)) (rest lat)
    :else (cons (first lat) (rember a (rest lat)) )))

(defn firsts [l]
  (cond
    (empty? l) ()
    :else (cons (first (first l)) (firsts (rest l)))))

(defn insertR
  "Inserts the atom new to the right of the first occurrence of the atom old in lat"
  [new old lat]
  (cond
    (empty? lat) ()
    (= old (first lat)) (cons old (cons new (rest lat)))
    :else (cons (first lat) (insertR new old (rest lat)))))

(defn insertL
  "Inserts the atom new to the left of the first occurrence of the atom old in lat"
  [new old lat]
  (cond
    (empty? lat) ()
    (= old (first lat)) (cons new lat)
    :else (cons (first lat) (insertL new old (rest lat)))))

(defn subst
  "Replaces the first occurence of old in the lat with new"
  [new old lat]
  (cond
    (empty? lat) ()
    (= old (first lat)) (cons new (rest lat))
    :else (cons (first lat) (subst new old (rest lat)))))

(defn subst2
  "Replaces either the first occurence of o1 or the first occurrence of o2 by new"
  [new o1 o2 lat]
  (cond
    (empty? lat) ()
    (= o1 (first lat)) (cons new (rest lat))
    (= o2 (first lat)) (cons new (rest lat))
    :else (cons (first lat) (subst2 new o1 o2 (rest lat)))))

(defn multirember
  "Removes the all occurence of the atom a from lat"
  [a lat]
  (cond
    (empty? lat) ()
    (= a (first lat)) (multirember a (rest lat))
    :else (cons (first lat) (multirember a (rest lat)) )))

(defn multiinsertR
  "Inserts the atom new to the right of the occurrences of the atom old in lat"
  [new old lat]
  (cond
    (empty? lat) ()
    (= old (first lat)) (cons old
                              (cons new
                                    (multiinsertR new old (rest lat))))
    :else (cons (first lat)
                (multiinsertR new old (rest lat)))))

(defn multiinsertL
  "Inserts the atom new to the left of the occurrences of the atom old in lat"
  [new old lat]
  (cond
    (empty? lat) ()
    (= old (first lat)) (cons new
                              (cons old
                                    (multiinsertL new old (rest lat))))
    :else (cons (first lat)
                (multiinsertL new old (rest lat)))))

(defn multisubst
  "Replaces the all the occurences of old in the lat with new"
  [new old lat]
  (cond
    (empty? lat) ()
    (= old (first lat)) (cons new
                              (multisubst new old (rest lat)))
    :else (cons (first lat)
                (multisubst new old (rest lat)))))

(deftest chap3test
  (testing "rember"
    (are [x y] (= x y)
      (rember 'mint ()) ()
      (rember 'mint '(lamb chops and mint jelly)) '(lamb chops and jelly)
      (rember 'mint '(lamb chops and mint flavored mint jelly)) '(lamb chops and flavored mint jelly)
      (rember 'toast '(bacon lettuce and tomato)) '(bacon lettuce and tomato)
      ))

  (testing "firsts"
    (are [x y] (= x y)
      (firsts ()) ()
      (firsts '((apple peach pumpkin) (plum pear cherry) (grape raisin pea) (bean carrot eggplant))) '(apple plum grape bean)
      ))

  (testing "insertR"
    (are [x y] (= x y)
      (insertR 'topping 'fudge '(ice cream with fudge for dessert)) '(ice cream with fudge topping for dessert)
      (insertR 'e 'd '(a b c d f g h)) '(a b c d e f g h)))

  (testing "insertL"
    (is (= (insertL 'e 'f '(a b c d f g h)) '(a b c d e f g h))))

  (testing "subst"
    (is (= (subst 'topping 'fudge '(ice cream with fudge for dessert)) '(ice cream with topping for dessert))))

  (testing "subst2"
    (is (= (subst2 'vanilla 'chocolate 'banana '(banana ice cream with chocolate topping))
           '(vanilla ice cream with chocolate topping))))

  (testing "multirember"
    (is (= (multirember 'cup '(coffee cup tea cup and hick cup)) '(coffee tea and hick))))

  (testing "multiinsertR"
    (is (= (multiinsertR 'x 'a '(a b c a d)) '(a x b c a x d))))

  (testing "multiinsertL"
    (is (= (multiinsertL 'x 'a '(a b c a d)) '(x a b c x a d))))

   (testing "multisubst"
    (is (= (multisubst 'x 'a '(a b c a d)) '(x b c x d)))) )

;; ---------------------------------------------------------------------------
;; 4. Numbers Games
;; ---------------------------------------------------------------------------
