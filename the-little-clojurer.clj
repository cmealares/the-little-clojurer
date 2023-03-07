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

(defn add1 [n]
  (+ n 1))

(defn sub1 [n]
  (- n 1))

(defn o+ [n m]
  (cond
    (zero? m) n
    :else (add1 (o+ n (sub1 m)))))

(defn o- [n m]
  (cond
    (zero? m) n
    :else (sub1 (o- n (sub1 m)))))

(defn tup? [l]
  (cond
    (empty? l) true
    (number? (first l)) (tup? (rest l))
    :else false))

(defn addtup [tup]
  (cond
    (empty? tup) 0
    :else (o+ (first tup) (addtup (rest tup)))))

(defn o* [n m]
  (cond
    (zero? m) 0
    :else (o+ n (o* n (sub1 m)))))


(defn tup1+ [tup1 tup2]
  (cond
    (and (empty? tup1) (empty? tup2)) '()
    :else (cons (o+ (first tup1) (first tup2))
                (tup1+ (rest tup1) (rest tup2))) ))

(defn tup+ [tup1 tup2]
  (cond
    (empty? tup1) tup2
    (empty? tup2) tup1
    :else (cons (o+ (first tup1) (first tup2))
                (tup+ (rest tup1) (rest tup2))) ))

(defn o> [n m]
  (cond
    (zero? n) false
    (zero? m) true
    :else (o> (sub1 n) (sub1 m))))

(defn o< [n m]
  (cond
    (zero? m) false
    (zero? n) true
    :else (o< (sub1 n) (sub1 m))))

(defn o= [n m]
  (cond
    (o> n m) false
    (o< n m) false
    :else true))

(defn exp [n m]
  (cond
    (zero? m) 1
    :else (o* n (exp n (sub1 m)))
    ))

(defn quotient [n m]
  (cond
    (o< n m) 0
    :else (add1 (quotient (o- n m) m))))

(defn length
  "Returns the length of a list"
  [lat]
  (cond
    (empty? lat) 0
    :else (add1 (length (rest lat)))))

(defn pick
  "Returns the n-th element of a list. Indexes start at 1"
  [n lat]
  (cond
    (zero? (sub1 n)) (first lat)
    :else (pick (sub1 n) (rest lat))))

(defn rempick
  "Removes the n-th element from the lat"
  [n lat]
  (cond
    (zero? (sub1 n)) (rest lat)
    :else (cons (first lat)
                (rempick (sub1 n) (rest lat)))))

(defn no-nums
  "Removes the numbers from lat"
  [lat]
  (cond
    (empty? lat) '()
    (number? (first lat)) (no-nums (rest lat))
    :else (cons (first lat)
                (no-nums (rest lat)))))

(defn all-nums
  "Extract a tup from a lat using all the numbers"
  [lat]
  (cond
    (empty? lat) '()
    (number? (first lat)) (cons (first lat) (all-nums (rest lat)))
    :else (no-nums (rest lat))))

(def equan =)

(defn occur
  "Counts the number of times an atom a appears in a lat"
  [a lat]
  (cond
    (empty? lat) 0
    (= a (first lat)) (add1 (occur a (rest lat)))
    :else (occur a (rest lat))))

(defn one? [n]
  (= n 1))

(deftest chap4test
  (testing "o+"
    (is (= 5 (o+ 2 3))))

  (testing "o-"
    (is (= 11 (o- 14 3))))

  (testing "tup?"
    (is (= true (tup? '(2 11 3))))
    (is (= false (tup? '(1 2 8 apple 4 3))))
    (is (= false (tup? '(3 (7 4) 13 9))))
    (is (= true (tup?  '()))))

  (testing "addtup"
    (is (= 18 (addtup '(3 5 2 8)))))

  (testing "o*"
    (is (= 15 (o* 5 3))))

  (testing "tup+"
    (is (= '(6 9) (tup+ '(2 3) '(4 6))))
    (is (= '(7 13 8 1) (tup+ '(3 7 8 1) '(4 6)))))

  (testing "o>"
    (is (= false (o> 12 133)))
    (is (= true (o> 120 11)))
    (is (= false (o> 3 3))))

  (testing "o<"
    (is (= true (o< 4 6)))
    (is (= false (o< 8 3)))
    (is (= false (o< 3 3))))

  (testing "o="
    (is (= true (o= 4 4)))
    (is (= false (o= 8 3)))
    (is (= false (o= 3 8))))

  (testing "exp"
    (is (= 1 (exp 1 1)))
    (is (= 8 (exp 2 3)))
    (is (= 125 (exp 5 3))))

  (testing "quotient"
    (is (= 3 (quotient 15 4))))

  (testing "length"
    (is (= 6 (length '(hotdogs with mustard sauerkraut and pickles)))))

  (testing "pick"
    (is (= 'lasagna (pick 1 '(lasagna spaghetti ravioli macaroni meatball))))
    (is (= 'macaroni (pick 4 '(lasagna spaghetti ravioli macaroni meatball)))))

  (testing "rempick"
    (is (= '(hotdogs with mustard) (rempick 3 '(hotdogs with hot mustard)))) )

  (testing "no-nums"
    (is (= '(pears prunes dates) (no-nums '(5 pears 6 prunes 9 dates)))))

  (testing "occur"
    (is (= 0 (occur 'a '(b c d))))
    (is (= 2 (occur 'a '(b a c d a)))))

  (testing "one?"
    (is (= false (one? 0)))
    (is (= true (one? 1)))
    (is (= false (one? 5)))) )

;; ---------------------------------------------------------------------------
;; 5. Oh My Gawd: It's Full of Stars
;; ---------------------------------------------------------------------------

(defn rember* [a l]
  (cond
    (empty? l) '()
    (atom? (first l)) (cond
                        (= a (first l)) (rember* a (rest l))
                        :else (cons (first l)
                                    (rember* a (rest l))))
    :else (cons (rember* a (first l))
                (rember* a (rest l))) ))

(defn insertR*
  "Inserts the atom new to the right of old everywhere"
  [new old l]
  (cond
    (empty? l) '()
    (atom? (first l)) (cond
                        (= old (first l)) (cons old
                                                (cons new
                                                      (insertR* new old (rest l))))
                        :else (cons (first l)
                                    (insertR* new old (rest l))))
    :else (cons (insertR* new old (first l))
                (insertR* new old (rest l))) ))

;; The First Commandment
;;
;; When recurring on a list of atoms, lat, ask two questions about it: (empty? lat) and :else.
;; When recurring on a number, n, ask two questions about it: (zero? n) and :else.
;; When recurring on a list of S-expressions, l, ask three questions about it:
;; (empty? l), (atom? (first l)) and :else.

(defn occur*
  "Counts the number of times an atom a appears in a S-exp"
  [a l]
  (cond
    (empty? l) 0
    (atom? (first l)) (cond
                        (= a (first l)) (add1 (occur* a (rest l)))
                        :else (occur* a (rest l)))
    :else (o+ (occur* a (first l))
             (occur* a (rest l))) ))

(defn subst* [new old l]
  (cond
    (empty? l) l
    (atom? (first l)) (cond
                        (= old (first l)) (cons new
                                                (subst* new old (rest l)))
                        :else (cons (first l)
                                    (subst* new old (rest l))))
    :else (cons (subst* new old (first l))
                (subst* new old (rest l)))) )

(defn insertL* [new old l]
  (cond
    (empty? l) '()
    (atom? (first l)) (cond
                        (= old (first l)) (cons new
                                                (cons old
                                                      (insertL* new old (rest l))))
                        :else (cons (first l)
                                    (insertL* new old (rest l))))
    :else (cons (insertL* new old (first l))
                (insertL* new old (rest l))) ))

(defn member* [a l]
  (cond
    (empty? l) false
    (atom? (first l)) (or (= a (first l))
                          (member* a (rest l)))
    :else (or (member* a (first l))
              (member* a (rest l))) ))

(defn leftmost
  "Finds the leftmost atom in a non-empty list of S-expressions that does not contain the empty list"
  [l]
  (cond
    (atom? (first l)) (first l)
    :else (leftmost (first l)) ))

(defn eqlist1?
  "True if the two lists are equal"
  [l1 l2]
  (cond
    (and (empty? l1) (empty? l2)) true
    (and (empty? l1) (atom? (first l2))) false
    (empty? l1) false

    (and (atom? (first l1)) (empty? l2)) false
    (and (atom? (first l1)) (atom? (first l2))) (and (= (first l1) (first l2)) (eqlist1? (rest l1) (rest l2)))
    (atom? (first l1)) false

    ;; (first l1) is a list
    (empty? l2) false
    (atom? (first l2)) false
    :else (and (eqlist1? (first l1) (first l2))
               (eqlist1? (rest l1) (rest l2))) ))

(defn eqlist?
  "True if the two lists are equal"
  [l1 l2]
  (cond
    (and (empty? l1) (empty? l2)) true
    (or (empty? l1) (empty? l2)) false

    (and (atom? (first l1))
         (atom? (first l2))) (and (= (first l1) (first l2)) (eqlist? (rest l1) (rest l2)))

    (or (atom? (first l1))
        (atom? (first l2))) false
    :else (and (eqlist? (first l1) (first l2))
               (eqlist? (rest l1) (rest l2))) ))

(defn rember2
  "Removes the first occurence of the S-expr from a list of S-expr"
  [s l]
  (cond
    (empty? l) ()
    (= (first l) s) (rest l)
    :else (cons (first l)
                (rember2 s (rest l))) ))



(deftest chap4test
  (testing "rember*"
    (is (= (rember* 'sauce '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce)))
           '(((tomato)) ((bean)) (and ((flying))))
           )))

  (testing "insertR*"
    (is (= (insertR* 'roast 'chuck '((how much (wood))
                                     could
                                     ((a (wood) chuck))
                                     (((chuck)))
                                     (if (a) ((wood chuck)))
                                     could chuck wood))
           '((how much (wood))
             could
             ((a (wood) chuck roast))
             (((chuck roast)))
             (if (a) ((wood chuck roast)))
             could chuck roast wood) )))

  (testing "occur*"
    (is (= 5 (occur* 'banana '((banana)
                               (split ((((banana ice)))
                                       (cream (banana))
                                       sherbet))
                               (banana)
                               (bread)
                               (banana brandy))))))

  (testing "subst*"
    (is (= (subst* 'orange 'banana '((banana)
                                     (split ((((banana ice)))
                                             (cream (banana))
                                             sherbet))
                                     (banana)
                                     (bread)
                                     (banana brandy)))
           '((orange)
             (split ((((orange ice)))
                     (cream (orange))
                     sherbet))
             (orange)
             (bread)
             (orange brandy))
           )))

  (testing "insertL*"
    (is (= (insertL* 'pecker 'chuck '((how much (wood))
                                      could
                                      ((a (wood) chuck))
                                      (((chuck)))
                                      (if (a) ((wood chuck)))
                                      could chuck wood))
           '((how much (wood))
             could
             ((a (wood) pecker chuck))
             (((pecker chuck)))
             (if (a) ((wood pecker chuck)))
             could pecker chuck wood)) ))

  (testing "member*"
    (is (= true (member* 'chips '((potato) (chips ((with) (chips))))))))

  (testing "leftmost"
    (is (= 'potato (leftmost '((potato) (chips ((with) (chips))))))))

  (testing "eqlist?"
    (is (= true (eqlist? '(strawberry ice cream) '(strawberry ice cream))))
    (is (= false (eqlist? '(beef ((sausage)) (and (soda))) '(beef ((salami)) (and (soda))) )))
    (is (= true (eqlist? '(beef ((sausage)) (and (soda))) '(beef ((sausage)) (and (soda))) ))) )

  (testing "rember2"
    (is (= '(a d) (rember2 '(a b) '(a (a b) d)) '(a d)))
    (is (= '(a c) (rember2 '((b)) '(a ((b)) c))) '(a c))
    (is (= '(((a))) (rember2 '(a) '( ( (a) )))) )  ))
