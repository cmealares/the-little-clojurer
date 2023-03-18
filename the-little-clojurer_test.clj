(ns the-little-clojurer.core-test
  (:require [clojure.test :refer :all]
            [the-little-clojurer.core :refer :all]))


;; ---------------------------------------------------------------------------
;; 2. Do It, Do It Again, and Again, and Again...
;; ---------------------------------------------------------------------------

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

(deftest chap5test
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


;; ---------------------------------------------------------------------------
;; 6. Shadows
;; ---------------------------------------------------------------------------

(deftest chap6test
  (testing "numbered?"
    (is (= true  (numbered? 1)))
    (is (= true  (numbered? '(4 x 5))))
    (is (= true  (numbered? '(3 + (4 x 5))))) )

  (testing "value"
    (is (= 1  (value 1)))
    (is (= 20  (value '(4 x 5))))
    (is (= 23  (value '(3 + (4 x 5))))) )

  (testing "prefix-value"
    (is (= 1  (prefix-value 1)))
    (is (= 20  (prefix-value '(x 4 5))))
    (is (= 23  (prefix-value '(+ 3 (x 4 5))))) )

  (testing "Empty lists arithmetic"
    (is (= true (sero? '())))
    (is (= false (sero? '(()))))
    (is (= '(()) (edd1 '())))
    (is (= true (sero? (zub1 (edd1 '())))))
    (is (= '(() () () () ()) (z+ '(() ()) '(() () ()))))) )


;; ---------------------------------------------------------------------------
;; 7. Friends and Relations
;; ---------------------------------------------------------------------------

(deftest chap7test
  (testing "set?"
    (is (= false (set? '(apple peaches apple plum))))
    (is (= true (set? '(apple peaches pears plum)))))

  (testing "makeset"
    (is (= (makeset '(apple peach pear peach plum apple lemon peach)) '(pear plum apple lemon peach)))
    (is (= (makeset2 '(apple peach pear peach plum apple lemon peach)) '(apple peach pear plum lemon))))

  (testing "subset?"
    (is (= true (subset? '(5 chicken wings) '(5 hamburgers 2 pieces fried chicken and light duckling wings))))
    (is (= false (subset? '(5 chicken wings) '(5 hamburgers 2 pieces fried duckling wings)))))

  (testing "eqset?"
    (is (= true (eqset? '(6 large chickens with wings) '(6 chickens with large wings)))))

  (testing "intersect?"
    (is (= true (intersect? '(stewed tomatoes and macaroni) '(macaroni and cheese)))))

  (testing "intersect"
    (is (= '(and macaroni) (intersect '(stewed tomatoes and macaroni) '(macaroni and cheese)))))

  (testing "union"
    (is (= '(stewed tomatoes caserole macaroni and cheese)
           (union '(stewed tomatoes and macaroni caserole) '(macaroni and cheese)))))

  (testing "set-diff"
    (is (= '(a c) (set-diff '(a b c d) '(d m b n)))))

  (testing "intersectall"
    (is (= (intersectall '((a b c) (c a d e) (e f g h a b)))
           '(a))))

  (testing "pair representation"
    (is (a-pair? '(3 7)))
    (is (a-pair? '((2) (pair))))
    (is (not (a-pair? '())))
    (is (not (a-pair? 0)))
    (is (not (a-pair? '(3 7 11))))
    ;;
    (is (a-pair? (build 2 '(3))))
    (is (= 3 (third '(1 2 3 4 5)))) )

  (testing "relations"
    (is (fun? '((8 3) (4 2) (7 6) (6 2) (3 4))))
    (is (not (fun? '((d 4) (b 0) (b 9) (e 5) (g 4)))))
    (is (= (revrel '((8 a) (pumpkin pie) (got sick)))
           '((a 8) (pie pumpkin) (sick got))))
    (is (not (fullfun? '((8 3) (4 2) (7 6) (6 2) (3 4)))))
    (is (fullfun? '((8 3) (4 8) (7 6) (6 2) (3 4))))
    (is (= (seconds '((a 1) (b 2))) '(1 2)))) )


;; ---------------------------------------------------------------------------
;; 8. Lambda the Ultimate
;; ---------------------------------------------------------------------------
