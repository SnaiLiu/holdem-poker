(ns holdem-poker.max-cards-type-test
  (:require [holdem-poker.max-cards-type :refer :all]
            [clojure.test :refer :all]))


;;手上2张牌， 桌面5张公共牌 加起来7张，比大小的时候从7张中取5张，
;;组成最大的牌型，显示牌型和5张牌
;; 花色：0(黑) 1（红） 2（梅） 3（方）;;用数字表达牺牲可读性，容易出错，程序中数值加减导致错误
;; 牌面数值：0 1 2 3 4 5 6 7 8 9 10 11 12 => A23456789TJQK ;;

;; 花色：:s(黑) :h（红） :c（梅） :d（方）;;用数字表达牺牲可读性，容易出错，程序中数值加减导致错误
;; 牌面数值：:a :2 :3 :4 :5 :6 :7 :8 :9 :t :j :q :k => A23456789TJQK ;;

;; 花色、牌面数值 用keyword
;; 皇家同花顺，同花顺 四带一 葫芦 同花 顺子 三同 两对 对子 高张


(deftest convert-pais-test
         (are [pais result]
              (= (convert-cards {:2 2 :3 3 :4 4 :5 5 :6 6 :7 7 :8 8 :9 9 :t 10 :j 11 :q 12 :k 13 :a 14} pais) result)
              [[:s :2] [:s :3] [:s :4] [:s :5] [:s :6] [:d :7] [:d :8] [:d :9] [:d :t] [:d :j] [:d :q] [:d :k] [:d :a]]
              [[:s 2] [:s 3] [:s 4] [:s 5] [:s 6] [:d 7] [:d 8] [:d 9] [:d 10] [:d 11] [:d 12] [:d 13] [:d 14]]))

(deftest sort-by-num-test
         (are [pais result]
              (= (sort-by-num  < pais) result)
              [[:s 2] [:s 8] [:s 7] [:d 6] [:d 4]]
              [[:s 2] [:d 4] [:d 6] [:s 7] [:s 8]])

         (are [pais result]
              (= (sort-by-num > pais ) result)
              [[:s 2] [:s 8] [:s 7] [:d 6] [:d 4]]
              [[:s 8] [:s 7] [:d 6] [:d 4] [:s 2]]))


(deftest filter-cards-by-count-test
         (are [pais t-count result]
              (= result (filter-cards-by-count pais t-count))

              {4 '(4 4 4 4)
               3 '(3 3 3)
               2 '(2 2 )
               13 '(13)
               12 '(12)} 4 {:4 '(4)}

              {4 '(4 4 4 4)
               3 '(3 3 3)
               2 '(2 2 )
               13 '(13)
               12 '(12)} 3 {:3 '(3)}

              {4 '(4 4 4 4)
               3 '(3 3 3)
               2 '(2 2 )
               13 '(13)
               12 '(12)} 2 {:2 '(2)}

              {4 '(4 4 4 4)
               3 '(3 3 3)
               2 '(2 2 )
               13 '(13)
               12 '(12)} 1 {:1 '(13 12)}
              ))

(deftest cards-group-test
         (are [pais type-fn result]
              (= (cards-group pais type-fn) result)

              [[:s 4] [:h 4] [:c 4] [:d 4] [:s 3] [:h 3] [:c 3] [:h 2] [:d 2] [:c 13] [:d 12]] first
              {:s '(4 3)
               :h '(4 3 2)
               :c '(13 4 3)
               :d '(12 4 2)}

              [[:s 4] [:h 4] [:c 4] [:d 4] [:s 3] [:h 3] [:c 3] [:h 2] [:d 2] [:c 13] [:d 12]] last
              {4 '(4 4 4 4)
               3 '(3 3 3)
               2 '(2 2 )
               13 '(13)
               12 '(12)}))

(deftest cards-classify-test
         (are [original-cards result]
              (= (cards-classify original-cards) result)

              [[:s 4] [:h 4] [:c 4] [:d 4] [:s 3] [:h 3] [:c 3] [:h 2] [:d 2] [:c 13] [:d 12]]
              {:original '([:c 13] [:d 12] [:s 4] [:h 4] [:c 4] [:d 4] [:s 3] [:h 3] [:c 3] [:h 2] [:d 2] )
               :s        '(4 3)
               :c        '(13 4 3)
               :h        '(4 3 2)
               :d        '(12 4 2)
               :4        '(4)
               :3        '(3)
               :2        '(2)
               :1        '(13 12)}
              ))

(deftest max-straight-cards-test
         (are [sorted-nums n result]
              (= (max-straight-nums sorted-nums n) result)

              [9 8 7 6 5 4 3 1] 5 [9 8 7 6 5]
              [9 8 7 5 5 4 3 1] 5 nil
              ))


(deftest royal-flush-filter-test
         (are [cards result]
              (= (royal-flush-filter cards) result)

              (cards-classify [[:s 14] [:s 13] [:s 12] [:s 11] [:s 10] [:h 3] [:c 3]])
              [:royal-flush [[:s 14] [:s 13] [:s 12] [:s 11] [:s 10]]]

              (cards-classify [[:s 14] [:s 12] [:s 10] [:s 9] [:s 8] [:h 3] [:c 3]])
              nil
              ))

(deftest straight-flush-filter-test
         (are [cards result]
              (= (straight-flush-filter cards) result)

              (cards-classify [[:s 13] [:s 12] [:s 11] [:s 10] [:s 9] [:s 8] [:c 3]])
              [:straight-flush [[:s 13] [:s 12] [:s 11] [:s 10] [:s 9]]]
              (cards-classify [[:s 13] [:s 12] [:s 10] [:h 10] [:s 9] [:s 8] [:c 3]])
              nil
              ))

(deftest four-of-a-kind-filter-test
         (are [cards result]
              (= (four-of-a-kind-filter cards) result)

              (cards-classify [[:d 14] [:s 13] [:d 13] [:c 13] [:h 13] [:s 9][:c 3]])
              [:4-of-a-kind [[:s 13] [:c 13] [:h 13] [:d 13] [:d 14]]]

              (cards-classify [[:d 14] [:s 13] [:d 13] [:c 13] [:h 12] [:s 9][:c 3]])
              nil
              ))

(deftest full-house-filter-test
         (are [cards result]
              (= (full-house-filter cards) result)

              (cards-classify [[:s 13] [:d 13] [:c 13] [:h 12] [:d 12] [:s 9][:c 3]])
              [:full-house [[:s 13] [:d 13] [:c 13] [:h 12] [:d 12]]]

              (cards-classify [[:s 13] [:d 13] [:c 13] [:h 12] [:d 11] [:s 3][:c 3]])
              [:full-house [[:s 13] [:d 13] [:c 13] [:s 3] [:c 3]]]

              (cards-classify [[:s 13] [:d 13] [:c 13] [:h 12] [:d 12] [:s 12][:c 3]])
              [:full-house [[:s 13] [:d 13] [:c 13] [:h 12] [:d 12]]]

              (cards-classify [[:s 13] [:d 13] [:c 13] [:h 12] [:d 12] [:s 12][:c 3] [:d 3]])
              [:full-house [[:s 13] [:d 13] [:c 13] [:h 12] [:d 12]]]

              (cards-classify [[:s 13] [:d 13] [:c 13] [:h 12] [:d 11] [:s 9][:c 3]])
              nil))

(deftest flush-filter-test
         (are [cards result]
              (= (flush-filter cards) result)

              (cards-classify [[:s 14] [:s 13] [:s 10] [:s 8] [:s 7] [:s 6][:c 3]])
              [:flush [[:s 14] [:s 13] [:s 10] [:s 8] [:s 7]]]

              (cards-classify [[:s 14] [:h 13] [:s 10] [:s 8] [:s 7] [:s 6][:c 3]])
              [:flush [[:s 14] [:s 10] [:s 8] [:s 7] [:s 6]]]

              (cards-classify [[:s 14] [:h 13] [:h 10] [:s 8] [:s 7] [:s 6][:c 3]])
              nil
              ))

(deftest straight-filter-test
         (are [cards result]
              (= (straight-filter cards) result)

              (cards-classify [[:s 14] [:h 13] [:s 12] [:s 11] [:c 10] [:s 6][:c 3]])
              [:straight [[:s 14] [:h 13] [:s 12] [:s 11] [:c 10]]]

              (cards-classify [[:s 14] [:h 12] [:s 11] [:s 10] [:c 9] [:s 8][:c 3]])
              [:straight [[:h 12] [:s 11] [:s 10] [:c 9] [:s 8]]]

              (cards-classify [[:s 14] [:h 12] [:h 10] [:s 10] [:c 9] [:s 8][:c 3]])
              nil
              ))

(deftest three-of-a-kind-filter-test
         (are [cards result]
              (= (three-of-a-kind-filter cards) result)

              (cards-classify [[:s 14] [:h 13] [:s 13] [:c 13] [:c 9] [:s 8][:c 3]])
              [:three-of-a-kind [[:h 13] [:s 13] [:c 13] [:s 14] [:c 9]]]

              (cards-classify [[:s 14] [:h 14] [:s 13] [:c 13] [:c 9] [:s 8][:c 3]])
              nil
              ))

(deftest two-pairs-filter-test
         (are [cards result]
              (= (two-pairs-filter cards) result)

              (cards-classify [[:s 14] [:h 14] [:s 13] [:c 13] [:c 9] [:s 9][:c 3]])
              [:two-pairs [[:s 14] [:h 14] [:s 13] [:c 13] [:c 9] ]]

              (cards-classify [[:s 14] [:h 13] [:s 13] [:c 12] [:c 12] [:s 9][:c 3]])
              [:two-pairs [[:h 13] [:s 13] [:c 12] [:c 12] [:s 14]]]

              (cards-classify [[:s 14] [:h 13] [:s 13] [:c 12] [:c 11] [:s 9][:c 3]])
              nil
              ))

(deftest one-pair-filter-test
         (are [cards result]
              (= (one-pair-filter cards) result)

              (cards-classify [[:s 14] [:h 13] [:s 12] [:c 12] [:c 9] [:s 8][:c 3]])
              [:one-pair [ [:s 12] [:c 12] [:s 14] [:h 13] [:c 9] ]]

              (cards-classify [[:s 14] [:h 13] [:s 12] [:c 11] [:c 9] [:s 8][:c 3]])
              nil))

(deftest high-card-filter-test
         (are [cards result]
              (= (high-card-filter cards) result)

              (cards-classify [[:s 14] [:h 13] [:s 12] [:c 10] [:c 9] [:s 8][:c 3]])
              [:high-card [[:s 14] [:h 13] [:s 12] [:c 10] [:c 9]]]
              ))

(deftest 测试德州扑克最优牌型
         (are [pais result]
              (= (cards-type pais) result)

              [[:s :a] [:s :k] [:s :j] [:s :q] [:s :t] [:d :3] [:d :4]]
              [:royal-flush [[:s :a] [:s :k] [:s :q] [:s :j] [:s :t]]]

              [[:s :2] [:s :3] [:s :4] [:s :5] [:s :6] [:d :3] [:d :4]]
              [:straight-flush [[:s :6] [:s :5] [:s :4] [:s :3] [:s :2]]]

              [[:c :7] [:d :9] [:s :7] [:h :7] [:d :k] [:d :7] [:d :a]]
              [:4-of-a-kind [[:s :7] [:c :7] [:h :7] [:d :7] [:d :a]]]

              [[:d :q] [:d :j] [:c :j] [:s :j] [:s :q] [:d :8] [:d :t]]
              [:full-house [[:d :j] [:c :j] [:s :j] [:d :q] [:s :q]]]

              [[:c :3] [:c :9] [:d :t] [:c :j] [:c :8] [:c :q] [:s :t]]
              [:flush  [[:c :q] [:c :j] [:c :9] [:c :8] [:c :3]]]

              [[:s :7] [:c :6] [:d :8] [:h :5] [:c :4] [:s :8] [:h :8]]
              [:straight [[:d :8] [:s :7] [:c :6] [:h :5] [:c :4]]]

              [[:s :7] [:c :7] [:d :8] [:h :7] [:c :4] [:s :2] [:h :a]]
              [:three-of-a-kind [[:s :7] [:c :7] [:h :7] [:h :a] [:d :8]]]

              [[:s :7] [:c :7] [:d :8] [:h :8] [:c :4] [:s :4] [:h :a]]
              [:two-pairs [[:d :8] [:h :8] [:s :7] [:c :7] [:h :a]]]

              [[:s :7] [:c :7] [:d :9] [:h :8] [:c :3] [:s :4] [:h :a]]
              [:one-pair [[:s :7] [:c :7] [:h :a] [:d :9] [:h :8]]]

              [[:s :7] [:c :k] [:d :9] [:h :8] [:c :3] [:s :4] [:h :a]]
              [:high-card [[:h :a] [:c :k]  [:d :9] [:h :8] [:s :7]]]
              ))

