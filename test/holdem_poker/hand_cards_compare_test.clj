(ns holdem-poker.hand-cards-compare-test
  (:require [clojure.test :refer :all]
            [holdem-poker.hand-cards-compare :refer :all]))

(comment
  ;; 评分标准
  (def score-standard
    {:royal-flush     "19"
     :straight-flush  "18"
     :4-of-a-kind     "17"
     :full-house      "16"
     :flush           "15"
     :straight        "14"
     :three-of-a-kind "13"
     :two-pairs       "12"
     :one-pair        "11"
     :high-card       "10"
     :2               "02"
     :3               "03"
     :4               "04"
     :5               "05"
     :6               "06"
     :7               "07"
     :8               "08"
     :9               "09"
     :t               "10"
     :j               "11"
     :q               "12"
     :k               "13"
     :a               "14"}))

(deftest type-score-test
  (are [cards-type result]
    (= (type-score cards-type) result)

    [:royal-flush [[:s :a] [:s :k] [:s :q] [:s :j] [:s :t]]]
    "191413121110"

    [:straight-flush [[:s :6] [:s :5] [:s :4] [:s :3] [:s :2]]]
    "180605040302"

    [:4-of-a-kind [[:s :7] [:c :7] [:h :7] [:d :7] [:d :a]]]
    "170707070714"

    [:full-house [[:d :j] [:c :j] [:s :j] [:d :q] [:s :q]]]
    "161111111212"

    [:straight [[:d :8] [:s :7] [:c :6] [:h :5] [:c :4]]]
    "140807060504"

    [:three-of-a-kind [[:s :7] [:c :7] [:h :7] [:h :a] [:d :8]]]
    "130707071408"

    [:two-pairs [[:d :8] [:h :8] [:s :7] [:c :7] [:h :a]]]
    "120808070714"

    [:one-pair [[:s :7] [:c :7] [:h :a] [:d :9] [:h :8]]]
    "110707140908"

    [:high-card [[:h :a] [:c :k]  [:d :9] [:h :8] [:s :7]]]
    "101413090807"))

(deftest type-compare-test
  (are [position-cards-type result]
    (= (type-compare position-cards-type) result)

    [{:position 0
      :cards-type [:royal-flush [[:s :a] [:s :k] [:s :q] [:s :j] [:s :t]]]}
     {:position 1
      :cards-type [:straight-flush [[:s :6] [:s :5] [:s :4] [:s :3] [:s :2]]]}
     {:position 2
      :cards-type [:one-pair [[:s :7] [:c :7] [:h :a] [:d :9] [:h :8]]]}
     {:position 3
      :cards-type [:one-pair [[:h :7] [:c :7] [:h :a] [:d :9] [:h :8]]]}]

    [["110707140908" [{:position 2
                       :cards-type [:one-pair [[:s :7] [:c :7] [:h :a] [:d :9] [:h :8]]]}
                      {:position 3
                       :cards-type [:one-pair [[:h :7] [:c :7] [:h :a] [:d :9] [:h :8]]]}]]

     ["180605040302" [{:position 1
                       :cards-type [:straight-flush [[:s :6] [:s :5] [:s :4] [:s :3] [:s :2]]]}]]
     ["191413121110" [{:position 0
                       :cards-type [:royal-flush [[:s :a] [:s :k] [:s :q] [:s :j] [:s :t]]]}]]

     ]))

(deftest max-cards-type-test
  (are [cards-types result]
    (= (max-cards-type cards-types) result)

    [{:position 0
      :cards-type [:royal-flush [[:s :a] [:s :k] [:s :q] [:s :j] [:s :t]]]}
     {:position 1
      :cards-type [:straight-flush [[:s :6] [:s :5] [:s :4] [:s :3] [:s :2]]]}
     {:position 2
      :cards-type [:one-pair [[:s :7] [:c :7] [:h :a] [:d :9] [:h :8]]]}
     {:position 3
      :cards-type [:one-pair [[:h :7] [:c :7] [:h :a] [:d :9] [:h :8]]]}]

    [{:position 0
      :cards-type [:royal-flush [[:s :a] [:s :k] [:s :q] [:s :j] [:s :t]]]}]


    [{:position 0
      :cards-type [:high-card [[:h :a] [:c :k]  [:d :9] [:h :8] [:s :7]]]}
     {:position 1
      :cards-type [:high-card [[:h :a] [:c :k]  [:d :9] [:h :8] [:s :7]]]}
     {:position 2
      :cards-type [:one-pair [[:s :7] [:c :7] [:h :a] [:d :9] [:h :8]]]}
     {:position 3
      :cards-type [:one-pair [[:h :7] [:c :7] [:h :a] [:d :9] [:h :8]]]}]

    [{:position 2
      :cards-type [:one-pair [[:s :7] [:c :7] [:h :a] [:d :9] [:h :8]]]}
     {:position 3
      :cards-type [:one-pair [[:h :7] [:c :7] [:h :a] [:d :9] [:h :8]]]}]
    ))
