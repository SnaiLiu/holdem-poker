(ns holdem-poker.play-proccess-test
  "打牌过程测试"
  (:require [clojure.test :refer :all]
            [holdem-poker.play-proccess :refer :all]))

(defn init-cards
  "初始化一副牌用于测试
  card-template:  {:position nil :card [:s :2]}
  :position有：某个座位 '0、1、2、3'，公共牌'public'"
  []
  (let [suits [:s :h :c :d]
        numbers [:2 :3 :4 :5 :6 :7 :8 :9 :t :j :q :k :a]
        fn-suit-cards (fn [suit]
                        (map #(do [suit %]) numbers))]
    (->> (map fn-suit-cards suits)
         (apply concat [])
         (mapv #(do {:position nil :card %})))))

(deftest init-cards-test
  (is (= (init-cards) [{:position nil, :card [:s :2]}
                       {:position nil, :card [:s :3]}
                       {:position nil, :card [:s :4]}
                       {:position nil, :card [:s :5]}
                       {:position nil, :card [:s :6]}
                       {:position nil, :card [:s :7]}
                       {:position nil, :card [:s :8]}
                       {:position nil, :card [:s :9]}
                       {:position nil, :card [:s :t]}
                       {:position nil, :card [:s :j]}
                       {:position nil, :card [:s :q]}
                       {:position nil, :card [:s :k]}
                       {:position nil, :card [:s :a]}
                       {:position nil, :card [:h :2]}
                       {:position nil, :card [:h :3]}
                       {:position nil, :card [:h :4]}
                       {:position nil, :card [:h :5]}
                       {:position nil, :card [:h :6]}
                       {:position nil, :card [:h :7]}
                       {:position nil, :card [:h :8]}
                       {:position nil, :card [:h :9]}
                       {:position nil, :card [:h :t]}
                       {:position nil, :card [:h :j]}
                       {:position nil, :card [:h :q]}
                       {:position nil, :card [:h :k]}
                       {:position nil, :card [:h :a]}
                       {:position nil, :card [:c :2]}
                       {:position nil, :card [:c :3]}
                       {:position nil, :card [:c :4]}
                       {:position nil, :card [:c :5]}
                       {:position nil, :card [:c :6]}
                       {:position nil, :card [:c :7]}
                       {:position nil, :card [:c :8]}
                       {:position nil, :card [:c :9]}
                       {:position nil, :card [:c :t]}
                       {:position nil, :card [:c :j]}
                       {:position nil, :card [:c :q]}
                       {:position nil, :card [:c :k]}
                       {:position nil, :card [:c :a]}
                       {:position nil, :card [:d :2]}
                       {:position nil, :card [:d :3]}
                       {:position nil, :card [:d :4]}
                       {:position nil, :card [:d :5]}
                       {:position nil, :card [:d :6]}
                       {:position nil, :card [:d :7]}
                       {:position nil, :card [:d :8]}
                       {:position nil, :card [:d :9]}
                       {:position nil, :card [:d :t]}
                       {:position nil, :card [:d :j]}
                       {:position nil, :card [:d :q]}
                       {:position nil, :card [:d :k]}
                       {:position nil, :card [:d :a]}])))


(deftest next-n-position-test
  (are [position-count curr-position n result]
    (= (next-n-position position-count curr-position n) result)

    2 0 1 1
    2 0 2 0
    2 1 3 0

    3 0 1 1
    3 1 1 2
    3 1 2 0))

(deftest deal-postions-cards-test
  (let [initial-cards (init-cards)]
    (is (= (->> (deal-postions-cards initial-cards 0 [:public :public :public])
                (take 3))
           [{:position :public :card [:s :2]}
            {:position :public :card [:s :3]}
            {:position :public :card [:s :4]}]
           ))))

;; 第一步，每人发牌2张底牌
(deftest deal-hole-cards-test
  (let [initial-cards (init-cards)]
    (is (= (deal-hole-cards initial-cards 3 0)
           (concat [{:position 2 :card [:s :2]}
                    {:position 0 :card [:s :3]}
                    {:position 1 :card [:s :4]}
                    {:position 2 :card [:s :5]}
                    {:position 0 :card [:s :6]}
                    {:position 1 :card [:s :7]}]
                   (vec (take-last (- 52 6) initial-cards)))))))

(deftest deal-n-public-cards-test
  (let [initial-cards (init-cards)
        deal-resut-cards (deal-n-public-cards initial-cards 6 3)]
    (is (= (take 3 (take-last (- 52 6) deal-resut-cards))
           [{:position :public, :card [:s :8]}
            {:position :public, :card [:s :9]}
            {:position :public, :card [:s :t]}]
           ))))

(deftest position-cards-test
  (let [        cards [{:position 0, :card [:s :2]}
                       {:position 1, :card [:s :3]}
                       {:position 0, :card [:s :4]}
                       {:position 1, :card [:s :5]}
                       {:position :public, :card [:s :6]}
                       {:position :public, :card [:s :7]}
                       {:position :public, :card [:s :8]}
                       {:position :public, :card [:s :9]}
                       {:position :public, :card [:s :t]}]]

    (is (= (position-cards cards)
           [{:position 0 :cards [[:s :2] [:s :4] [:s :6] [:s :7] [:s :8] [:s :9] [:s :t]]}
            {:position 1 :cards [[:s :3] [:s :5] [:s :6] [:s :7] [:s :8] [:s :9] [:s :t]]}]))))

(deftest max-cards-type-test
  (let [position-cards [{:position 0 :cards [[:s :2] [:s :3] [:s :4] [:s :5] [:s :6] [:d :3] [:d :4]]}
                        {:position 1 :cards [[:c :7] [:d :9] [:s :7] [:h :7] [:d :k] [:d :7] [:d :a]]}]]
    (is (= (max-cards-type position-cards)
           [{:position 0 :cards-type [:straight-flush [[:s :6] [:s :5] [:s :4] [:s :3] [:s :2]]]}
            {:position 1 :cards-type [:4-of-a-kind [[:s :7] [:c :7] [:h :7] [:d :7] [:d :a]]]}]
           ))
    ))

(deftest winner-positions-test
  (let [cards [{:position 0 :cards-type [:straight-flush [[:s :6] [:s :5] [:s :4] [:s :3] [:s :2]]]}
               {:position 1 :cards-type [:4-of-a-kind [[:s :7] [:c :7] [:h :7] [:d :7] [:d :a]]]}]]
    (is (= (winner-positions cards)
           [{:position 0 :cards-type [:straight-flush [[:s :6] [:s :5] [:s :4] [:s :3] [:s :2]]]}]))
    ))

(deftest easy-play-test
  (let [cards [{:position 0, :card [:s :2]}
               {:position 1, :card [:h :t]}
               {:position 0, :card [:s :3]}
               {:position 1, :card [:h :9]}
               {:position :public, :card [:s :4]}
               {:position :public, :card [:s :5]}
               {:position :public, :card [:s :6]}
               {:position :public, :card [:h :9]}
               {:position :public, :card [:h :t]}]]
    (is (= (easy-play cards 2 0)
           [{:position 0 :cards-type [:straight-flush [[:s :6] [:s :5] [:s :4] [:s :3] [:s :2]]]}]))))


