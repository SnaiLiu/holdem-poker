(ns holdem-poker.play-proccess-test
  "打牌过程测试"
  (:require [clojure.test :refer :all]
            [holdem-poker.play-proccess :refer :all]))

(defn init-cards
  "初始化一副牌用于测试
  card-template:  {:owner nil :card [:s :2]}
  :owner有：某个座位 '0、1、2、3'，公共牌'public'"
  []
  (let [suits [:s :h :c :d]
        numbers [:2 :3 :4 :5 :6 :7 :8 :9 :t :j :q :k :a]
        fn-suit-cards (fn [suit]
                        (map #(do [suit %]) numbers))]
    (->> (map fn-suit-cards suits)
         (apply concat [])
         (mapv #(do {:owner nil :card %})))))

(deftest init-cards-test
  (is (= (init-cards) [{:owner nil, :card [:s :2]}
                       {:owner nil, :card [:s :3]}
                       {:owner nil, :card [:s :4]}
                       {:owner nil, :card [:s :5]}
                       {:owner nil, :card [:s :6]}
                       {:owner nil, :card [:s :7]}
                       {:owner nil, :card [:s :8]}
                       {:owner nil, :card [:s :9]}
                       {:owner nil, :card [:s :t]}
                       {:owner nil, :card [:s :j]}
                       {:owner nil, :card [:s :q]}
                       {:owner nil, :card [:s :k]}
                       {:owner nil, :card [:s :a]}
                       {:owner nil, :card [:h :2]}
                       {:owner nil, :card [:h :3]}
                       {:owner nil, :card [:h :4]}
                       {:owner nil, :card [:h :5]}
                       {:owner nil, :card [:h :6]}
                       {:owner nil, :card [:h :7]}
                       {:owner nil, :card [:h :8]}
                       {:owner nil, :card [:h :9]}
                       {:owner nil, :card [:h :t]}
                       {:owner nil, :card [:h :j]}
                       {:owner nil, :card [:h :q]}
                       {:owner nil, :card [:h :k]}
                       {:owner nil, :card [:h :a]}
                       {:owner nil, :card [:c :2]}
                       {:owner nil, :card [:c :3]}
                       {:owner nil, :card [:c :4]}
                       {:owner nil, :card [:c :5]}
                       {:owner nil, :card [:c :6]}
                       {:owner nil, :card [:c :7]}
                       {:owner nil, :card [:c :8]}
                       {:owner nil, :card [:c :9]}
                       {:owner nil, :card [:c :t]}
                       {:owner nil, :card [:c :j]}
                       {:owner nil, :card [:c :q]}
                       {:owner nil, :card [:c :k]}
                       {:owner nil, :card [:c :a]}
                       {:owner nil, :card [:d :2]}
                       {:owner nil, :card [:d :3]}
                       {:owner nil, :card [:d :4]}
                       {:owner nil, :card [:d :5]}
                       {:owner nil, :card [:d :6]}
                       {:owner nil, :card [:d :7]}
                       {:owner nil, :card [:d :8]}
                       {:owner nil, :card [:d :9]}
                       {:owner nil, :card [:d :t]}
                       {:owner nil, :card [:d :j]}
                       {:owner nil, :card [:d :q]}
                       {:owner nil, :card [:d :k]}
                       {:owner nil, :card [:d :a]}])))

;; 第一步，每人发牌2张底牌
(deftest deal-hole-cards-test
  (let [initial-cards (init-cards)]
    (is (= (deal-hole-cards initial-cards 3)
           (concat [{:owner "2", :card [:s :2]}
                    {:owner "0", :card [:s :3]}
                    {:owner "1", :card [:s :4]}
                    {:owner "2", :card [:s :5]}
                    {:owner "0", :card [:s :6]}
                    {:owner "1", :card [:s :7]}]
                   (vec (take-last (- 52 6) initial-cards)))))

    ))


