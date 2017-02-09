(ns holdem-poker.play-proccess
  "打牌过程"
  (:require [holdem-poker.max-cards-type :as mct]
            [holdem-poker.hand-cards-compare :as hcc]))

(defn next-n-position
  "相对于当前位置的下n个位置"
  [position-count curr-position n]
  (mod (+ curr-position n) position-count))

(defn deal-postions-cards
  "从第start-index牌开始，将牌发给指定的位置（包括公共牌位置）"
  [cards start-index positions]
  (let [cards-count (count positions)
        card-indexes (range start-index (+ cards-count start-index))]
    (reduce (fn [r card-index]
              (let [card (get r card-index)]
                (->> (assoc card :position (get positions (- card-index start-index)))
                     (assoc r card-index))))
            cards card-indexes)))

(defn deal-hole-cards
  "发两张底牌"
  [cards position-count button-position]
  (->> (range 0 (* 2 position-count))
       (mapv #(next-n-position position-count button-position (+ 2 %)))
       (deal-postions-cards cards 0)))

(defn deal-n-public-cards
  "从地start-index牌开始，发n张公共牌"
  [cards start-index n]
  (->> (repeat n :public)
       vec
       (deal-postions-cards cards start-index)))

(defn position-cards
  "获得各个位置上的牌（各自底牌+公共牌）"
  [cards]
  (let [a-fn (fn [r position-card]
               (let [{:keys [position card]} position-card]
                 (if-let [p-cards (get r position)]
                   (assoc r position (conj p-cards card))
                   (merge r {position [card]}))))
        p-cards-map (reduce a-fn {} cards)
        public-cards (:public p-cards-map)
        p-cards (dissoc p-cards-map :public)]
    (mapv (fn [[p cs]] {:position p :cards (concat cs public-cards)}) p-cards)))

(defn max-cards-type
  "获得各个位置上最大的牌型"
  [position-cards]
  (mapv (fn [position-card]
          (let [cards-type (mct/cards-type (:cards position-card))]
            {:position (:position position-card)
             :cards-type cards-type}))
        position-cards))

(defn winner-positions
  "获得拥有最大牌型的位置和对应牌型（可能有多个）"
  [position-cards-types]
  (hcc/max-hand-cards position-cards-types))

;;=====================================================================
;; TODO 目前只是假定所有玩家都跟的简易打牌流程，用于测试
(defn easy-play
  "简单的打牌流程"
  [cards position-count button-position]
  ;; 第一步，各个位置发两张底牌，从大盲位置开始发
  (-> (deal-hole-cards cards position-count button-position)
      ;; 第二步，假定都跟，发三张公共牌
      (deal-n-public-cards (* 2 position-count) 3)
      ;; 第三步，假定都跟，发第四张公共牌
      (deal-n-public-cards (+ 3 (* 2 position-count)) 1)
      ;; 第四步，假定都跟，发第五张公共牌
      (deal-n-public-cards (+ 4 (* 2 position-count)) 1)

      ;;;;;;;;;; 发牌结束，进行各位置牌型比较
      ; 各位置拥有的牌（底牌+公共牌）
      position-cards
      ; 各位置最大牌型
      max-cards-type
      ; 最终赢家
      winner-positions))
