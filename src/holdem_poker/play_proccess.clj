(ns holdem-poker.play-proccess
  "打牌过程"
  (:require [holdem-poker.max-cards-type :as mct]
            [holdem-poker.hand-cards-compare :as hcc]))

(defn next-n-position
  "相对于当前位置的下n个位置"
  [position-count curr-position n]
  (-> (+ curr-position n)
      (mod position-count)))

(defn assign-card-position
  "指定一副牌中card-index所在牌的"
  [positions start-index cards card-index]
  (let [card (get cards card-index)]
    (->> (- card-index start-index)
         (get positions)
         (assoc card :position )
         (assoc cards card-index))))

(defn deal-positions-cards
  "从第start-index牌开始，将牌发给指定的位置（包括公共牌位置）"
  [cards start-index positions]
  (->> (count positions)
       (+ start-index)
       (range start-index)
       (reduce (partial assign-card-position positions start-index) cards)))

(defn deal-hole-cards
  "发两张底牌"
  [cards position-count button-position]
  (->> (* 2 position-count)
       (range 0)
       (mapv #(next-n-position position-count button-position (+ 2 %)))
       (deal-positions-cards cards 0)))

(defn deal-n-public-cards
  "从地start-index牌开始，发n张公共牌"
  [cards start-index n]
  (->> (repeat n :public)
       vec
       (deal-positions-cards cards start-index)))

(defn locate-a-card
  "判断一张牌的所属位置，并添加到其中"
  [located-cards {:keys [position card]}]
  (if-let [cards (get located-cards position)]
    (->> (conj cards card)
         (assoc located-cards position))
    (assoc located-cards position [card])))

(defn position-cards
  "获得各个位置上的牌（各自底牌+公共牌）"
  [cards]
  (let [located-cards (reduce locate-a-card {} cards)
        public-cards (:public located-cards)
        p-cards (dissoc located-cards :public)]
    (mapv (fn [[p cs]] {:position p :cards (concat cs public-cards)})
          p-cards)))

(defn max-cards-type
  "获得各个位置上最大的牌型"
  [position-cards]
  (mapv (fn [position-card]
          {:position   (:position position-card)
           :cards-type (mct/cards-type (:cards position-card))})
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
