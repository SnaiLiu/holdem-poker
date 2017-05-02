(ns holdem-poker.play-proccess
  "打牌过程"
  (:require [holdem-poker.max-cards-type :as mct]
            [holdem-poker.hand-cards-compare :as hcc]
            [holdem-poker.utils :refer :all]
            [clojure.set :as set]))

(defn mk-pos
  "数据结构: positions 是位置上的玩家，它的序号单局游戏唯一关心的概念
     - :pos dealer 位置为 0，按实际入座玩家的打牌次序依次增加。如 2 人游戏则分别是 0, 1.
     - :chips 玩家可以用在单局中的筹码数量
     - :status 此局中的状态 :fold(已弃牌) :betted（已下注）:all-in（全押）
     - :bet 单轮中玩家已经下注的数量, 一旦单轮结束词数据必须归 0"
  [pos chips]
  {:pos    pos
   :chips  chips
   :status :wait
   :bet    0})

(defn init-cards
  "初始化一副牌
  card-template:  {:position nil :card [:s :2]}
  :position有：某个座位 '0、1、2、3'，公共牌':public'"
  []
  (for [s [:s :h :d :c]
        n [:a :k :q :j :t :9 :8 :7 :6 :5 :4 :3 :2]]
    {:position nil :card [s n]}))

(defn deal-cards
  "发牌，内部可以一次将所有的牌都发完，包括公共牌"
  [cards position-count]
  (->> cards
       (map #(assoc %2 :position %1)
            (concat (range position-count)                  ;;发第一轮牌
                    (range position-count)                  ;;发第二轮牌
                    ;; 发公共牌，nil表示切牌
                    [nil :public :public :public nil :public nil :public]))))

(defn new-game
  "单局游戏
  position-chips: 各座位上的筹码数"
  ([positon-chips]
   (new-game positon-chips (shuffle (init-cards))))
  ([positon-chips cards]
   {:positions (vec (map-indexed mk-pos positon-chips))
    :pot       [0 #{}]
    :side-pots []
    :round     0
    :cur-pos   1
    :cards     (deal-cards cards (count positon-chips))
    ;;;最小筹码数，一轮结束时，每个玩家(除弃牌的位置)都应该向彩池中投入这么多筹码（或者all-in）
    ;:min-bet   0
    }))

(defn position-cnt
  "游戏座位数"
  [game]
  (count (:positions game)))

(defn alive?
  "判断某个位置是否存活"
  [pos]
  (not= :fold (:status pos)))

(defn single-alive?
  "是否只有一个人存活：即只有一个人没弃牌，其他人都弃牌了。"
  [{:keys [positions]}]
  (= 1 (count (filter alive? positions))))

(defn bet-total
  "所有位置下注的总筹码数"
  [game]
  (->> (:positions game) (map :bet) (reduce + 0)))

(defn single-winner
  "单个赢家：只有一个位置没弃牌，其他位置都弃牌，则该位置赢，这种情况下，边池应该为空"
  [game]
  (let [game    (update-in game [:pot 0] + (bet-total game)) ; 将所有人的筹码放入主池
        win-pos (find-first alive? (:positions game))]
    (assoc game :winners
                (->> (conj (:side-pots game) (:pot game))   ;; 此时边池为空，是不是直接（:pot game)？？
                     (map first)
                     (map #(vector (:pos win-pos) %))))))

(defn max-bet
  "最大的下注数"
  [game]
  (apply max (map :bet (:positions game))))

(defn all-in?
  "某个位置是否是all-in：状态为all-in，下注筹码小于最小筹码
  ？？为什么不是直接根据状态判断？？？"
  [max-bet pos]
  (and (= :all-in (:status pos)) (< 0 (:bet pos) max-bet)))

(defn new-side-pot?
  "是否需要新建一个边池 side pot"
  [game]
  (some #(all-in? (max-bet game) %) (:positions game)))

(defn new-side-pot
  "给 game 建立必要的边池"
  [game]
  (let [pos            (find-first #(all-in? (max-bet game) %) (:positions game))
        bet            (:bet pos)
        contenders     (get-in game [:pot 1])
        side-pot-chips (* bet (count contenders))
        side-pot       [side-pot-chips contenders]
        new-pot        [(- (bet-total game) side-pot-chips)
                        (disj contenders (:pos pos))]
        positions      (mapv #(if (contenders (:pos %))
                                (update % :bet - bet)
                                %)
                             (:positions game))]
    (-> game
        (assoc :positions positions)
        (update :side-pots conj side-pot)
        (assoc :pot new-pot))))

(defn find-next-pos
  "找到下一个可以操作的位置"
  [game cur-pos]
  (let [poses (concat (range (inc cur-pos) (position-cnt game)) (range 0 cur-pos))]
    (find-first #(#{:wait :betted} (get-in game [:positions % :status])) poses)))

(defn next-pos
  "更新下一个需要操作的位置"
  [game]
  (update game :cur-pos #(find-next-pos game %)))

(defn place-bet
  "下注。pos: 下注座位号； chips: 下的筹码数"
  [game pos chips]
  (let [pos-chips (get-in game [:positions pos :chips])]
    (-> game
        (update-in [:positions pos :chips] - chips)
        (update-in [:positions pos :bet] + chips)
        (update-in [:positions pos :status]
                   (fn [os] (if (= pos-chips chips) :all-in os)))
        ;; 下注者参与底池竞争
        (update-in [:pot 1] conj pos))))

(defn round-end?
  "单轮是否结束?"
  [{:keys [positions]}]
  (let [waiting (filter #(= :wait (:status %)) positions)
        betting (filter #(#{:betted :all-in} (:status %)) positions)]
    (and
      ;; 所有人都已经结束动作，且每个人都已经向奖池投入相同的注额。
      (zero? (count waiting))
      ;; ??? 假如A（betted:1000） B(all-in:300) C(:all-in:200)此时也应该结束一轮吧？？
      (= 1 (->> betting (map :bet) (set) (count))))))

(defn merge-round
  "结束一个下注轮，进入下一个下注轮"
  [game]
  (-> game
      ;; 底池包含所有的下注筹码
      (update-in [:pot 0] + (bet-total game))
      (update :positions
              (fn [ps] (mapv #(if (= :betted (:status %))
                                (assoc % :bet 0 :status :wait)
                                %) ps)))
      (assoc :cur-pos (find-next-pos game 0))
      (update :round inc)))

(defn show-down?
  "是否已经可以摊牌?"
  [game]
  (or
    ;河牌圈结束
    (> (:round game) 3)
    ;所有玩家要么收牌，要么全压
    (set/subset? (->> (:positions game) (map :status) (set)) #{:fold :all-in})))

(defn pos-cards-type
  "获得各个位置上的牌（各自底牌+公共牌）"
  [cards pos]
  (let [p-cards (->> (filter #(#{pos :public} (:position %)) cards)
                     (mapv :card))]
    (mct/cards-type p-cards)))

(defn find-winners
  "查找获胜者"
  [cards positions]
  (let [type-fn (fn [pos] (vector pos (pos-cards-type cards pos)))
        types     (->> positions
                       (map type-fn)
                       (sort-by second (desc hcc/compare-hands)))
        lagest-cs (-> types first second)]
    (->> types
         (take-while #(= lagest-cs (second %)))
         (map first))))

(defn show-down-pot
  "摊牌过程"
  [{:keys [positions cards] :as game} [pot-size contenders]]
  (let [win-positions (->> (map #(get positions %) contenders)
                           (filter alive?)
                           (map :pos)
                           (find-winners cards))
        share         (quot pot-size (count win-positions))
        winners       (mapv #(vector % share) win-positions)]
    winners))

(defn show-down
  "摊牌，输出赢家"
  [game]
  (assoc game :winners
              (mapcat #(show-down-pot game %)
                      (conj (:side-pots game) (:pot game)))))


(defn pack
  "用规则检查游戏状态并更新之,
  理论上每一次操作都要进行常规检查。"
  [game]
  (reduce
    (fn [g [pred f]]
      (if (pred g)
        (f g)
        g))
    game
    [[(constantly true) next-pos]
     [new-side-pot? new-side-pot]
     [round-end? merge-round]
     [show-down? show-down]
     [single-alive? single-winner]]))

;;=================================
;; 操作

(defn blind
  "下盲注"
  [{:keys [cur-pos] :as game} chips]
  (-> game
      (place-bet cur-pos chips)
      (assoc :min-bet chips)
      (pack)))

(defn bet
  "下注 chips"
  [{:keys [cur-pos] :as game} chips]
  (-> game
      (assoc-in [:positions cur-pos :status] :betted)
      (place-bet cur-pos chips)
      (pack)))

(defn call
  "跟牌"
  [{:keys [cur-pos] :as game}]
  (bet game (- (max-bet game) (get-in game [:positions cur-pos :bet]))))

(defn all-in
  "全压"
  [{:keys [cur-pos] :as game}]
  (let [chips (get-in game [:positions cur-pos :chips])
        new-game (bet game chips)]
    new-game))

(defn fold
  "收牌"
  [{:keys [cur-pos] :as game}]
  (-> game
      (assoc-in [:positions cur-pos :status] :fold)
      (assoc-in [:positions cur-pos :bet] 0)
      (update-in [:pot 0] + (get-in game [:positions cur-pos :bet]))
      (update-in [:pot 1] disj cur-pos)
      (update :side-pots (fn [sp] (mapv #(update % 1 disj cur-pos) sp)))
      (pack)))

(defn check
  "检查. 是 bet 0 的简洁说法"
  [game]
  (bet game 0))

; 用数据表达操作
;--------------
(def op-m
  "外部数据到操作函数的转换表"
  {:fold  fold :bet bet :all-in all-in
   :check check :call call :blind blind})

(defn bet-chips-ok?
  "下注额 chips 是否符合规则"
  [game chips]
  (let [cur-bet (max-bet game)]
    (if (pos? cur-bet)
      (or (= cur-bet chips);call
          (>= chips (* 2 cur-bet)))
      (or (zero? chips)
          (>= chips (or (:min-bet game) 0))))))

(defn valid?
  "检测操作是否合法"
  [game [op pos chips]]
  (and
    (op-m op)
    (nil? (:winners game))                                  ;游戏尚未结束
    (= pos (:cur-pos game))                                 ;位置正确
    (or (nil? chips) (pos? chips))
    (or (not= op :bet) (bet-chips-ok? game chips))))

(defn advance-game
  "游戏推进的接口"
  [game operations]
  (reduce
    (fn [g [op _ & args :as operation]]
      (if (valid? g operation)
        (apply (get op-m op) g args)
        g))
    game operations))