(ns holdem-poker.max-cards-type
  "德州扑克最大牌型分析单元
  输入")

(comment
  (def cards-classify
    {:original '([:s 14] [:s 13] [:h 12] [:h 11] [:c 9] [:c 8]) ;; 原始牌 从大到小排序
     ;;按花色分类， 牌从大到小排序
     :s        '(5 4 3 2)                                   ;;黑
     :h        '(9 8)                                       ;;红
     :c        '(3 2)                                       ;;梅
     :d        '(14 13)                                     ;;方
     ;; 按牌的张数分类， 牌从大到小排序, 德州扑克不按花色区分大小，
     :4        '(4)                                         ;;4同
     :3        '(3 2)                                       ;;3同
     :2        '(7 6)                                       ;;对子
     :1        '(10 9 8 7 6 5 4 3)}                         ;;单张
    ))

(defn sort-by-num
  "根据牌面数值大小排序"
  [compare-f pais]
  (sort #(compare-f (last %1) (last %2)) pais))

(defn filter-cards-by-count
  "根据牌的张数过滤出牌"
  [grouped-by-number t-count]
  (let [result (->> (filter #(= t-count (count (last %))) grouped-by-number)
                    (mapcat #(distinct (last %))))]
    (when-not (empty? result)
      {(keyword (str t-count)) result})))

(defn cards-group
  "根据花色和张数分类"
  [original-cards type-fn]
  (->> original-cards
       (sort-by-num >)
       (group-by type-fn)
       (map (fn [[k v]] {k (map last v)}))
       (apply merge)))

(defn cards-classify
  "牌分类，根据花色和牌面数值对牌进行大体分类"
  [original-cards]
  (let [grouped-by-color (cards-group original-cards first)
        grouped-by-number (cards-group original-cards last)
        count-four (filter-cards-by-count grouped-by-number 4)
        count-three (filter-cards-by-count grouped-by-number 3)
        count-two (filter-cards-by-count grouped-by-number 2)
        count-one (filter-cards-by-count grouped-by-number 1)]
    (merge {:original (sort-by-num > original-cards)} grouped-by-color count-four count-three count-two count-one)))

(defn convert-cards
  "牌型显示形式与程序内部表现形式转换"
  [cards-dict cards]
  (mapv #(assoc % 1 (cards-dict (second %))) cards))

(defn cards-add-color
  "为牌添上花色"
  [nums color]
  (mapv #(do [color %]) nums))

(defn cards-by-num
  "根据牌面数值和所需张数过滤出牌列表"
  [origianl-pais num count]
  (->> (filter #(= num (last %)) origianl-pais)
       (take count)
       vec))

(defn max-straight-nums
  "取得最大的连牌, sorted-nums为从大到小排列的牌面值
  n为要获取的连牌张数
  没有符合要求的，则返回nil"
  [sorted-nums n]
  (let [result (reduce (fn [r val]
                         (cond
                           (= (count r) n) r
                           (= (last r) (inc val)) (conj r val)
                           :default [val]))
                       [] sorted-nums)]
    (when (= n (count result))
      result)))

;;===============================
;; 牌型判断

(defn flush-type-handler
  "同花类型的牌的处理
  check-fn接受两个参数：classified-cards 和 color
  如果有多个花色都满足check-fn要求，则返回拥有最大牌面数值的那一色牌"
  [classified-cards type-id check-fn]
  (let [type-cards (->> (map #(check-fn classified-cards %) [:s :h :c :d])
                        (filter #(not (empty? %))))
        [color nums] (when-not (empty? type-cards)
                       (reduce (fn [r val]
                                 (if (< (first (last r)) (first (last val)))
                                   val
                                   r))
                               type-cards))]
    (when color
      [type-id (cards-add-color nums color)])))

(defn royal-flush-filter
  "过滤出皇家同花顺"
  [classified-cards]
  (let [check-fn (fn [classified-cards color]
                   (when-let [max-straight
                              (max-straight-nums (get classified-cards color) 5)]
                     (when (= [14 13 12 11 10] max-straight)
                       [color max-straight])))]
    (flush-type-handler classified-cards :royal-flush check-fn)))

(defn straight-flush-filter
  "过滤出最大的同花顺"
  [classified-cards]
  (let [check-fn (fn [classified-cards color]
                   (when-let [straight-cards (max-straight-nums (get classified-cards color) 5)]
                     [color straight-cards]))]
    (flush-type-handler classified-cards :straight-flush check-fn)))

(defn flush-filter
  "过滤出最大的同花"
  [classified-cards]
  (let [check-fn (fn [classified-cards color]
                   (when-let [flush-nums (take 5 (get classified-cards color))]
                     (when (= 5 (count flush-nums))
                       [color flush-nums])))]
    (flush-type-handler classified-cards :flush check-fn)))


(defn four-of-a-kind-filter
  "过滤出最大的四带一"
  [classified-cards]
  (let [max-four-num (first (:4 classified-cards))
        max-one-card (->> (:original classified-cards)
                          (filter #(not= max-four-num (last %)))
                          first)]
    (when max-four-num
      [:4-of-a-kind [[:s max-four-num] [:c max-four-num] [:h max-four-num] [:d max-four-num] max-one-card]])))

(defn full-house-filter
  "过滤出最大的葫芦"
  [classified-cards]
  (let [three (:3 classified-cards)
        max-three (first three)
        second-three (second three)
        first-two (first (:2 classified-cards))
        max-two (if (and second-three first-two)
                  (max second-three first-two)
                  (or second-three first-two))
        original-cards (:original classified-cards)]
    (when (and max-three max-two)
      [:full-house (into (cards-by-num original-cards max-three 3)
                         (cards-by-num original-cards max-two 2))])))

(defn straight-filter
  "过滤出最大的顺子"
  [classified-cards]
  (let [distinct-nums (sort > (reduce #(into %1 (get classified-cards %2)) [] [:4 :3 :2 :1]))
        max-straight (max-straight-nums distinct-nums 5)
        original-cards (:original classified-cards)]
    (when max-straight
      [:straight (vec (mapcat #(cards-by-num original-cards % 1) max-straight))])))

(defn three-of-a-kind-filter
  "过滤出最大的三同"
  [classified-cards]
  (let [max-three (first (:3 classified-cards))
        max-two-nums (take 2 (:1 classified-cards))
        original-cards (:original classified-cards)]
    (when max-three
      [:three-of-a-kind (into (cards-by-num original-cards max-three 3)
                              (mapcat #(cards-by-num original-cards % 1) max-two-nums))])))

(defn two-pairs-filter
  "过滤出最大的两对"
  [classified-cards]
  (let [pairs (:2 classified-cards)
        max-two-pairs (take 2 pairs)
        ; 对子中（除去已被最大的两对），剩下最大对子
        next-two (nth pairs 2 nil)
        ; 单牌中，最大的单牌
        max-one (first (:1 classified-cards))
        ; 最大的牌
        max-single-num (if (and max-one next-two)
                         (max next-two max-one)
                         (or max-one next-two))
        original-cards (:original classified-cards)]
    (when (= 2 (count max-two-pairs))
      [:two-pairs (-> (mapcat #(cards-by-num original-cards % 2) max-two-pairs)
                      vec
                      (into (cards-by-num original-cards max-single-num 1)))])))

(defn one-pair-filter
  "过滤出最大的一对"
  [classified-cards]
  (let [max-pair (first (:2 classified-cards))
        max-three-singles (take 3 (:1 classified-cards))
        original-cards (:original classified-cards)]
    (when (and max-pair max-three-singles)
      [:one-pair (into (cards-by-num original-cards max-pair 2)
                       (mapcat #(cards-by-num original-cards % 1) max-three-singles))])))

(defn high-card-filter
  "过滤出最大的高张"
  [classified-cards]
  [:high-card (take 5 (:original classified-cards))])

;;==========================================
;; 过滤器组装

(defn mk-filter
  "构建一个filter"
  [filter-fn]
  (fn [[type cards]]
    (if type
      [type cards]
      (if-let [type-card (filter-fn cards)]
        type-card
        [nil cards]))))

(defn comp-filters
  "构建大筛子，从大到小牌型进行组装"
  [classified-cards]
  (->> [nil classified-cards]
       ((mk-filter royal-flush-filter))
       ((mk-filter straight-flush-filter))
       ((mk-filter four-of-a-kind-filter))
       ((mk-filter full-house-filter))
       ((mk-filter flush-filter))
       ((mk-filter straight-filter))
       ((mk-filter three-of-a-kind-filter))
       ((mk-filter two-pairs-filter))
       ((mk-filter one-pair-filter))
       ((mk-filter high-card-filter))))

;;=============================
(defn cards-type
  "入口，找出德州扑克7中牌中最大的牌型"
  [cards]
  (let [[type resp-cards] (->> cards
                               (convert-cards {:2 2 :3 3 :4 4 :5 5 :6 6 :7 7 :8 8 :9 9 :t 10 :j 11 :q 12 :k 13 :a 14})
                               cards-classify
                               comp-filters)]
    [type (convert-cards {2 :2 3 :3 4 :4 5 :5 6 :6 7 :7 8 :8 9 :9 10 :t 11 :j 12 :q 13 :k 14 :a} resp-cards)]))
