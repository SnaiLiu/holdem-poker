(ns holdem-poker.cards-type-compare
  "牌型评分单元，对于输入的每个牌型进行评分，输出该牌型的分值")

(defn type-score
  "外部调用接口
  输入：cards-type: [:two-pairs [[:d :8] [:h :8] [:s :7] [:c :7] [:h :a]]]
  输出：牌型对应分值
  cards-type中，牌的排列顺序：四张>三张>两张>单张，同一类牌，面值大的在前，如上示例
  算法概述：用一个用数字组成的字符串表示评分，前两位表示牌型的分数，后续每两位表示第一张、
  第二张、第三张...的数值，(字典排序同样可比较分值大小)"
  [[type cards]]
  (let [score-standard {:royal-flush     "19"
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
                        :a               "14"}
        type-score (score-standard type)]
    (reduce #(str %1 (score-standard (last %2))) type-score cards)))

(defn type-compare
  "牌型比较
  cards-types: [{:cards-type [:one-pair [:one-pair [[:h :7] [:c :7] [:h :a] [:d :9] [:h :8]]]
                 :other-keys other-vals}
                 ...]

  map结构中:cards-type是必带的一个kv, 其他k-v可携带其他信息，本函数不会破坏map结构，只是根据map中的牌型评分、排序
  输出：从小到大输出"
  [cards-types]
  (let [a-fn (fn [position-cards-type]
               (let [cards-type (:cards-type position-cards-type)
                     score (type-score cards-type)]
                 [score position-cards-type]))]
    (->> (map a-fn cards-types)
         (group-by first)
         (map (fn [[k v]] {k (map last v)}))
         (apply merge)
         (sort-by first))))

(defn max-cards-type
  "取出对大的牌型，可能有多个，用[cards-type1 cards-type2]表示"
  [cards-types]
  (->> (type-compare cards-types)
       last
       last))
