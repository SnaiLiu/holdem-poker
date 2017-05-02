(ns holdem-poker.utils)

(def find-first
  "返回序列中第一个符合要求的元素"
  (comp first filter))

(defn desc
  "将一个 comparator 函数变为降序"
  ([]
   (desc compare))
  ([f]
   (comp (partial * -1) f)))
