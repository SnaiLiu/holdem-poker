(ns holdem-poker.play-proccess-test
  "打牌过程测试"
  (:require [clojure.test :refer :all]
            [holdem-poker.play-proccess :refer :all]))

(def game (new-game (vec (repeat 3 1000)) (init-cards)))

(deftest advance-game-test
  (are [rslt init-game ops]
    (= rslt (-> (advance-game init-game ops) :winners set))
    ;简单游戏过程
    #{[2 300]} game
    [[:blind 1 100] [:blind 2 200] [:fold 0] [:fold 1]]

    ;简单摊牌过程
    #{[1 2200]} game
    [[:blind 1 100] [:blind 2 200] [:bet 0 200] [:all-in 1] [:all-in 2] [:fold 0]]

    #{[0 3000]} game
    [[:bet 1 100] [:bet 2 200] [:bet 0 200] [:all-in 1] [:all-in 2] [:all-in 0]]

    ;分池摊牌
    #{[0 1000] [1 1000]}
    (new-game [1000 1000] [{:card [:s :a]} {:card [:h :a]} {:card [:s :k]} {:card [:h :k]}
                           {:card [:c :2]} {:card [:c :5]} {:card [:c :6]} {:card [:c :7]}
                           {:card [:c :8]} {:card [:c :9]} {:card [:c :t]} {:card [:c :j]}])
    [[:all-in 1] [:all-in 0]]
    ;
    ;多边池摊牌
    #{[1 4000] [2 1000]}
    (new-game [1000 2000 3000] (init-cards))
    [[:all-in 1] [:all-in 2] [:fold 0]]

    #{[1 200] [0 200] [0 300]}
    (new-game [400 100 200] [{:card [:s :a]} {:card [:h :q]} {:card [:c :2]} {:card [:s :k]}
                             {:card [:c :q]} {:card [:c :6]} {:card [:h :7]}
                             {:card [:s :q]} {:card [:s :j]} {:card [:s :t]} {:card [:h :6]}
                             {:card [:h :j]} {:card [:h :5]} {:card [:c :7]}])
    [[:all-in 1] [:all-in 2] [:all-in 0]]

    ;简单翻牌
    #{[0 600]} game
    [[:blind 1 100] [:blind 2 200] [:call 0] [:call 1] [:check 2]
     [:check 1] [:check 2] [:check 0]
     [:check 1] [:check 2] [:check 0]
     [:check 1] [:check 2] [:check 0]
     ]

    ;过滤无效位置操作和游戏结束后操作
    #{[2 300]} game
    [[:bet 0 100] [:bet 1 100] [:bet 2 200] [:fold 2] [:fold 0] [:fold 1] [:bet 2 200]]

    ; Sample from Wiki
    #{[1 26]} (new-game [100 100 100 100]
                        [{:card [:h :k]} {:card [:s :q]} {:card [:s :k]} {:card [:h :a]} {:card [:s :a]}
                         {:card [:h :9]} {:card [:h :j]} {:card [:d :k]} {:card nil}
                         {:card [:c :9]} {:card [:c :k]} {:card [:h :3]} {:card nil}
                         {:card [:s :5]} {:card nil} {:card [:d :9]}]
                        #_(mk-cards "HK SQ SK HA SA H9 HJ DK nil C9 CK H3 nil S5 nil D9"))
    [[:blind 1 1] [:blind 2 2] [:fold 3] [:call 0] [:call 1] [:check 2]
     [:check 1] [:bet 2 2] [:bet 0 4] [:call 1] [:call 2]
     [:check 1] [:check 2] [:check 0]
     [:bet 1 4] [:call 2] [:fold 0]]))



