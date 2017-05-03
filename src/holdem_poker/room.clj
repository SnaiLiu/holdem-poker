(ns holdem-poker.room
  "房间管理")

(comment
  ;; 房间的数据结构
  (def room
    {; 房间名称
     :name "room-name"
     :capacity  10                                          ;房间容量
     ; 房间中的座位, 按座位号从小到大排序
     :seats     {0 {:username "username1" :position 0 :status :normal}
                 1 {:username "username2" :position 1 :status :ready}}}))

(defn room-capacity
  "房间容量"
  [room]
  (:capacity room))

(defn new-seat
  "新建一个座位"
  [username position]
  {:username username
   :position position
   :status   :normal})

(defn user-seat
  "查找玩家座位"
  [room username]
  (->> (:seats room)
       vals
       (filter #(= username (:username %)))
       first))

(defn position-seat
  "查找某个座位号对应的座位"
  [room position]
  (get-in room [:seats position]))

(defn enter-seat
  "玩家入座"
  [room username position]
  (->> (new-seat username position)
       (assoc-in room [:seats position])))

(defn username-in-position
  "查找某个座位上的玩家名称"
  [room position]
  (->> (position-seat room position)
       :username))

(defn room-users
  "房间内所有玩家的名字"
  [room]
  (->> (:seats room)
       vals
       (mapv :username)))

(defn str->num
  "数字的字符串转为数值形式"
  [num-str]
  (Integer/parseInt (re-find #"[1-9]\d*" num-str)))

(defn format-room-name
  "格式化房间名称"
  [room-num]
  (if (>= room-num 1000)
    (str room-num)
    (format "%03d" room-num)))

(defn new-room-name
  "获得新的房间名称"
  [exist-room-names]
  (let [max-room-name (if exist-room-names
                        (->> exist-room-names
                             (mapv str->num)
                             sort
                             last)
                        1)]
    (format-room-name (inc max-room-name))))

(defn create-room
  "创建新的房间"
  [room-name capacity]
  {:name room-name
   :capacity capacity
   :seats {}})

(defn all-user-ready?
  "玩家是否全部都已准备"
  [{:keys [seats] :as room}]
  (->> seats
       vals
       (filter #(not (empty? (:username %))))
       (every? #(= :ready (:status %)))))

(defn user-ready
  "用户准备游戏"
  [room username]
  (let [{:keys [position]} (user-seat room username)]
    (assoc-in room [:seats position :status] :ready)))

(defn position-range
  "房间内的所有座位号"
  [room]
  (range (room-capacity room)))

(defn find-vacant-position
  "查找空的座位"
  [room]
  (some #(when-not (username-in-position room %) %)
        (position-range room)))

(defn can-join?
  "是否可以加入房间"
  [room username vacant-position]
  (let [user-seat (user-seat room username)]
    (and (not (nil? vacant-position))
         (empty? user-seat))))

(defn join-room
  "用户加入房间"
  [room username]
  (let [vacant-position (find-vacant-position room)]
    (if (can-join? room username vacant-position)
      (enter-seat room username vacant-position)
      room)))

(defn leave-room
  "用户离开房间"
  [room username]
  (if-let [{:keys [position]} (user-seat room username)]
    (update-in room [:seats] dissoc position)
    room))