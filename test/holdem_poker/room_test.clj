(ns holdem-poker.room-test
  (:require [holdem-poker.room :refer :all]
            [clojure.test :refer :all]))

(def room-for-test
  {; 房间名称
   :name "001"
   :capacity  3                                          ;房间容量
   ; 房间中的座位, 按座位号从小到大排序
   :seats     {0 {:username "username1" :position 0 :status :normal}
               1 {:username "username2" :position 1 :status :ready}}})

(deftest user-seat-test
  (is (= (user-seat room-for-test "username1") {:username "username1" :position 0 :status :normal})))

(deftest position-seat-test
  (is (= (position-seat room-for-test 1) {:username "username2" :position 1 :status :ready})))

(deftest enter-seat-test
  (is (= (enter-seat room-for-test "username3" 1) (assoc-in room-for-test [:seats 1] {:username "username3" :position 1 :status :normal})))
  (is (= (enter-seat room-for-test "username3" 2) (assoc-in room-for-test [:seats 2] {:username "username3" :position 2 :status :normal}))))

(deftest room-users-test
  (is (= (room-users room-for-test) ["username1" "username2"])))

(deftest str->num-test
  (is (= (str->num "001") 1))
  (is (= (str->num "01") 1))
  (is (= (str->num "010") 10))
  (is (= (str->num "10009") 10009)))

(deftest format-room-name-test
  (is (= (format-room-name 1) "001"))
  (is (= (format-room-name 10) "010")))

(deftest new-room-name-test
  (is (= (new-room-name ["001" "002" "003"]) "004")))

(deftest all-user-ready?-test
  (is (= (all-user-ready? room-for-test) false))
  (is (= (all-user-ready? (assoc-in room-for-test [:seats 0 :status] :ready)) true)))

(deftest user-ready-test
  (is (= (user-ready room-for-test "username1") (assoc-in room-for-test [:seats 0 :status] :ready))))

(deftest find-vacant-position-test
  (is (= (find-vacant-position room-for-test) 2)))

(deftest can-join?-test
  (is (= (can-join? room-for-test "username1" 2) false))
  (is (= (can-join? room-for-test "username1" 0) false))
  (is (= (can-join? room-for-test "username3" 2) true)))

(deftest join-room-test
  (is (= (join-room room-for-test "username1") room-for-test))
  (is (= (join-room room-for-test "username3")
         (assoc-in room-for-test [:seats 2] {:username "username3" :position 2 :status :normal}))))

(deftest leave-room-test
  (is (= (leave-room room-for-test "username1") (update-in room-for-test [:seats] dissoc 0))))