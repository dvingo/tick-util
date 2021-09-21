(ns dv.tick-util-test
  (:require
    [clojure.test :refer [deftest is testing run-tests]]
    [dv.tick-util :as tu]
    [tick.core :as t]))

(deftest my-test
  (println "hello")
  (is (= 100 100)))

(deftest time-test
  (is (tu/time? (t/time)))
  (is (not (tu/time? (t/now))))
  (is (not (tu/time? (t/inst)))))

(deftest offset-test
  (is (= #time/offset "nil PT5H") (tu/offset 5 :hours)))
