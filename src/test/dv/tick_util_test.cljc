(ns dv.tick-util-test
  (:require
    [cljc.java-time.instant :as instant]
    [clojure.test :refer [deftest is testing run-tests]]
    [clojure.test.check :as test.check]
    [clojure.test.check.clojure-test :refer [defspec]]
    [clojure.test.check.generators :as gen]
    [clojure.test.check.properties :as prop]
    [dv.tick-util :as tu]
    [tick.alpha.interval :as t.i]
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

(deftest last-day-of-month-test
  (is (= #time/month "JANUARY"
        (t/month (tu/last-day-of-month (t/date "2022-01-01"))))))

(defn clamp [min max x] (long (Math/min (double max) (Math/max (double min) (double x)))))
(def max-instant-s (instant/get-epoch-second instant/max))
(def min-instant-s (- max-instant-s))

(def gen-date
  "A generator for local dates."
  (gen/fmap
    (comp t/date instant/of-epoch-milli (partial clamp min-instant-s max-instant-s) #(Math/floor %) #(* % (Math/random) 1e12) #(if (zero? %) 1 %) #(Math/abs %))
    gen/large-integer))

(def last-of-month-in-the-month
  (prop/for-all [d gen-date]
    (let [month     (t/month d)
          last-date (tu/last-day-of-month d)]
      (= month (t/month last-date)))))

(defspec last-of-month-in-the-month-spec 1000 last-of-month-in-the-month)

(comment
  (test.check/quick-check 10000 last-of-month-in-the-month)
  (test.check/quick-check 1000
    (prop/for-all [d gen-date]
      (let [last-in-month (tu/last-day-of-month d)
            in-month      (tu/dates-in-month d)]
        (= last-in-month (last in-month)))))
  (gen/generate gen-date)
  (gen/sample gen-date))

(defspec dates-in-month-spec 100
  (prop/for-all [d gen-date]
    (let [last-in-month (tu/last-day-of-month d)
          in-month      (tu/dates-in-month d)]
      (println "inmonth: " in-month)
      (and
        (= (count in-month) (.lengthOfMonth (t/year-month d)))
        (= last-in-month (last in-month))))))

(comment
  (t/range (tu/->date (t/year-month)) (t/today) (tu/period 1 :days))
  (.lengthOfMonth (t/year-month (t/today)))
  (tu/last-day-of-month (last (tu/dates-in-month)))
  )
