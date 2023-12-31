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

(deftest local-date-to-long-codec-test
  (is (= 20231230 (tu/local-date->ymd-long (t/date "2023-12-30"))))
  (is (= (t/date "2023-12-30") (tu/ymd-long->local-date 20231230))))

(deftest time-test
  (is (tu/time? (t/time)))
  (is (not (tu/time? (t/now))))
  (is (not (tu/time? (t/inst)))))

(deftest offset-test
  (is (= #time/offset "nil PT5H" (tu/offset 5 :hours))))

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
      (and
        (= (count in-month) (.lengthOfMonth (t/year-month d)))
        (= last-in-month (last in-month))))))

(comment
  (t/range (tu/->date (t/year-month)) (t/today) (tu/period 1 :days))
  (.lengthOfMonth (t/year-month (t/today)))
  (tu/last-day-of-month (last (tu/dates-in-month)))
  )

(defn monday-week? [week]
  (and
    (= t/MONDAY (t/day-of-week (first week)))
    (= t/SUNDAY (t/day-of-week (last week)))
    (= (count week) 7)))

(def monday-week-check
  (prop/for-all [d gen-date]
    (let [week (tu/monday-week d)]
      (monday-week? week))))

(comment (test.check/quick-check 1000 monday-week-check))

(defspec monday-week-spec 100 monday-week-check)

(defn sunday-week? [week]
  (and
    (= t/SUNDAY (t/day-of-week (first week)))
    (= t/SATURDAY (t/day-of-week (last week)))
    (= (count week) 7)))

(def sunday-week-check
  (prop/for-all [d gen-date]
    (let [week (tu/sunday-week d)]
      (sunday-week? week))))

(comment (test.check/quick-check 1000 sunday-week-check))
