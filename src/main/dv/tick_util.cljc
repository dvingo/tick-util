(ns dv.tick-util
  (:refer-clojure :exclude [< > >= <= + - format time])
  #?(:cljs (:require-macros [dv.tick-util]))
  (:require
    #?(:cljs [cljs.reader] :clj [clojure.edn])
    #?(:cljs [java.time :refer
              [Period LocalDate LocalDateTime ZonedDateTime OffsetTime
               Instant OffsetDateTime ZoneId DayOfWeek
               LocalTime Month Duration Year YearMonth]])
    [clojure.set :as set]
    [clojure.spec.alpha :as s]
    [clojure.string :as str]
    [cognitect.transit :as tr]
    [com.fulcrologic.guardrails.core :refer [>defn >def | => ?]]
    [tick.core :as t]
    [tick.alpha.interval :as t.i]
    [tick.protocols :refer [ITimeComparison ITimeArithmetic]]
    [tick.locale-en-us]
    [time-literals.read-write :as rw]
    #?(:clj [taoensso.nippy :as nippy])
    [taoensso.timbre :as log])
  #?(:clj (:import
            [java.io ByteArrayInputStream ByteArrayOutputStream Writer]
            [java.time Period LocalDate LocalDateTime ZonedDateTime OffsetTime Instant
                       OffsetDateTime ZoneId DayOfWeek LocalTime Month Duration Year YearMonth])))

#?(:clj
   (alter-var-root (var nippy/*thaw-serializable-allowlist*)
     (fn [c] (conj c "java.time.*"))))

;;; Docs for date time types:

;;; https://js-joda.github.io/js-joda/manual/LocalDate.html
;;; reference: https://js-joda.github.io/js-joda/identifiers.html
;;; https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html
;;;

(defn error [& msg]
  #?(:cljs (js/Error. (apply str msg))
     :clj  (RuntimeException. (apply str msg))))

(defn throw* [& args]
  (throw (apply error args)))

(def Date LocalDate)
(def DateTime LocalDateTime)
(def Time LocalTime)
;; Tick types

(comment (.from YearMonth (t/today))
  (t/at (.atDay (YearMonth/from (t/today)) 1) (t/midnight))
  )


;Duration
;Period

;LocalDate
;LocalTime
;LocalDateTime

;ZonedDateTime
;ZoneId

;Instant

;OffsetTime
;OffsetDateTime

;DayOfWeek
;Month
;MonthDay
;YearMonth
;Year

(comment
  (t/parse "2pm")
  (t/parse "14")
  (t/parse "2020-03-02T00:12:10"))

(defn time? [d] (instance? LocalTime d))
(comment (time? (t/time)))

(defn instant? [d] (instance? Instant d))
(comment (instant? (t/instant)))

(defn date? [d] (instance? LocalDate d))
(comment (date? (t/date)))

(defn date-time? [d] (instance? LocalDateTime d))

(comment (date-time? (t/date))
  (date-time? (. java.time.LocalDateTime parse "2020-05-11T00:00"))

  (date-time? (t/date-time)))

(defn duration? [d] (instance? Duration d))
(comment (duration? (t/new-duration 20 :minutes)))

(defn period? [p] (instance? Period p))
(comment (period? (t/new-period 1 :weeks))
  (period? (. java.time.Period parse "P1D"))
  (period? (t/date)))

(comment (time-type? (t/now)))

(declare offset + - add-offset subtract-offset offset-type? -period -duration)

(def date-type? (some-fn inst? instant? date? date-time?))
(def time-type? (some-fn inst? instant? time? date? date-time?))


;; Combines duration and period into one abstration
;; Todo I should lock down the semantics for this - the intention is that the duration is always less than 24 hours
;; and the period is at least one day.

#?(:cljs
   (deftype Offset [period duration _meta]
     IMeta
     (-meta [_] _meta)
     IWithMeta
     (-with-meta [_ _m] (Offset. period duration _m))

     IEquiv
     (-equiv [this other]
       (and
         (= (type this) (type other))
         (= (.-period this) (-period other))
         (= (.-duration this) (-duration other))))
     ITimeArithmetic
     (+ [offset other] (add-offset offset other))
     (- [offset other] (subtract-offset offset other)))
   :clj
   (deftype Offset [period duration _meta]
     clojure.lang.IObj
     (meta [_] _meta)
     (withMeta [_ _m] (Offset. period duration _m))
     Object
     (equals [this other]
       (and
         (= (type this) (type other))
         (= (.-period this) (.-period other))
         (= (.-duration this) (.-duration other))))

     ITimeArithmetic
     (+ [offset other] (add-offset offset other))
     (- [offset other] (subtract-offset offset other))))

;; If using Offset in clj instead of Object I would get the following error in certain use cases at the repl:
;; Error printing return value (ClassCastException) at dv.tick-util/-duration (tick_util.cljc:131).
;; class dv.tick_util.Offset cannot be cast to class dv.tick_util.Offset
;; (dv.tick_util.Offset is in unnamed module of loader clojure.lang.DynamicClassLoader @62b15496
(comment
  (offset 5 :hours)
  )

(defn -period [offset] (.-period ^{:tag #?(:cljs clj :clj Object)} offset))
(defn -duration [offset] (.-duration ^{:tag #?(:cljs clj :clj Object)} offset))

(defn offset? [d] (instance? Offset d))
(def offset-type? (some-fn offset? duration? period?))

(>defn period-duration-pair
  "Takes either order of period/duration return [period duration]
  with nils for either missing"
  [v1 v2]
  [::period-or-duration ::period-or-duration => (s/nilable offset?)]
  (cond
    (and (duration? v1) (or (period? v2) (nil? v2))) [v2 v1]
    (and (or (period? v1) (nil? v1)) (duration? v2)) [v1 v2]

    (and (or (duration? v1) (nil? v1)) (period? v2)) [v2 v1]
    (and (period? v1) (or (duration? v2) (nil? v2))) [v1 v2]
    :else nil))

(>defn add-offset*
  [d ^Offset offset]
  [time-type? offset? => time-type?]
  (let [duration (-duration offset)
        period   (-period offset)]
    (cond-> d
      duration (t/+ duration)
      period (t/+ period))))

(comment
  (-duration (offset 2 :days 5 :minutes))
  (t/+ (t/date-time) #time/duration "PT5M")
  (t/+
    (t/date)
    (-duration (offset 2 :days 5 :minutes)))
  (add-offset*
    (t/date) (offset 2 :days 5 :minutes)))


(>defn add-offset
  [v1 v2]
  [(s/or :time time-type? :offset offset-type?)
   (s/or :time time-type? :offset offset-type?)
   => (s/or :time time-type? :offset offset-type?)]
  (cond
    (and (time-type? v1) (offset? v2))
    (add-offset* v1 v2)

    (and (offset? v1) (time-type? v2))
    (add-offset* v2 v1)

    (and (offset? v1) (period? v2))
    (offset (t/+ (-period v1) v2) (-duration v1))

    (and (period? v1) (offset? v2))
    (offset (t/+ (-period v2) v1) (-duration v2))

    (and (offset? v1) (duration? v2))
    (offset (-period v1)
      (if (-duration v1) (t/+ (-duration v1) v2) v2))

    (and (duration? v1) (offset? v2))
    (offset (-period v2)
      (if (-duration v2) (t/+ (-duration v2) v1) v1))

    (and (offset? v1) (offset? v2))
    (offset
      (+ (-period v1) (-period v2))
      (+ (-duration v1) (-duration v2)))))

(comment
  (add-offset (t/date) (offset 2 :days 5 :minutes))
  (add-offset (offset 2 :days 5 :minutes) (offset 2 :days 5 :minutes))
  (t/+ (duration 5 :minutes) (offset (t/new-duration 25 :minutes)))
  (t/+ (t/now) (offset (t/new-duration 25 :minutes)))
  (t/+ (t/now) (t/new-duration 25 :minutes)))

(>defn subtract-offset*
  [d ^Offset offset]
  [time-type? offset? => time-type?]
  (let [duration (-duration offset)
        period   (-period offset)]
    (cond-> d
      duration (t/- duration)
      period (t/- period))))

(>defn subtract-offset
  [v1 v2]
  [(s/or :time time-type? :offset offset-type?)
   (s/or :time time-type? :offset offset-type?)
   => (s/or :time time-type? :offset offset-type?)]
  (cond
    (and (time-type? v1) (offset? v2))
    (subtract-offset* v1 v2)

    (and (offset? v1) (time-type? v2))
    (subtract-offset* v2 v1)

    (and (offset? v1) (period? v2))
    (offset (t/- (-period v1) v2) (-duration v1))

    (and (period? v1) (offset? v2))
    (offset (t/- (-period v2) v1) (-duration v2))

    (and (offset? v1) (duration? v2))
    (offset (-period v1) (t/- (-duration v1) v2))

    (and (duration? v1) (offset? v2))
    (offset (-period v2) (t/- (-duration v2) v1))

    (and (offset? v1) (offset? v2))
    (offset
      (- (-period v1) (-period v2))
      (- (-duration v1) (-duration v2)))))

(extend-protocol ITimeArithmetic
  #?(:clj Object :cljs object)
  (+ [t d]
    (cond
      (nil? d) t
      (nil? t) d
      (offset? d)
      (add-offset t d)
      :else
      (.plus ^{:tag #?(:cljs clj :clj Object)} t d)))
  (- [t d]
    (cond
      (nil? d) t
      (nil? t) d
      (offset? d)
      (subtract-offset t d)
      :else
      (.minus ^{:tag #?(:cljs clj :clj Object)} t d))))

#?(:clj
   (nippy/extend-freeze Offset :dv.tick-util/offset
     [x data-output]
     (nippy/freeze-to-out! data-output (.-period x))
     (nippy/freeze-to-out! data-output (.-duration x))))

#?(:clj
   (nippy/extend-thaw :dv.tick-util/offset
     [data-input]
     (let [period   (nippy/thaw-from-in! data-input)
           duration (nippy/thaw-from-in! data-input)]
       (Offset. period duration nil))))


(comment (offset? (->Offset (t/new-period 1 :days) (t/new-duration 1 :hours) nil)))

(comment
  (nippy/thaw (nippy/freeze (offset 1 :hours 1 :days)))
  (nippy/thaw (nippy/freeze (t/new-period 1 :days)))
  (nippy/thaw (nippy/freeze nil))

  (offset 1 :days 2 :hours)
  (.-period (offset 1 :minutes 2 :weeks))
  (.-period (->Offset (t/new-period 1 :weeks) nil))
  (->Offset (t/new-period 1 :weeks) nil)
  (offset 1 :seconds 2 :weeks)
  (offset? (->Offset nil nil))
  )

;; Todo could extend this protocol to offset
;; I'm not sure what semantics I want for offset : adding period and duration or just delegating to each one?
;(extend-protocol ITimeLength
;  Duration
;  (nanos [d] (.toNanos d))
;  (micros [d] (#?(:clj Long/divideUnsigned :cljs cljs.core//) (nanos d) 1000))
;  (millis [d] (.toMillis d))
;  (seconds [d] (cljc.java-time.duration/get-seconds d))
;  (minutes [d] (.toMinutes d))
;  (hours [d] (.toHours d))
;  (days [d] (.toDays d))
;
;  Period
;  (days [p] (cljc.java-time.period/get-days p))
;  (months [p] (cljc.java-time.period/get-months p))
;  (years [p] (cljc.java-time.period/get-years p)))

;(= (->Offset (t/new-period 1 :days) (t/new-duration 1 :hours)) (->Offset (t/new-period 1 :days) (t/new-duration 1 :hours)))


(def period-units #{:days :weeks :months :years})
(def duration-units #{:nanos :micros :millis :seconds :minutes :hours})
(def offset-units (set/union period-units duration-units))
(def offset-units? offset-units)
(def period-units? period-units)
(def duration-units? duration-units)

(s/def ::opt-map (s/* (s/cat :k keyword? :v any?)))

(def plus-period-fns
  {:days   (fn [d v] (.plusDays #?(:cljs ^js d :clj d) v))
   :months (fn [d v] (.plusMonths #?(:cljs ^js d :clj d) v))
   :years  (fn [d v] (.plusYears #?(:cljs ^js d :clj d) v))})

;; (offset 1 :days (t/new-duration 20 :minutes))
;; (offset (t/new-period 1 :days) (t/new-duration 20 :minutes))
;; (offset (t/new-duration 20 :minutes) (t/new-period 1 :days))
;; etc
;; (offset (t/new-period 1 :days) 20 :minutes)

(>defn offset
  "Offset from mix and match units of duration and period"
  ([val]
   [(s/or :period period? :duration duration?) => offset?]
   (cond
     (period? val) (->Offset val nil nil)
     (duration? val) (->Offset nil val nil)
     :else (throw (error "Unsupported type passed to offset: " (pr-str val)))))

  ([val units]
   [(s/or :int integer? :period period? :duration duration? :nil nil?)
    (s/or :units offset-units? :period period? :duration duration? :nil nil?) => offset?]
   (cond
     (and (duration? val) (nil? units)) (->Offset units val nil)
     (and (period? val) (nil? units)) (->Offset val units nil)
     (and (nil? val) (period? units)) (->Offset units val nil)
     (and (nil? val) (duration? units)) (->Offset val units nil)
     (and (period? val) (duration? units)) (->Offset val units nil)
     (and (duration? val) (period? units)) (->Offset units val nil)
     (and (integer? val) (duration-units? units)) (->Offset nil (t/new-duration val units) nil)
     (and (integer? val) (period-units? units)) (->Offset (t/new-period val units) nil nil)
     :else (throw (error (str "Unknown units passed to offset: " units)))))

  ([val units val2 units2]
   [integer? offset-units? integer? offset-units? => offset?]
   (let [duration (cond
                    (duration-units? units) (t/new-duration val units)
                    (duration-units? units2) (t/new-duration val2 units2))
         period   (cond
                    (period-units? units) (t/new-period val units)
                    (period-units? units2) (t/new-period val2 units2))]
     (when (and (nil? duration) (nil? period))
       (throw (error (str "Unknown units passed to offset: " units " and " units2))))
     (->Offset period duration nil))))

(declare ->date)

(>defn period-seq
  "Starting at 'start' - a tick/date, return lazy seq of dates every `period` units.
  Defaults to today if no date passed."
  ([period] [period? => seq?] (period-seq period (t/today)))

  ([period start]
   [period? date-type? => seq?]
   (iterate #(t/+ % period) (->date start))))

(comment (take 10 (period-seq (t/new-period 7 :days) (t/date "2020-03-15")))
  ;; similar to range:
  (take 10 (t/range (t/date-time "2020-03-15T00:00") nil (t/new-period 7 :days))))

(>defn dates-in-year-every-period
  "Return a seq of all dates within the year of the "
  [period date]
  [period? date-type? => seq?]
  (if (period? period)
    (let [date (->date date)
          year (t/year date)]
      (take-while #(= year (t/year %)) (period-seq period date)))))

(comment
  (dates-in-year-every-period (period 2 :weeks) (start-of-year))
  (dates-in-year-every-period (period 2 :weeks) (t/today)))

(def days-seq
  "Returns an infinite lazy seq of dates starting at the passed in date"
  (partial period-seq (t/new-period 1 :days)))
(comment (take 10 (days-seq (t/today))))

(defn interval-seq
  "Returns lazy seq of intervals starting at `start` separated by `period`."
  [period start]
  (let [start-intvl (t.i/new-interval start (t/+ start period))]
    (iterate #(t/>> % period) start-intvl)))

(def week-intvl-seq (partial interval-seq (t/new-period 7 :days)))
(comment (take 5 (week-intvl-seq (t/date "2020-03-15"))))

(comment (take 10 (interval-seq (t/new-period 7 :days) (t/date "2020-03-15"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type conversion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ->inst [d]
  (cond
    (date? d) (t/inst (t/at d (t/midnight)))
    (date-time? d) (t/inst d)
    (instant? d) (t/inst d)
    (time? d) (t/at (t/today) d)
    (inst? d) d
    (integer? d) (t/instant d)
    :else (throw (error "Cannot convert " (pr-str d) " to inst."))))

(comment (->inst (t/today))
  (->inst (t/now))
  (t/inst (t/now))
  )

(defn ->instant [d] (t/instant (->inst d)))

(defn today->inst [] (->inst (t/today)))
(defn today->instant [] (t/instant (today->inst)))

(defn date
  "Delegates to tick, return nil instead of throwing for invalid date strings"
  [string]
  (try
    (t/date string)
    (catch #?(:cljs js/Error :clj Exception) e nil)))

(defn time [v]
  (try
    (t/time (->instant v))
    (catch #?(:cljs js/Error :clj Exception) e nil)))

(comment (date "2020-05-33"))

(defn ->date [v]
  (cond
    (date-time? v) (t/date v)
    (date? v) v
    (t/instant? v) (t/date v)
    (inst? v) (t/date v)
    (string? v) (t/date v)
    (t/year-month? v) (.atDay v 1)
    (t/year? v) (t/new-date (t/int v) 1 1)
    (integer? v) (t/date (t/instant v))
    :else
    (do
      (log/error (str "Unsupported type passed to ->date: " (pr-str v)))
      nil)))

(comment
  (->date "2021-09-29")
  (->date (t/year)))

(defn ->date-time [v]
  (cond
    (date-time? v) v
    (date? v) (t/at v (t/midnight))
    (instant? v) (t/date-time v)
    (t/year? v) (t/at (t/new-date (t/int v) 1 1) (t/midnight))
    (t/year-month? v) (t/at (.atDay v 1) (t/midnight))
    (inst? v) (t/date-time v)
    (string? v)
    (try
      (t/date-time v)
      (catch #?(:cljs js/Error :clj java.time.format.DateTimeParseException) e
        (-> v (t/date) (->date-time))))
    (t/year? v) (t/at (t/new-date (t/int v) 1 1) (t/midnight))
    :else (throw (error (str "Unsupported type passed to ->date-time: " (pr-str v))))))

(defn ->time [v]
  (cond
    (date-time? v) (t/time v)
    (date? v) (t/midnight)
    (instant? v) (t/time (t/date-time v))
    (inst? v) (t/time (t/date-time v))
    (t/year? v) (-> (->date-time v) t/time)
    :else (throw (error (str "Unsupported type passed to ->date-time: " (pr-str v))))))

(comment
  (->date-time (t/year))
  (->date-time (t/year-month))
  (->date (t/year-month))
  (t/date-time (t/year))
  (->time (t/year)))

(defn today-dt []
  (t/at (t/today) (t/midnight)))

(comment (date-times-in-month (t/date-time))
  (date-times-in-month (t/date))
  (date-times-in-month))

(comment (days-in-month)
  (dates-in-month (t/date "2020-04-01"))
  (dates-in-month (t/date "2020-03-01"))
  (last-day-of-month (t/date "2020-04-01"))
  (last-day-of-month (t/today))
  (dates-between (first-day-of-month) (last-day-of-month (t/today))))

(comment (date-times-in-month-arr)
  )

(defn compare-periods
  [op #?(:cljs ^js x :clj x) #?(:cljs ^js y :clj y)]
  (when-not (period? y) (throw* (pr-str y) " is not a period."))
  (let [m1 (.toTotalMonths x)
        m2 (.toTotalMonths y)]
    (if (= m1 m2)
      (op (t/days x) (t/days y))
      (op m1 m2))))

;; for some reason the compiler is associating clojure.core/< with the
;; ones defined in this namespace resulting in the comparison
;; functions below being called, so I add support for comparing them
;; using the tick protocol.
(extend-protocol ITimeComparison
  #?(:clj Long :cljs number)
  (< [x y] (clojure.core/< x y))
  (<= [x y] (clojure.core/<= x y))
  (> [x y] (clojure.core/> x y))
  (>= [x y] (clojure.core/>= x y))
  Period
  (< [x y] (compare-periods clojure.core/< x y))
  (<= [x y] (compare-periods clojure.core/<= x y))
  (> [x y] (compare-periods clojure.core/> x y))
  (>= [x y] (compare-periods clojure.core/>= x y)))

(defn make-compare
  [op]
  (fn [d1 d2]
    ;; for some reason the compiler is associating clojure.core/< with the
    ;; one defined in this namespace
    ;; so we check for times
    (cond
      (or (and (int? d1) (int? d2))
        (and (period? d1) (period? d2)))
      (op d1 d2)
      :else
      (let [d1 (->instant d1)
            d2 (->instant d2)]
        (op d1 d2)))))

(def > (make-compare t/>))
(def < (make-compare t/<))
(def >= (make-compare t/>=))
(def <= (make-compare t/<=))

(comment
  (> (t/new-date 2020 5 3) #time/date-time"2020-05-04T00:00")
  (>= (t/new-date 2020 5 3) #time/date-time"2020-05-04T00:00")
  (< (t/new-date 2020 5 3) #time/date-time"2020-05-04T00:00")
  (<= (t/new-date 2020 5 3) #time/date-time"2020-05-04T00:00")
  )

;; TODO java support
;; There's prob a helper in tick format
#?(:cljs
   (defn iso-date-str
     "Returns the ISO 8601 repr just the Date portion
     Pass any :tick-util/date? type in it will be converted to inst"
     ([] (iso-date-str (t/today)))
     ([d]
      (first (str/split (.toISOString (->inst d)) "T")))))
(comment (iso-date-str (t/today)))

;; by default the DayOfWeek Enum  starts at 1 for Monday.
;https://docs.oracle.com/javase/8/docs/api/java/time/DayOfWeek.html
;The int value follows the ISO-8601 standard, from 1 (Monday) to 7 (Sunday).
; [0 M] [1 T] [2 W] [3 Th] [4 F] [5 S] [6 Su]

;; The offset cycles right to left
;; so plus one is a sunday based week (back 1 day from monday)
;; plus 2 is a saturday based week. (back 2 days from monday)
;; plus 3 is a friday based week. etc

(def day-of-week-offsets
  {t/MONDAY    0
   t/SUNDAY    1
   t/SATURDAY  2
   t/FRIDAY    3
   t/THURSDAY  4
   t/WEDNESDAY 5
   t/TUESDAY   6})

(def weekdays->int
  (into {} (map (juxt identity t/int))
    [t/MONDAY t/TUESDAY t/WEDNESDAY t/THURSDAY t/FRIDAY t/SATURDAY t/SUNDAY]))

(def int->weekdays (into {} (map (juxt val key)) weekdays->int))

(defn day-of-week-with-offset
  "Returns the zero-based index of the weekday, for a week started by the given offset.
   The offsets map to weekdays like so:
  0: Monday
  1: Sunday
  2: Saturday
  3: Friday
  4: Thursday
  5: Wednesday
  6: Tuesday"
  [offset date]
  (let [offset (cond-> offset (not (int? offset)) day-of-week-offsets)]
    (-> (t/day-of-week date)
      ;; go to zero based so modular arithmetic works
      t/int dec
      (clojure.core/+ offset)
      (mod 7))))

(def ^{:doc "Returns int between 0-6 where zero is Monday."}
  day-of-week-monday (partial day-of-week-with-offset (day-of-week-offsets t/MONDAY)))
(def ^{:doc "Returns int between 0-6 where zero is Sunday."}
  day-of-week-sunday (partial day-of-week-with-offset (day-of-week-offsets t/SUNDAY)))
(def ^{:doc "Returns int between 0-6 where zero is Saturday."}
  day-of-week-saturday (partial day-of-week-with-offset (day-of-week-offsets t/SATURDAY)))
(def ^{:doc "Returns int between 0-6 where zero is Friday."}
  day-of-week-friday (partial day-of-week-with-offset (day-of-week-offsets t/FRIDAY)))
(def ^{:doc "Returns int between 0-6 where zero is Thursday."}
  day-of-week-thursday (partial day-of-week-with-offset (day-of-week-offsets t/THURSDAY)))
(def ^{:doc "Returns int between 0-6 where zero is Wednesday."}
  day-of-week-wednesday (partial day-of-week-with-offset (day-of-week-offsets t/WEDNESDAY)))
(def ^{:doc "Returns int between 0-6 where zero is Tuesday."}
  day-of-week-tuesday (partial day-of-week-with-offset (day-of-week-offsets t/TUESDAY)))

(comment
  ;; thur - 3 for monday based week
  (def d (t/date "2020-05-14"))
  (day-of-week-offsets 0 (t/date "2020-05-14"))
  (day-of-week-monday d)
  (day-of-week-tuesday d)
  (day-of-week-wednesday d)
  (day-of-week-thursday d)
  (day-of-week-friday d)
  (day-of-week-saturday d)
  (day-of-week-sunday d)
  )

(defn int-month
  ([] (int-month (t/month)))
  ([d] (t/int (t/month d))))

(defn int-year
  ([] (int-year (t/year)))
  ([d] (t/int (t/year d))))

(defn prior
  ([units extract] (prior units extract (today-dt)))
  ([units extract d]
   (extract (t/- d (t/new-period 1 units)))))

(def prior-year (partial prior :years t/year))
(def prior-int-year (partial prior :years int-year))

(def prior-month (partial prior :months identity))
(def prior-week (partial prior :weeks identity))
(def prior-day (partial prior :days identity))
(def yesterday prior-day)

(defn subsequent
  ([units extract] (subsequent units extract (today-dt)))
  ([units extract d]
   (extract (t/+ d (t/new-period 1 units)))))

(defn today [] (t/at (t/today) (t/midnight)))
(def next-day (partial subsequent :days identity))
(def tomorrow next-day)
(def next-month (partial subsequent :months identity))
(def next-week (partial subsequent :weeks identity))
(def next-year (partial subsequent :years identity))

(defn today? [v] (= (->date v) (t/today)))
(defn tomorrow? [v] (= (->date v) (t/tomorrow)))

(comment
  (today? (t/today))
  (today? (t/tomorrow))
  (next-day)
  (prior-month)
  (next-week)
  (next-month)
  (next-year)
  (prior :years int-year)
  (prior :years t/year (t/today))
  (t/year (t/- (t/today) (t/new-period 1 :years))))

(defn prior-day-of-week
  "Returns d if it is the day of week `day` already, otherwise the most recent `day` of the week in the past."
  ([day-of-week] (prior-day-of-week day-of-week (t/today)))

  ([day-of-week d]
   (if (= day-of-week (t/day-of-week d))
     d
     (t/- d
       (t/new-period (day-of-week-with-offset day-of-week d)
         :days)))))

(def prior-sunday (partial prior-day-of-week t/SUNDAY))
(def prior-monday (partial prior-day-of-week t/MONDAY))
(def prior-tuesday (partial prior-day-of-week t/TUESDAY))
(def prior-wednesday (partial prior-day-of-week t/WEDNESDAY))
(def prior-thursday (partial prior-day-of-week t/THURSDAY))
(def prior-friday (partial prior-day-of-week t/FRIDAY))
(def prior-saturday (partial prior-day-of-week t/SATURDAY))

(defn next-dow
  ([dow] (next-dow dow (t/today)))
  ([dow d]
   (let [dow* (day-of-week-with-offset (day-of-week-offsets dow) d)]
     (t/+ d (t/new-period (clojure.core/- 7 dow*) :days)))))

(def next-sunday (partial next-dow t/SUNDAY))
(def next-monday (partial next-dow t/MONDAY))
(def next-tuesday (partial next-dow t/TUESDAY))
(def next-wednesday (partial next-dow t/WEDNESDAY))
(def next-thursday (partial next-dow t/THURSDAY))
(def next-friday (partial next-dow t/FRIDAY))
(def next-saturday (partial next-dow t/SATURDAY))

(comment
  (next-dow t/SUNDAY (t/today))
  (next-dow t/MONDAY (t/today))
  (next-dow t/TUESDAY (t/today))
  (next-dow t/WEDNESDAY (t/today))
  (next-dow t/THURSDAY (t/today))
  (next-dow t/FRIDAY (t/today))
  (next-dow t/SATURDAY (t/today))
  (dec (t/int t/THURSDAY))
  (dec (t/int t/WEDNESDAY))
  )

(comment
  (t/+ (t/today (t/new-period :days)))
  (offset-day-of-week t/+ t/MONDAY (t/today))
  (t/date (t/+ (today)
            (t/new-period (clojure.core/-
                            (dec (t/int t/SUNDAY))
                            (dec (t/int (t/day-of-week)))) :days)))
  (t/date (t/+ (today)
            (t/new-period (clojure.core/+
                            (dec (t/int (t/day-of-week)))
                            (dec (t/int t/SUNDAY))
                            ) :days)))

  (prior-sunday (t/date "2020-01-01"))
  (prior-sunday) (prior-monday) (prior-tuesday) (prior-wednesday)
  (prior-thursday) (prior-friday) (prior-saturday)
  (next-sunday) (next-monday) (next-tuesday) (next-wednesday)
  (next-thursday) (next-friday) (next-saturday)

  (next-sunday (t/date "2020-05-29")))

(defn at-midnight
  "Input: date or date-time return date-time at 00:00"
  [d]
  (cond
    (date? d)
    (t/at d (t/midnight))

    (date-time? d)
    (t/truncate d :days)

    :otherwise
    (throw (error "Unknown type for `at-midnight`: '" d "' of type: " (type d)))))

(comment (at-midnight (t/date))
  (at-midnight (t/date-time))
  (at-midnight (t/inst)))

(defn first-of-year [] (-> (t/year) t.i/bounds t/beginning))
(comment (first-of-year))

(defn start-of [v]
  (t/beginning (t.i/bounds v)))
(comment (start-of (t/bounds (t/date-time))))

(defn end-of [v]
  (t/end (t.i/bounds v)))

(defn start-of-year
  "Input: optional [int] year - returns date-time at start of `year`"
  ([] (start-of-year (t/year)))
  ([year] (-> (t/year year) t.i/bounds t/beginning)))

(defn end-of-year
  ([] (end-of-year (t/date-time)))
  ([v]
   (end-of (t/year v))))

(comment (start-of-year))

(defn days-this-year
  ([]
   (let [year (t/int (t/year))]
     (t/range (start-of-year year) (start-of-year (inc year))
       (t/new-duration 1 :days))))
  ([year] (t/range (start-of-year year) (start-of-year (inc year))
            (t/new-duration 1 :days))))

(defn days-this-year-inst []
  (->> (days-this-year)
    (map t/inst)))

(defn day-of-week-int [d]
  (t/int (t/day-of-week d)))

(defn months-in-year
  ([] (months-in-year (t/year)))
  ([d]
   (let [intvl (t.i/bounds (t/year d))]
     (t/range (t/beginning intvl)
       (t/end intvl)
       (t/new-period 1 :months)))))

(comment (months-in-year))

(defn dates-between [start end]
  (->> (t/range (->date start) (t/+ (->date end) (t/new-period 1 :days)) (t/new-period 1 :days))
    (map t/date)))

(comment
  (dates-between (t/year) (t/today))
  (t/range
    (->date (t/year))
    (t/+ (->date (last-day-of-month)) (t/new-period 1 :days))
    (t/new-period 1 :days))

  (dates-between (t/date-time) (last-day-of-month))
  (dates-between (first-day-of-month)
    (last-day-of-month)))

(defn date-times-between [start end]
  "Return seq of date-times at midnight for each day between start until end."
  (t/range (at-midnight start) end
    (t/new-period 1 :days)))

(comment
  (date-times-between (t/date-time) (last-day-of-month))
  (date-times-between (first-day-of-month) (last-day-of-month)))

(defn first-day-of-month
  ([] (first-day-of-month (t/today)))
  ([date]
   (-> date t/year-month t/beginning t/date)))

(defn last-day-of-month
  "date - tick/date or similar"
  ([] (last-day-of-month (t/today)))
  ([x]
   #?(:clj  (.atEndOfMonth (YearMonth/from (->date x)))
      :cljs (.atEndOfMonth (.from YearMonth (->date x))))))

(comment
  (last-day-of-month (t/year) #_(t/date "2022-02-01"))
  )

(defn dates-in-month
  "Return seq of dates for each day of the entire month of the passed in date, or of today."
  ([] (dates-in-month (t/today)))
  ([date]
   (dates-between (first-day-of-month date) (last-day-of-month date))))

(defn date-times-in-month
  "Return seq of date-times for each day of the entire month of the passed in date, or of today."
  ([] (date-times-in-month (t/today)))
  ([date]
   (date-times-between (first-day-of-month date) (last-day-of-month date))))

(defn dates-in-month-arr
  "Return native array of dates for each day of the entire month of the passed in date, or of today."
  ([] (into-array (dates-in-month)))
  ([date]
   (into-array (dates-in-month date))))

(defn date-times-in-month-arr
  "Return native array of dates for each day of the entire month of the passed in date, or of today."
  ([] (into-array (date-times-in-month)))
  ([date]
   (into-array (date-times-in-month date))))

(def start-of-month first-day-of-month)

(comment (first-day-of-month)
  (first-day-of-month (t/date-time "2020-02-09T00:00")))

(def end-of-month last-day-of-month)

(comment
  ;; LEAP YEAR! HOW EXCITIN
  (last-day-of-month (t/date-time "2020-02-03T00:00"))
  (last-day-of-month)
  (last-day-of-month (t/today))

  (last-day-of-month (t/date "2020-04-05"))

  (last-day-of-month #time/date "2020-09-18")
  (last-day-of-month (t/+ (t/today) (t/new-period 3 :months)))
  )

;; todo update the week* version to take a date-like object (could also support year, month etc)

(defn week*
  "Return a seq of date-times starting from the prior Monday"
  ([day-of-week prior-fn next-fn] (week* day-of-week prior-fn next-fn (t/today)))
  ([day-of-week prior-fn next-fn d]
   (t/range
     (prior-fn d)
     ;; use next-day b/c range is exclusive of the end date
     (if (= day-of-week (t/day-of-week d)) (next-day d) (next-fn d)))))

(defn week-with-days*
  ([day-of-week prior-fn next-fn] (week-with-days* day-of-week prior-fn next-fn (t/today)))
  ([day-of-week prior-fn next-fn d] (map (juxt identity t/day-of-week) (week* day-of-week prior-fn next-fn d))))

;(def sunday-week (partial week* prior-sunday #(= t/SUNDAY (t/day-of-week %)) next-sunday))
;; todo this should use macro to generate these
;; could be a good use of a macro that defines macros, such that the
;; ultimately expanded code is inlined at compile time
(def sunday-week (partial week* t/SATURDAY prior-sunday next-sunday))
(defn sunday-week-with-days [& args] (->> (apply sunday-week args) (map (juxt identity t/day-of-week))))
(def monday-week (partial week* t/SUNDAY prior-monday next-monday))
(defn monday-week-with-days [& args] (->> (apply monday-week args) (map (juxt identity t/day-of-week))))
(def tuesday-week (partial week* t/MONDAY prior-tuesday next-tuesday))
(defn tuesday-week-with-days [& args] (->> (apply tuesday-week args) (map (juxt identity t/day-of-week))))

(macroexpand '(->> (apply monday-week args) (map (juxt identity t/day-of-week))))

(comment
  (monday-week)
  (sunday-week-with-days)
  (monday-week-with-days)
  (tuesday-week-with-days)
  )

;(get-week-fn day t/SUNDAY, prior-fn prior-monday, next-fn next-monday)
(comment
  (get-week-fn day t/SUNDAY, prior-fn prior-monday, next-fn next-monday)
  )

(comment (week-with-days))

(comment
  (week-with-days (t/today))
  (week (t/date "2022-01-01"))
  (= (monday-week) (week))
  (map (juxt identity t/day-of-week) (sunday-week))
  (map (juxt identity t/day-of-week) (monday-week))
  (week)
  )
;; todo extract `prior-sunday` into an argument to make this generic
;; across days of the week.
(defn get-days-of-week
  "date-times from prior sunday to subsequent for passed in val."
  ([] (get-days-of-week (t/now)))
  ([d]
   (take 7 (period-seq (t/new-period 1 :days) (prior-sunday d)))))
(comment (get-days-of-week (next-month)))

(defn weeks-of-month-monday
  "Returns a seq of seqs containing dates where the interior seqs are seven days of a week starting on Monday.
   Some dates from the prior month and subsequent may be in the first and last seq, as each seq is a full week."
  ([] (weeks-of-month-monday (t/date-time)))
  ([d]
   (let [s (prior-monday (start-of-month d))
         e (t/date (next-monday (prior-day (end-of-month d))))]
     (partition 7 (t/range s e (t/new-period 1 :days))))))
(comment
  (weeks-of-month-monday #time/date "2021-02-04"))

(defn weeks-of-month
  "Returns a seq of seqs containing dates where the interior seqs are seven days of a week starting on Sunday.
   Some dates from the prior month and subsequent may be in the first and last seq, as each seq is a full week."
  ([] (weeks-of-month (t/date-time)))
  ([d]
   (let [s (prior-sunday (start-of-month d))
         e (t/date (next-sunday (prior-day (end-of-month d))))]
     (partition 7 (t/range s e (t/new-period 1 :days))))))

(comment (weeks-of-month)
  (weeks-of-month (t/date "2021-03-01")))

(defn prior-sunday-of-month
  "See `prior-sunday`"
  ([] (prior-sunday-of-month (t/today)))
  ([date] (-> date first-day-of-month prior-sunday)))
(comment (prior-sunday-of-month))

;; todo rename
(defn get-weeks
  "Given a date return a seq of all the weeks in the year as tick/intervals"
  [date]
  (let [date      (t/date date)
        first-day (start-of-year (t/year date))
        d1        (t/day-of-month first-day)
        d2        (day-of-week-sunday first-day)
        d-offset  (clojure.core/- d1 (inc d2))
        ;; Start at the first of the year and move back to prior sunday
        sunday    (t/+ (t/new-date (t/year date) 1 1)
                    (t/new-period d-offset :days))
        intvl     (t.i/bounds sunday (t/new-date (t/year date) 12 31))]
    (map #(apply t.i/new-interval %)
      (t.i/divide-by (t/new-period 7 :days) intvl))))


(comment (get-weeks (t/date))
  (t/fields (t/now)))

;; todo test, need to get full list of interval relations, see comment below here,
;; there's also an example in the tick docs you can use
;; maybe :contains is what I'm looking for?
(defn within?
  "Is date inside given interval (or start end)"
  ([intvl d]
   (contains? #{:starts :meets :during :finishes}
     (t.i/relation d intvl)))
  ([start end d]
   (within? (t.i/new-interval start end) d)))

(defn within-new?
  "Is date inside given interval (or start end)"
  ([intvl d]
   (t/< (->date-time (t/beginning intvl)) (->date-time d) (->date-time (t/end intvl))))
  ([start end d]
   (t/< (->date-time start) (->date-time d) (->date-time end))))

(comment
  (within-orig?
    {:tick/beginning #time/date-time"2019-12-29T00:00", :tick/end #time/date-time"2020-01-05T00:00"}
    (t/date "2020-01-05")
    )

  (within?
    {:tick/beginning #time/date-time"2019-12-29T00:00", :tick/end #time/date-time"2020-01-05T00:00"}
    (t/date "2020-01-05")
    )

  (within? (t/yesterday) (t/tomorrow) (t/today))
  (within? (t/yesterday) (t/today) (t/today))
  (within? (t.i/new-interval (t/yesterday) (t/tomorrow)) (t/now))
  (within? (start-of-year) (t/date-time) (t/date-time))
  (within? (start-of-year) (t/date-time) (t/date-time))
  (within? (start-of-year) (t/yesterday) (t/date-time))
  (within? (start-of-year) (t/date-time) (t/yesterday))
  (within? (t.i/new-interval (start-of-year) (t/date-time)) (t/today))
  )

(comment
  ;from tick.interval

  (def relation->kw
    {precedes?      :precedes
     meets?         :meets
     starts?        :starts
     during?        :during
     finishes?      :finishes
     overlaps?      :overlaps
     equals?        :equals
     contains?      :contains
     started-by?    :started-by
     finished-by?   :finished-by
     overlapped-by? :overlapped-by
     met-by?        :met-by
     preceded-by?   :preceded-by})
  )


;; This now appears to be working, need to add tests.
(defn week-num*
  "Given a date return the week number of the year [1-52]."
  [date]
  (let [date  (t/date date)
        weeks (get-weeks date)]
    (first (keep-indexed
             (fn [idx intvl]
               (when (within? intvl date) idx))
             weeks))))

(def week-num (memoize week-num*))

; There is also this technique, but may not support weeks on "unaligned" days:
; (-> (cljc.java-time.local-date/now)
;  (cljc.java-time.local-date/get cljc.java-time.temporal.chrono-field/aligned-week-of-year))

(comment
  (time (week-num (t/new-date 2021 3 5)))
  (time (week-num* (t/new-date 2021 3 5))))

(comment (week-num (t/new-date 2021 3 5))
  (week-num #time/date "2020-01-05")
  (week-num (t/new-date 2021 12 30))
  (week-num (t/new-date 2021 12 30))
  (week-num (t/new-date 2020 1 1))
  (week-num (t/new-date 2020 1 5))
  (week-num (t/new-date 2021 1 1))
  (week-num (t/new-date 2021 1 2))
  (week-num (t/new-date 2021 1 3))
  (week-num (t/new-date 2021 1 4))
  (week-num (t/new-date 2020 2 1))
  (week-num (t/new-date 2022 1 1)) ;saturday
  (week-num (t/new-date 2022 1 2))
  (week-num (t/new-date 2022 1 3))
  (week-num (t/new-date 2023 1 1))
  (week-num (t/new-date 2023 1 2))
  (week-num (t/new-date 2023 1 8)))

;; todo rename to week-num-of-month
(defn week-index
  "Week number within this month for given date `d`, subtracts week of `d` and week-num of beginning of month."
  [d]
  (let [d (t/date d)]
    (clojure.core/- (week-num d)
      (-> (first-day-of-month d) week-num))))

(comment
  (week-index #time/date "2020-01-05")
  (week-index (t/date "2020-01-05"))
  (week-num (first-day-of-month (t/date "2020-01-05")))

  (t/date (t/date "2020-01-05"))

  )

(defn capitalized-month [date]
  (str/capitalize (str (t/month date))))

(comment (week-index (t/inst))
  (week-index (t/date "2020-03-01"))
  (week-index (t/date "2020-03-01"))
  (week-num (t/date "2020-03-01"))
  (week-num (first-day-of-month (t/date "2020-03-01")))
  (first-day-of-month (t/date "2020-03-01"))
  (week-num (t/date "2020-03-01"))
  (week-num (t/date "2020-03-01"))
  ;(d3/timeWeek.count (t/inst (start-of-year)) (js/Date. 2020 0 1))
  ;(d3/timeWeek.count (t/inst (start-of-year)) (js/Date. 2020 0 5))
  (week-num (t/date "2020-01-05"))
  ;(d3/timeWeek.count (t/inst (start-of-year 2021)) (js/Date. 2021 11 31))
  ;(d3/timeWeek.count (t/inst (start-of-year 2021)) (js/Date. 2021 0 1))
  (week-num (t/new-date 2020 1 1))
  (week-num (t/new-date 2021 1 1))
  (week-num (t/new-date 2021 12 30))
  (week-num (t/new-date 2021 12 31))
  ;(d3/timeWeek.count (t/inst (start-of-year 2021)) (js/Date. 2021 11 30))
  (week-index (t/date "2020-01-01"))
  (week-index (t/date "2020-01-05"))
  ;(week-index (js/Date.))
  (week-index (t/date "2020-02-01")) ; should be 0
  (week-index (t/date "2020-02-02")) ; should be 1
  (week-index (t/date "2020-02-09"))
  (week-num (first-day-of-month (t/date "2020-02-01")))
  (week (t/inst)))

(comment
  (def i (t.i/new-interval (t/date "2020-03-15") (t/+ (t/date "2020-03-15") (t/new-period 7 :days))))
  i
  (t/>> i (t/new-period 7 :days)))

;; Might be able to use t/group-by for this with t/day or something
(defn make-dates-to-intvls
  "Return a map of date-time to the interval that the date-time is within, for
  all date-times (a day apart) within the seq of passed in intervals."
  [intvls]
  (reduce
    (fn [acc intvl]
      (let [dates (t/range (t/beginning intvl) (t/end intvl) (t/new-period 1 :days))]
        (into acc (map #(vector % intvl)) dates)))
    {}
    intvls))

(comment (make-dates-to-intvls
           ;; see date-gen ns
           (make-week-intervals (first-day-of-month))))

;; Taken from the tick site, just here for reference.
(defn countdown
  [end-time]
  (let [duration (t/duration
                   {:tick/beginning (t/instant)
                    :tick/end       end-time})
        hours    (t/hours duration)
        minutes  (t/minutes (t/- duration
                              (t/new-duration hours :hours)))
        seconds  (t/seconds (t/- duration
                              (t/new-duration minutes :minutes)
                              (t/new-duration hours :hours)))]
    (if (t/< (t/instant) end-time)

      (println (interpose ":" [hours minutes seconds]))
      "Time's up!")))

;; From the tick site.
(defn instant-breakdown
  "Takes an instant of time and breaks it down into units."
  [t]
  {:day   (t/day-of-week t)
   :month (t/month t)
   :dd    (t/day-of-month t)
   :MM    (t/int (t/month t))
   :yyyy  (t/int (t/year t))
   :mm    (t/minute t)
   :HH    (t/hour t)
   :ss    (t/second t)})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Transit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; copied from time-literals lib

;; period prints first then duration
(defn print-offset [^Offset o]
  (let [duration (-duration o)
        period   (-period o)]
    (str "#time/offset \"" (if period period "nil") " " (if duration duration "nil") "\"")))

(comment
  (pr-str (offset (t/new-duration 1 :hours) (t/new-period 2 :days)))
  (offset (t/new-period 2 :days))
  (-period (offset (t/new-period 2 :days)))
  (offset (t/new-duration 2 :minutes))
  )

#?(:cljs
   (extend-protocol IPrintWithWriter
     Offset (-pr-writer [d writer opts] (-write writer (print-offset d)))))

#?(:clj (defmethod print-method Offset [c ^Writer w] (.write w ^String ^String (print-offset c))))
#?(:clj (defmethod print-dup Offset [c ^Writer w] (.write w ^String (print-offset c))))

(def transit-tag "time/tick")

#?(:cljs (deftype TickHandler []
           Object
           (tag [_ v] transit-tag)
           (rep [_ v] (pr-str v))
           (stringRep [_ v] nil)))

(def tick-transit-write-handler
  #?(:cljs (TickHandler.)
     :clj  (tr/write-handler
             transit-tag
             (fn [v] (pr-str v)))))

(def tick-transit-writer-handler-map
  (reduce
    (fn [m [k v]] (assoc m k v))
    {}
    (partition 2
      (interleave
        ;; todo add all the types here
        [Date DateTime Time Period Duration Instant Offset]
        (repeat tick-transit-write-handler)))))

;;
;; (2020-09-01) I'm not sure why but you have to return the code as data instead of being evaluated at read time.
;; This was figured out by looking at the code for the time-literals library.
;;
(defn read-offset-edn
  "Period is printed first then duration."
  [offset-str]
  (let [[period duration] (str/split offset-str #" ")]
    `(let [period#   (if (= "nil" ~period) nil (. java.time.Period ~'parse ~period))
           duration# (if (= "nil" ~duration) nil (. java.time.Duration ~'parse ~duration))]
       (Offset. period# duration# nil))))

#?(:cljs (cljs.reader/register-tag-parser! 'time/offset read-offset-edn))

(defn read-offset-transit
  "Period is printed first then duration."
  [offset-str]
  (let [offset-str (str/replace (str/replace offset-str "#time/offset " "") "\"" "")
        [period duration] (str/split offset-str #" ")
        period*    (if (= "nil" period) nil (. java.time.Period parse period))
        duration*  (if (= "nil" duration) nil (. java.time.Duration parse duration))]
    (->Offset period* duration* nil)))

(comment
  (. Period parse "nil")

  (-duration #time/offset"nil PT1H")
  #time/offset"nil PT1H"
  (offset (t/new-duration 1 :hours) (t/new-period 2 :days))
  (offset (t/new-period 2 :days))
  ;(clojure.edn/read-string {:readers (assoc rw/tags 'time/offset read-offset)} #time/offset "#time/duration\"PT1H\" #time/period\"P2D\"" )
  (clojure.edn/read-string {:readers (assoc rw/tags 'time/offset read-offset)} (pr-str (offset (t/new-duration 1 :hours))))
  (clojure.edn/read-string {:readers (assoc rw/tags 'time/offset read-offset)} (pr-str (offset (t/new-duration 1 :hours) (t/new-period 2 :days))))
  (clojure.edn/read-string {:readers (assoc rw/tags 'time/offset read-offset)} (pr-str (offset (t/new-period 2 :days))))
  (offset (t/new-duration 1 :hours))
  (pr-str (t/new-duration 1 :hours)))

(def tick-transit-reader
  {transit-tag
   #?(:cljs (fn [v]
              (if (str/starts-with? v "#time/offset ")
                (read-offset-transit v)
                (cljs.reader/read-string v)))
      :clj  (tr/read-handler #(clojure.edn/read-string {:readers (assoc rw/tags 'time/offset read-offset-transit)} %))
      ;:clj (tr/read-handler #(clojure.edn/read-string {:readers rw/tags} %))
      )})

#?(:clj (defn write-tr [data]
          (let [out         (ByteArrayOutputStream. 4096)
                date-writer (tr/writer out :json {:handlers tick-transit-writer-handler-map})]
            (tr/write date-writer data)
            (.toString out))))
;; clj
(comment
  (do
    (def out (ByteArrayOutputStream. 4096))
    (def date-writer (tr/writer out :json {:handlers tick-transit-writer-handler-map}))
    (tr/write date-writer (t/today))
    (.toString out)

    (def in (ByteArrayInputStream. (.toByteArray out)))

    (def date-reader (tr/reader in :json {:handlers tick-transit-reader}))
    (def v (tr/read date-reader))
    v
    )

  (def my-data
    #:user{:habits [#:habit{:id #uuid"9069a8ef-88c6-41fd-9cca-b09bd6fe2f9a", :task-id {}}
                    #:habit{:id #uuid"cf574926-40dc-4ea9-aeff-97f1077e5365", :task-id {}}
                    #:habit{:active?         true,
                            :description     "Something to do",
                            :schedule        nil,
                            :task            #:task{:description nil :id :task-dishes},
                            starts-on        #time/date-time"2020-05-03T00:00",
                            :criteria        :exactly,
                            :repeats-every   #time/period"P1D",
                            :user-scheduled? false,
                            :duration        nil,
                            :id              #uuid"0ca3642e-a27a-4b5f-b056-5206ac18ca9c",
                            :criteria-num    2}]})
  (write-tr my-data)
  (do
    (def out (ByteArrayOutputStream. 4096))
    (def date-writer (tr/writer out :json
                       {:handlers tick-transit-writer-handler-map}))

    (def d {[:habit/id #uuid"0ca3642e-a27a-4b5f-b056-5206ac18ca9c"] #:habit{:starts-on #time/date"2020-05-03",
                                                                            :task      #:task{:description nil}}})
    (tr/write date-writer d)
    (.toString out))
  )

;; cljs
(comment
  "{\"~:task/scheduled-at\":{\"~#time/tick\":\"#time/offset \"P0D PT9H\"\"}}"
  (clojure.edn/read-string "[\"hello world\"]")
  (def date-reader (tr/reader :json {:handlers tick-transit-reader}))
  (def date-writer (tr/writer :json {:handlers tick-transit-writer-handler-map}))
  (tr/read date-reader (tr/write date-writer (t/today)))
  (tr/read date-reader (tr/write date-writer (offset 1 :hours 2 :minutes)))
  (tr/read date-reader (tr/write date-writer (t/now)))
  (tr/read date-reader (tr/write date-writer (t/new-period 1 :days)))
  (def w (tr/writer :json))
  ;(tr/write w (js/Date.))
  (do
    (def w (tr/writer :json))
    (def r (tr/reader :json))
    (tr/read r (tr/write w (t/today))))
  (tr/write date-writer #time/date "2020-05-19"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; name ideas: floor-minutes, truncate-duration duration->just-minutes
;; duration->only-minutes
;; duration->minutes-only
;; drop-hours
;; without-hours
;;
(defn rest-minutes
  "take a duration remove the hours and get mins remaining"
  [duration]
  (t/minutes
    (t/- duration
      (t/new-duration (t/hours duration) :hours))))

(defn duration->hrs-mins-secs
  [duration]
  (let [hours   (t/hours duration)
        minutes (t/minutes (t/- duration
                             (t/new-duration hours :hours)))
        seconds (t/seconds (t/- duration
                             (t/new-duration minutes :minutes)
                             (t/new-duration hours :hours)))]

    [hours minutes seconds]))

(comment
  (duration->hrs-mins-secs (duration 1 :hours 10 :minutes 5 :seconds))
  (duration 1 :hours 10 :minutes 5 :seconds)
  )

(defn offset->time
  "Takes an offset and returns the time version of the duration.
  example: (offset (period 1 :days) (duration 9 :hours 10 :minutes))
  -> #time/time 09:10"
  [offset]
  (let [[hr mins] (duration->hrs-mins-secs (-duration offset))]
    (t/new-time hr mins)))

(comment
  (offset->time (offset (period 1 :days) (duration 9 :hours 10 :minutes))))


;; this may be unintuitive b/c units add for more precise with this setup
;;
;; todo, could give back a map with successive larger values removed
;; in the form that's intuitive => the sum of the map is the total duration time
;; this would be mins:
;(t/minutes (t/- duration
;             (t/new-duration (t/hours duration) :hours)))


(defn duration->map
  [d]
  {:nanos   (t/nanos d)
   :micros  (t/micros d)
   :millis  (t/millis d)
   :seconds (t/seconds d)
   :minutes (t/minutes d)
   :hours   (t/hours d)})

(def duration-unit-keys [:hours :minutes :seconds :millis :micros :nanos])

(def plus-duration-fns
  {:nanos   (fn [d v] (.plusNanos #?(:cljs ^js d :clj d) v))
   ;; there is no plusMicros in java.time.Duration
   :micros  (fn [d v] (.plusNanos #?(:cljs ^js d :clj d) (* 1000 v)))
   :millis  (fn [d v] (.plusMillis #?(:cljs ^js d :clj d) v))
   :seconds (fn [d v] (.plusSeconds #?(:cljs ^js d :clj d) v))
   :minutes (fn [d v] (.plusMinutes #?(:cljs ^js d :clj d) v))
   :hours   (fn [d v] (.plusHours #?(:cljs ^js d :clj d) v))})

(comment (duration->map (t/new-duration 1 :minutes)))

;;
;; todo update to accept singular units minute hour second etc
;; mainly for 1 (duration 1 :hour 2 :minutes)
;;
(>defn duration
  [num units & args]
  [int? duration-units? (s/* (s/cat :n int? :v duration-units?)) => duration?]
  (reduce (fn [acc [val units]]
            (if-let [f (get plus-duration-fns units)]
              (f acc val)
              (throw (error "Unknown units passed to duration: " units))))
    (t/new-duration num units)
    (partition 2 args)))

(comment
  (duration 1 :hours 2 :minutes 100 :seconds)
  (duration -1 :hours 2 :minutes 100 :nanos)
  (t/new-duration -5 :minutes)
  (.plusMinutes (t/new-duration 1 :hours) 20)
  )

;; todo - could take a string and pass that to this fn as well and then dispatch on type
(defn time->duration
  "Given a time object return a duration."
  [t]
  (duration (t/hour t) :hours (t/minute t) :minutes (t/second t) :seconds))

(comment (time->duration #time/time "13:34"))

(defn +impl
  [v1 v2]
  [(s/or :time time-type? :offset offset-type? :nil nil?)
   (s/or :time time-type? :offset offset-type? :nil nil?)
   =>
   (s/or :date-time time-type? :offset offset-type?)]
  (cond
    (nil? v2) v1
    (nil? v1) v2

    (or
      (and (duration? v1) (period? v2))
      (and (period? v1) (duration? v2)))
    (offset v1 v2)

    (or
      (and (duration? v1) (duration? v2))
      (and (period? v1) (period? v2)))
    (t/+ v1 v2)

    (or (and (offset? v1) (offset? v2))
      (and (offset? v2) (offset? v1)))
    (add-offset v1 v2)

    (and (offset-type? v1) (time-type? v2))
    (t/+ (->instant v2) v1)

    (and (date? v1) (offset-type? v2))
    (t/+ (->date-time v1) v2)

    (and (date-time? v1) (offset-type? v2))
    (t/+ v1 v2)

    (and (date? v2) (date-time? v2) (offset-type? v1))
    (t/+ (->date-time v2) v1)

    (and (date-time? v2) (offset-type? v1))
    (t/+ v2 v1)

    (and (time-type? v1) (offset-type? v2))
    (t/+ (->instant v1) v2)

    (and (time-type? v1) (offset-type? v2)) (t/+ (->instant v1) v2)

    (and (period? v1) (offset? v2)) (offset (+ (-period v2) v1) (-duration v2))
    (and (offset? v1) (period? v2)) (offset (+ (-period v1) v2) (-duration v1))

    (and (offset? v1) (duration? v2)) (offset (-period v1) (+ (-duration v1) v2))
    (and (duration? v1) (offset? v2)) (offset (-period v2) (+ (-duration v2) v1))

    (and (time? v1) (date? v2)) (t/at v2 v1)
    (and (date? v1) (time? v2)) (t/at v1 v2)

    (and (time? v1) (time? v2)) (t/+ v1 (time->duration v2))

    (and (time? v1) (date-time? v2)) (t/at (t/date v2) (t/+ v1 (time->duration (t/time v2))))
    (and (date-time? v1) (time? v2)) (t/at (t/date v1) (t/+ v2 (time->duration (t/time v1))))

    ;;todo i should add support for & args by using reduce to call + again 2 args at a time.

    :else
    (throw (error "Unkown types passed to +: " (type v1) ", " (type v2) " \nvals: " (pr-str v1) ", " (pr-str v2)))))

(defn +
  "Add thing without caring about type
  any combination of any two of:
  time-type|date-type|duration|period|offset => one of these input types dependent upon the semantics based on the args."
  ([] (offset nil (. Duration -ZERO)))
  ([arg] arg)
  ([arg & args]
   (reduce #(+impl %1 %2) arg args)))

(comment
  (+ (offset 1 :days) (offset 2 :hours) (offset 3 :days)
    (period 1 :days))
  (+ #time/time "04:00" #time/time "05:02")

  (let [t (t/time "13:03")]
    (duration (t/hour t) :hours (t/minute t) :minutes (t/second t) :seconds))
  (t/duration (t/time "13:00"))
  (+ (t/date) (offset 5 :days 2 :hours))
  (t/+ (t/date) (offset 5 :days 2 :hours))
  (t/hour (t/time "13:00"))
  (t/+ (t/time "12:00") (t/new-duration 5 :minutes))
  (t/+ #time/time "03:14" (t/date-time))
  (+ #time/time "03:14" (t/date-time))
  (t/time (t/date-time))
  (+ (t/now) (offset 1 :hours))
  (+ #time/date "2020-09-11" #time/time "03:14")

  (t/now)
  (offset-type? #time/date "2020-07-21")
  (time-type? #time/date "2020-07-21")
  (+ #time/date "2020-07-21" #time/period "P1D")
  (t/+ (->instant #time/date "2020-07-21") #time/period "P1D")
  (+ #time/date "2020-07-21", #time/period "P1D")
  (+ (period 1 :weeks) (offset 1 :days))
  (+ (period 1 :weeks) (offset 1 :minutes))
  )

(>defn -
  "Subtract things without caring about type
  any combination of any two of:
  time-type|date-type|duration|period|offset => one of these input types dependent upon the semantics based on the args."
  [v1 v2]
  [(s/or :time time-type? :offset offset-type? :nil nil?)
   (s/or :time time-type? :offset offset-type? :nil nil?)
   =>
   (s/or :date-time time-type? :offset offset-type?)]
  (cond
    (nil? v2) v1
    (nil? v1) v2
    (or
      (and (duration? v1) (period? v2))
      (and (period? v1) (duration? v2)))
    (offset v1 v2)
    (or
      (and (duration? v1) (duration? v2))
      (and (period? v1) (period? v2)))
    (t/- v1 v2)

    (and (offset-type? v1) (time-type? v2))
    (t/- (->instant v2) v1)

    (and (date? v1) (offset-type? v2))
    (t/- (->date-time v1) v2)

    (and (date-time? v1) (offset-type? v2))
    (t/- v1 v2)

    (and (date? v2) (offset-type? v1))
    (t/- (->date-time v2) v1)

    (and (date-time? v2) (offset-type? v1))
    (t/- v2 v1)

    (and (time-type? v1) (offset-type? v2))
    (t/- (->instant v1) v2)

    (and (time? v1) (time? v2))
    (t/- (time->duration v1) (time->duration v2))

    ;; returns a period
    (and (period? v1) (offset? v2)) (- v1 (-period v2))

    (and (offset? v1) (period? v2)) (offset (- (-period v1) v2) (-duration v1))

    ;; returns a duration
    (and (duration? v1) (offset? v2)) (- v1 (-duration v2))

    (and (offset? v1) (duration? v2)) (offset (-period v1) (- (-duration v1) v2))

    :else
    (throw (error "Unkown types passed to `-`: " (type v1) ", " (type v2) " vals: " v1 ", " v2))))

(comment
  (+ (duration 10 :minutes) (duration 10 :hours))
  (+ (today) (period 1 :years))
  (+ (today) (offset (period 1 :years) (duration 5 :minutes)))
  (+ (t/inst) (offset (period 1 :years) (duration 5 :minutes)))
  (+ (period 1 :days) (duration 10 :hours))
  (+ (duration 10 :hours) (period 1 :days))
  (+ (period 1 :days) (period 10 :days))
  (+ (offset (t/new-duration 25 :minutes)) (t/now))
  (+ (t/now) (offset (t/new-duration 25 :minutes)))
  (+ (t/new-duration 25 :minutes) (t/now))
  (+ (t/new-period 25 :days) (t/now))
  (+ (t/now) (t/new-duration 25 :minutes))
  (+ (t/now) (t/new-period 25 :days))
  (+ (offset (t/new-duration 25 :minutes)) (period 2 :weeks))

  (- (duration 10 :minutes) (duration 10 :hours))
  (- (period 1 :days) (duration 10 :hours))
  (- (today) (period 1 :years))
  (- (duration 10 :hours) (period 1 :days))
  (- (period 1 :days) (period 10 :days))
  (- (period 10 :days) (period 1 :days))
  (- (offset (t/new-duration 25 :minutes)) (t/now))
  (- (t/now) (offset (t/new-duration 25 :minutes)))
  (- (t/new-duration 25 :minutes) (t/now))
  (- (t/new-period 25 :days) (t/now))
  (- (t/now) (t/new-duration 25 :minutes))
  (- (t/now) (t/new-period 25 :days))
  (- (offset (t/new-duration 25 :minutes)) (period 2 :weeks))
  (- (period 2 :weeks) (offset (t/new-duration 25 :minutes)))
  (- (period 2 :weeks) (offset (t/new-period 1 :days) (t/new-duration 25 :minutes)))

  (reduce t/+ (t/new-duration 0 :seconds) ())
  (t/+ (today->instant) (t/new-duration 20 :minutes))
  (type (t/+ (t/today) (t/new-duration 20 :minutes)))
  (+ (t/today) (t/new-duration 20 :minutes))
  (+ (t/now) (t/new-duration 20 :minutes))
  (let [o (offset (t/new-duration 20 :minutes) (t/new-period 1 :days))]
    (+ (t/date-time) o))
  (let [o (offset 20 :minutes 1 :days)]
    (+ (t/date-time) o))
  (t/+ (today->instant) (t/new-period 20 :days))
  )

(comment
  (t/range (t/today) (t/tomorrow)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Formatting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO remove the use of t/format and just add support for your use cases
;; this should decrease FE bundle size
;; using  inst-breakdown helper

;; symbols here:
;; https://js-joda.github.io/js-joda/class/packages/core/src/format/DateTimeFormatter.js~DateTimeFormatter.html#static-method-ofPattern

(def default-format "eee MMM dd, yyyy")
(def default-format-w-time "eee MMM dd, yyyy @ HH:mm")
(def full-format "eeee MMM dd, yyyy")

(defn format
  "Any date-like object"
  ([] (format (t/today)))
  ([d] (format d default-format))
  ([d fmt]
   (if d
     (t/format (t/formatter fmt) (->date-time d))
     "")))

(comment
  (t/in (t/now) "Europe/Paris")
  )

(>defn format-w-time
  [d]
  [(s/or :dt date-time? :i inst?) => string?]
  (t/format (t/formatter default-format-w-time) (->date-time d)))

(>defn format-time
  "HH:mm"
  [t]
  [time? => string?]
  (let [hour   (t/hour t)
        minute (t/minute t)]
    ;; You can use (str t) to render the time but it will show highest
    ;; precision - like millis and nanos.
    (str
      (if (clojure.core/< hour 10) (str "0" hour) hour)
      ":"
      (if (clojure.core/< minute 10) (str "0" minute) minute))))

(comment
  (format-time (t/time "20:06:12"))
  (format-time (t/time "20:23")))

(defn weekday-format [d] (format d "eee MMM dd"))
(defn weekday-format-w-time [d] (format d "eee MMM dd @ HH:mm"))

(comment (weekday-format (offset-from-date (t/today) (offset 2 :days))))
(comment
  (offset-from-date (t/today) (offset 2 :days))
  (offset
    (duration 16 :hours 15 :minutes)
    (t/new-period 2 :days)
    )
  )

(defn format-full [d] (format d full-format))


(defn format-relative-date
  "d in relation to target"
  [target d]
  (cond
    (= target d) "Today"
    (date? d) (weekday-format d)
    (date-time? d) (weekday-format-w-time d)))

;; todo fix
(defn format-relative-time [target t]
  "d in relation to target"
  (cond
    (= target t) "Now"
    (time? t) (format-time t)))

(defn format-relative
  "d in relation to target"
  [target d]
  (cond
    (and (date? target) (date? d)) (format-relative-date target d)
    (and (time? target) (time? d)) (format-relative-time target d)
    :else (throw* "Unsupported types passed to format-relative: " (pr-str target) " " (pr-str d)))
  )

;(defn period->map
;  {:days (t/truncate days)})
(comment
  (format (t/today))

  (def p (t/between (t/today) (t/tomorrow)))
  (t/minutes p)
  (t/hours p)
  (t/days p)

  (t/hours p)

  (defn nanos [v] (core/nanos v))
  (defn micros [v] (core/micros v))
  (defn millis [v] (core/millis v))
  (defn seconds [v] (core/seconds v))
  (defn minutes [v] (core/minutes v))
  (defn hours [v] (core/hours v))


  (defn days [v] (core/days v))
  (defn months [v] (core/months v))
  (defn years [v] (core/years v))
  )

(>defn period
  [num units & args]
  [int? period-units? (s/* (s/cat :n int? :v period-units?)) => period?]
  (reduce (fn [acc [val units]]
            (if-let [f (get plus-period-fns units)]
              (f acc val)
              (throw (error "Unknown units passed to period: " units))))
    (t/new-period num units)
    (partition 2 args)))

(comment
  (period -1 :years 2 :days)
  (period 1 :years 2 :days)
  (.plusDays (period 1 :days) 20)
  )

;; I believe this is broken when d1 is after d2. You get a negative offset which is fine,
;; but I need to verify that the math works out.
(defn between
  "Returns a tick.util/Offset for the given {date, date-time, or time} objects"
  [d1 d2]
  (let [d1         (if (time? d1) d1 (->date-time d1))
        d2         (if (time? d2) d2 (->date-time d2))
        both-time? (and (time? d1) (time? d2))]
    (when-not
      (or
        both-time?
        (and (date-time? d1) (date-time? d2)))
      (throw (error "You must pass the same time types (date, date-time, or time) to 'between'.")))
    ;; on cljs java.time lib doesn't work for 'between' of two time objects using; duration gets around that.
    (let [duration1 (if both-time? (t/duration {:tick/beginning d1 :tick/end d2}) (t/between d1 d2))
          num-days  (t/days duration1)
          period*   (if (zero? num-days) nil (t/new-period num-days :days))
          duration* (t/- duration1 (t/new-duration num-days :days))
          duration* (if (zero? (t/seconds duration*)) nil duration*)]
      (->Offset period* duration* nil))))

(defn period-between
  "Returns a period "
  [d1 d2]
  (let [p (-period (between d1 d2))]
    (if (nil? p)
      (t/new-period 0 :days)
      p)))

(comment
  (between (+ (yesterday) (duration 24 :minutes 30 :seconds)) (t/now))
  ;; should throw b/c different types
  (between (+ (yesterday) (duration 24 :minutes 30 :seconds)) (t/time (t/now)))
  (between (t/time (+ (yesterday) (duration 24 :minutes 30 :seconds))) (t/time (t/now)))

  (t/truncate (+ (t/date) #time/offset "nil PT20H40M0.005S") :seconds)
  (t/truncate (t/duration {:tick/beginning (t/time (+ (yesterday) (duration 24 :minutes 30 :seconds))) :tick/end (t/time (t/now))})
    :seconds)
  (between (t/time (+ (yesterday) (duration 24 :minutes 30 :seconds))) (t/time (t/now)))
  (t/between (t/time (+ (yesterday) (duration 24 :minutes 30 :seconds))) (t/time (t/now)))
  (between (prior-month) (+ (yesterday) (duration 24 :minutes 30 :seconds)))
  (between (->date (prior-month)) (+ (yesterday) (duration 24 :minutes 30 :seconds)))
  )

(defn format-duration
  [du]
  (when du
    (let [[hours minutes seconds]
          (duration->hrs-mins-secs du)]
      (let [hrs  [hours (if (= 1 hours) "hour" "hours")]
            secs [seconds (if (= 1 seconds) "second" "seconds")]
            mins [minutes (if (= 1 minutes) "minute" "minutes")]
            nums (remove #(zero? (first %)) [hrs mins secs])]
        (if (empty? nums)
          "Midnight"
          (str/join " " (apply concat nums)))))))

(comment
  (format-duration (duration 0 :seconds))
  (apply concat (remove #(zero? (first %)) [[0 "minutes"] [10 "seconds"]]))
  (format-duration (duration 1 :minutes 100 :seconds))
  (->> (duration->map (duration 1 :minutes 10 :seconds))
    (remove #(zero? (val %))))
  (t/seconds (duration 1 :minutes 20 :seconds))
  (.-_seconds (duration 1 :minutes 10 :seconds))
  )

(defn period->map
  "Takes a period of time and breaks it down into units."
  [p]
  (when p
    {:days   (t/days p)
     :months (t/months p)
     :years  (t/years p)}))

(comment (period->map (t/new-period 3 :days)))

(defn format-period
  "takes period map"
  [p]
  (if-let [p (period->map p)]
    (if (= (:days p) 1)
      "day"
      (->> p
        (remove #(zero? (val %)))
        (map #(str (val %) " " (name (key %))))
        first))
    ""))

;; todo this is breaking for some reason in cljs with shadow cljs
(defn offset-from-date
  "Returns a date-time by adding the given offset to the provided ref-date."
  [ref-date offset]
  #_[date? offset? => date-time?]
  (let [period   (:period offset)
        duration (:duration offset)]
    (cond-> (->date-time ref-date)
      period (t/+ period)
      duration (t/+ duration))))

(comment
  (offset-from-date (t/today) (offset 2 :days)))

(defn format-offset
  [x]
  (cond
    (offset? x)
    (do
      ;(log/info "offset " x)
      (str
        (when (-period x) (str (format-period (-period x)) " "))
        (when (-duration x) (format-duration (-duration x)))))
    (period? x) (format-period x)
    (duration? x) (format-duration x)))

;(defn relative-str [d]
;  (let [p (t/between d (t/today))])
;  )
(comment
  (format-offset (offset 1 :days 2 :minutes))
  (offset? (offset 1 :days))
  (type (offset 1 :days))
  (format-period (t/new-period 3 :days))
  (format-period (t/new-period 3 :years)))


;; Form helpers, str-> tick types and back
;; duplicated from fulcro-utils to avoid pulling in this ns

(s/def ::str-or-num (s/or :s string? :n number?))

(defn parse-int [int-str]
  #?(:cljs (js/parseInt int-str 10)
     :clj  (Long/parseLong int-str)))

(>defn to-int
  [str-num]
  [::str-or-num => (s/or :empty #{""} :n number?)]
  (if (string? str-num)
    (let [v (str/replace str-num #"[^-0-9]" "")]
      (cond-> v
        (not= v "")
        parse-int))
    str-num))
(comment (to-int "9sss")
  (to-int "-10") (to-int "-0"))

(>defn pos-or-empty
  "Return a number if it is positive or the empty string if i is emptry string."
  [i]
  [(s/or :s string? :n number?) => ::str-or-num]
  (if (and (string? i) (empty? i))
    i
    (Math/max 0 (to-int i))))

(defn str->tick-days-period
  "Returns a string if empty or a tick period with units of days if a valid number is passed."
  [s]
  (let [i (pos-or-empty s)
        i (cond (string? i) ""
                :else i)]
    (cond-> i (number? i)
      (t/new-period :days))))

(defn str->hours-duration
  "Returns a string if empty or a tick duration with units of hours if a valid number is passed."
  [s]
  (let [i (pos-or-empty s)
        i (cond (string? i) ""
                (zero? i) 1
                :else i)]
    (cond-> i (number? i)
      (t/new-duration :hours))))

(comment
  (t/time (->instant (t/inst)))
  )

(comment
  #time/offset"P1D nil"
  #time/period"P1D",

  (period 1 :months 2 :days)
  (+ (today) (period 1 :years))
  )
