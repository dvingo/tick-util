(ns space.matterandvoid.time-literals-transit
  (:require
    [cognitect.transit :as transit]
    #?(:cljs
       [java.time :refer
        [Period LocalDate LocalDateTime ZonedDateTime OffsetTime OffsetDateTime
         Instant ZoneId DayOfWeek LocalTime Month MonthDay Duration Year YearMonth]])
    [time-literals.read-write :as rw]
    [space.matterandvoid.tick-utils.offset :as time.offset])
  #?(:clj (:import [java.io
                    ByteArrayInputStream ByteArrayOutputStream Writer]
                   [space.matterandvoid.tick_utils.offset Offset]
                   [java.time
                    Period LocalDate LocalDateTime ZonedDateTime OffsetTime
                    OffsetDateTime Instant ZoneId DayOfWeek LocalTime Month MonthDay Duration Year YearMonth])))

;; see https://github.com/henryw374/time-literals/issues/2
;; and https://gist.github.com/jjttjj/6bc0b62ef1dbf29c1c69ea22f8eb7f55
;; uses prefix str "t/" for brevity instead of "time/"

(def time-classes
  {'date             LocalDate
   'time             LocalTime
   'date-time        LocalDateTime
   'zoned-date-time  ZonedDateTime
   'zone             ZoneId
   'instant          Instant
   'offset-time      OffsetTime
   'offset-date-time OffsetDateTime
   'duration         Duration
   'period           Period
   'offset           #?(:clj Offset :cljs time.offset/Offset)
   'year             Year
   'year-month       YearMonth
   'day-of-week      DayOfWeek
   'month-day        MonthDay
   'month            Month})

(def write-handlers
  (into {}
    (for [[tick-class host-class] time-classes]
      [host-class (transit/write-handler (constantly (str "t/" (name tick-class))) str)])))

(def read-handlers
  (into {}
    (for [[sym reader-fn] (assoc time-literals.read-write/tags
                            'time/offset time.offset/read-offset-transit)]
      [(str "t/" (name sym)) (transit/read-handler reader-fn)])))

;(comment
;  ;; cljs test
;  (defn round-it [d]
;    (let [reader (transit/reader :json-verbose {:handlers read-handlers})
;          writer (transit/writer :json-verbose {:handlers write-handlers})]
;      (->> d
;        (transit/write writer)
;        (transit/read reader))))
;  (transit/read
;    (transit/reader :json-verbose {:handlers read-handlers})
;    (transit/write
;      (transit/writer :json-verbose {:handlers write-handlers})
;      (time.offset/offset 10 :days 20 :seconds)
;      ))
;  (round-it (time.offset/offset 10 :days))
;  )

;(comment
;  (clojure.edn/read-string {:readers (assoc rw/tags 'time/offset time.offset/read-offset-transit)} (pr-str (time.offset/offset (t/new-duration 1 :hours))))
;  (clojure.edn/read-string {:readers (assoc rw/tags 'time/offset time.offset/read-offset-transit)} (pr-str (time.offset/offset (t/new-duration 1 :hours) (t/new-period 2 :days))))
;  (clojure.edn/read-string {:readers (assoc rw/tags 'time/offset time.offset/read-offset-transit)} (pr-str (time.offset/offset (t/new-period 2 :days))))
;  (time.offset/offset (t/new-duration 1 :hours)))
