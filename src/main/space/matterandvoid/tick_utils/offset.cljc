(ns space.matterandvoid.tick-utils.offset
  (:refer-clojure :exclude [< > >= <= + - format time])
  (:require
    #?(:cljs [cljs.reader])
    #?(:clj [taoensso.nippy :as nippy])
    [clojure.string :as str]
    [clojure.set :as set]
    [tick.core :as t]
    [tick.protocols :refer [ITimeComparison ITimeArithmetic]])
  #?(:clj (:import [java.io Writer])))

(defn error [& msg]
  #?(:cljs (js/Error. (apply str msg))
     :clj (RuntimeException. (apply str msg))))

(def date-type? (some-fn inst? t/instant? t/date? t/date-time?))
(def time-type? (some-fn inst? t/instant? t/time? t/date? t/date-time?))

(declare offset + - add-offset subtract-offset offset-type? -period -duration)

;; Combines duration and period into one abstration
;; Todo I should lock down the semantics for this - the intention is that the duration is always less than 24 hours
;; and the period is at least one day.

(def to-str (fnil str "nil"))

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
     (- [offset other] (subtract-offset offset other))

     Object
     (toString [this] (str (to-str (-period this)) " " (to-str (-duration this)))))
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
     (toString [this] (str (to-str (-period this)) " " (to-str (-duration this))))

     ITimeArithmetic
     (+ [offset other] (add-offset offset other))
     (- [offset other] (subtract-offset offset other))))

(defn get-offset-class []
  #?(:clj (class (Offset. nil nil nil))
     :cljs Offset))

;; If using Offset in clj instead of Object I would get the following error in certain use cases at the repl:
;; Error printing return value (ClassCastException) at dv.tick-util/-duration (tick_util.cljc:131).
;; class dv.tick_util.Offset cannot be cast to class dv.tick_util.Offset
;; (dv.tick_util.Offset is in unnamed module of loader clojure.lang.DynamicClassLoader @62b15496

(def period-units #{:days :weeks :months :years})
(def duration-units #{:nanos :micros :millis :seconds :minutes :hours})
(def offset-units (set/union period-units duration-units))
(def offset-units? offset-units)
(def period-units? period-units)
(def duration-units? duration-units)

(defn offset
  "Offset from mix and match units of duration and period"
  ([val]
   ;[(s/or :period period? :duration duration?) => offset?]
   (cond
     (t/period? val) (->Offset val nil nil)
     (t/duration? val) (->Offset nil val nil)
     :else (throw (error "Unsupported type passed to offset: " (pr-str val)))))

  ([val units]
   ;[(s/or :int integer? :period period? :duration duration? :nil nil?)
   ; (s/or :units offset-units? :period period? :duration duration? :nil nil?) => offset?]
   (cond
     (and (t/duration? val) (nil? units)) (->Offset units val nil)
     (and (t/period? val) (nil? units)) (->Offset val units nil)
     (and (nil? val) (t/period? units)) (->Offset units val nil)
     (and (nil? val) (t/duration? units)) (->Offset val units nil)
     (and (t/period? val) (t/duration? units)) (->Offset val units nil)
     (and (t/duration? val) (t/period? units)) (->Offset units val nil)
     (and (integer? val) (duration-units? units)) (->Offset nil (t/new-duration val units) nil)
     (and (integer? val) (period-units? units)) (->Offset (t/new-period val units) nil nil)
     :else (throw (error (str "Unknown units passed to offset: " units)))))

  ([val units & others]
   (let [{period-vals true duration-vals false}
         (group-by (fn [[_ u]] (contains? period-units u)) (partition 2 (list* val units others)))
         p-units (map second period-vals)
         d-units (map second duration-vals)]
     (assert (every? period-units? p-units) (str "Unknown period units passed to offset: " (pr-str p-units)))
     (assert (every? duration-units? d-units) (str "Unknown duration units passed to offset: " (pr-str d-units)))
     (let [p (reduce
               (fn [ac [v u]] (t/+ ac (t/new-period v u)))
               (t/new-period 0 :days)
               period-vals)
           d (reduce
               (fn [ac [v u]] (t/+ ac (t/new-duration v u)))
               (t/new-duration 0 :seconds)
               duration-vals)]
       (->Offset p d nil)))))

(comment
  (offset (t/new-period 100 :years) (t/new-duration 10 :minutes))
  (offset (t/new-duration 10 :minutes) (t/new-period 100 :years))
  (offset 7 :hours 30 :minutes)
  ;; should throw:
  (offset 10 :days 2 :hours 40 :minutes 30 :seconds 10 :thirds)
  (offset 10 :days 2 :hours 40 :minutes 30 :seconds)
  (offset 5 :hours))

(defn -period [offset] (.-period ^{:tag #?(:cljs clj :clj Offset)} offset))
(defn -duration [offset] (.-duration ^{:tag #?(:cljs clj :clj Offset)} offset))

(defn offset? [d] (instance? Offset d))
(def offset-type? (some-fn offset? t/duration? t/period?))

(defn period-duration-pair
  "Takes either order of period/duration return [period duration]
  with nils for either missing"
  [v1 v2]
  {:malli/schema [:=> [:cat [::period-or-duration ::period-or-duration]] [:maybe ::offest]]}
  (cond
    (and (t/duration? v1) (or (t/period? v2) (nil? v2))) [v2 v1]
    (and (or (t/period? v1) (nil? v1)) (t/duration? v2)) [v1 v2]

    (and (or (t/duration? v1) (nil? v1)) (t/period? v2)) [v2 v1]
    (and (t/period? v1) (or (t/duration? v2) (nil? v2))) [v1 v2]
    :else nil))

(defn add-offset*
  [d ^Offset offset]
  ;[time-type? offset? => time-type?]
  (let [duration (-duration offset)
        period   (-period offset)]
    (cond-> d
      duration (t/+ duration)
      period (t/+ period))))


(defn add-offset
  [v1 v2]
  ;[(s/or :time time-type? :offset offset-type?)
  ; (s/or :time time-type? :offset offset-type?)
  ; => (s/or :time time-type? :offset offset-type?)]
  (cond
    (and (time-type? v1) (offset? v2))
    (add-offset* v1 v2)

    (and (offset? v1) (time-type? v2))
    (add-offset* v2 v1)

    (and (offset? v1) (t/period? v2))
    (offset (t/+ (-period v1) v2) (-duration v1))

    (and (t/period? v1) (offset? v2))
    (offset (t/+ (-period v2) v1) (-duration v2))

    (and (offset? v1) (t/duration? v2))
    (offset (-period v1)
      (if (-duration v1) (t/+ (-duration v1) v2) v2))

    (and (t/duration? v1) (offset? v2))
    (offset (-period v2)
      (if (-duration v2) (t/+ (-duration v2) v1) v1))

    (and (offset? v1) (offset? v2))
    (offset
      (+ (-period v1) (-period v2))
      (+ (-duration v1) (-duration v2)))))

(defn with-period [offset' period]
  (offset period (-duration offset')))

(defn with-duration [offset' duration]
  (offset (-period offset') duration))

(comment
  (add-offset (t/date) (offset 2 :days 5 :minutes))
  (add-offset (offset 2 :days 5 :minutes) (offset 2 :days 5 :minutes))
  (t/+ (duration 5 :minutes) (offset (t/new-duration 25 :minutes)))
  (t/+ (t/now) (offset (t/new-duration 25 :minutes)))
  (t/+ (t/now) (t/new-duration 25 :minutes)))

(defn subtract-offset*
  [d ^Offset offset]
  ;[time-type? offset? => time-type?]
  (let [duration (-duration offset)
        period   (-period offset)]
    (cond-> d
      duration (t/- duration)
      period (t/- period))))

(defn subtract-offset
  [v1 v2]
  ;[(s/or :time time-type? :offset offset-type?)
  ; (s/or :time time-type? :offset offset-type?)
  ; => (s/or :time time-type? :offset offset-type?)]
  (cond
    (and (time-type? v1) (offset? v2))
    (subtract-offset* v1 v2)

    (and (offset? v1) (time-type? v2))
    (subtract-offset* v2 v1)

    (and (offset? v1) (t/period? v2))
    (offset (t/- (-period v1) v2) (-duration v1))

    (and (t/period? v1) (offset? v2))
    (offset (t/- (-period v2) v1) (-duration v2))

    (and (offset? v1) (t/duration? v2))
    (offset (-period v1) (t/- (-duration v1) v2))

    (and (t/duration? v1) (offset? v2))
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

(defn print-offset [^Offset o]
  (str "#time/offset \"" (to-str (-period o)) " " (to-str (-duration o)) "\""))

#?(:cljs
   (extend-protocol IPrintWithWriter
     Offset (-pr-writer [d writer _opts] (-write writer (print-offset d)))))

#?(:clj (defmethod print-method Offset [c ^Writer w] (.write w ^String ^String (print-offset c))))
#?(:clj (defmethod print-dup Offset [c ^Writer w] (.write w ^String (print-offset c))))

(comment (offset? (->Offset (t/new-period 1 :days) (t/new-duration 1 :hours) nil))
  (-duration (offset 2 :days 5 :minutes))
  (t/+ (t/date-time) #time/duration "PT5M")
  (t/+
    (t/date)
    (-duration (offset 2 :days 5 :minutes)))
  (add-offset*
    (t/date) (offset 2 :days 5 :minutes)))

#?(:clj
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
     ))

;; (2020-09-01) I'm not sure why but you have to return the code as data instead of being evaluated at read time.
;; This was figured out by looking at the code for the time-literals library.

(defn read-offset-transit
  "Period is printed first then duration."
  [offset-str]
  (let [offset-str (cond->
                     offset-str
                     (str/includes? offset-str "#t/offset ")
                     (str/replace "#t/offset " "")

                     (str/includes? offset-str "#time/offset ")
                     (str/replace "#time/offset " "")

                     'always
                     (str/replace "\"" ""))
        [period duration] (str/split offset-str #" ")
        period*    (if (= "nil" period) nil (. java.time.Period parse period))
        duration*  (if (= "nil" duration) nil (. java.time.Duration parse duration))]
    (->Offset period* duration* nil)))

(defn read-offset-edn
  "Period is printed first then duration."
  [offset-str]
  (let [[period duration] (str/split offset-str #" ")]
    `(let [period#   (if (= "nil" ~period) nil (. java.time.Period ~'parse ~period))
           duration# (if (= "nil" ~duration) nil (. java.time.Duration ~'parse ~duration))]
       (Offset. period# duration# nil))))

#?(:cljs (cljs.reader/register-tag-parser! 'time/offset read-offset-edn))

