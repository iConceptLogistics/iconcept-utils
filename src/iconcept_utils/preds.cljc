(ns iconcept-utils.preds
  (:require [clojure.string :as s]
            ;;
            #?@(:cljs
                [[goog.string :as gstring]
                 [goog.string.format]])
            ;;
            #_[clojure.set    :refer [rename-keys]]
            #_[clojure.pprint :refer [pprint]])
  #?(:clj (:import [java.time Instant LocalDate])))

(defn local-date?
  [x]
  (instance? LocalDate x))

(defn instant?
  [x]
  (instance? Instant x))

(defn non-blank-string?
  [x]
  (and (string? x)
       (not (s/blank? x))))

(defn non-blank-upper-string?
  [x]
  (and (non-blank-string? x)
       (= x (s/upper-case x))))

(defn non-blank-lower-string?
  [x]
  (and (non-blank-string? x)
       (= x (s/lower-case x))))

(defn non-blank-svg-string?
  [x]
  (-> (and (non-blank-string? x)
           (re-find #"(?i)<svg\p{Zs}+" x)
           (re-find #"(?i)\bxmlns=\"http://www.w3.org/2000/svg" x))
      boolean))

(defn string-tags?
  [x]
  (and (set? x)
       (every? non-blank-string? x)
       (< 0 (count x))))

(defn positive-integer?
  [x]
  (and (integer? x) (< 0 x)))

(defn edn-map?
  [x]
  ;; FIXME: see if we can check for a `readable` map, ie references to
  ;; functions can't be read, if they are in the map then it's not an
  ;; ednable map.
  (map? x))

(defn postcode?
  [x]
  (and (string? x)
       (re-matches #"^[0-9]{4}$" x)))

(def +email-re+ (-> (str "(?i)[a-z0-9!#$%&'*+/=?^_`{|}~-]+"
                         "(?:\\.[a-z0-9!#$%&'*+/=?" "^_`{|}~-]+)*"
                         "@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+"
                         "[a-z0-9](?:[a-z0-9-]*[a-z0-9])?")
                    re-pattern))
(defn email?
  "Returns true if the email address is valid, based on RFC 2822. Email
  addresses containing quotation marks or square brackets are considered
  invalid, as this syntax is not commonly supported in practise. The domain of
  the email address is not checked for validity."
  [x]
  ;; Snarfed from: https://github.com/weavejester/valip/blob/master/src/valip/predicates.clj
  (and (non-blank-string? x)
       (-> (re-matches +email-re+ x)
           boolean)))

(def +e164-re+ #"^\+[1-9]\d{10,14}$")
(defn e164?
  [x]
  (and (non-blank-string? x)
       (-> (re-matches +e164-re+ x)
           boolean)))

(defn year?
  [x]
  (and (non-blank-string? x)
       (-> (re-matches #"^[0-9]{4}$" x)
           boolean)))
