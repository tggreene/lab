(ns metacomponents.component
  (:require clojure.walk))

(defonce registrations
  (atom {}))

(defn fn-body
  [form]
  (if (= 'fn (first form))
    (drop 2 form)
    form))

(def clojure-syms
  (set (keys (ns-publics 'clojure.core))))

(defn get-registered
  [form]
  (let [syms (->> (clojure.walk/postwalk
                   (fn [x]
                     (cond
                       (list? x)
                       (remove nil? (fn-body x))

                       (map? x)
                       (into [] x)

                       (coll? x)
                       x

                       (symbol? x)
                       (cond
                         (namespace x)
                         x

                         (clojure-syms x)
                         nil

                         :else
                         (symbol (str *ns*) (str x)))
                       :else nil))
                   (fn-body form))
                  flatten
                  (remove nil?))]
    (->> syms
         (map #(vector (list 'quote %) (get @registrations %)))
         (filter (fn [[_ v]] v))
         (into {}))))

(defmacro defcomponent
  {:style/indent 1}
  [name {:keys [subscriptions component]}]
  (let [reg-key (symbol (str *ns*) (str name))
        children (get-registered component)]
    (swap! registrations assoc reg-key (cond-> {:subscriptions subscriptions}
                                         (seq? children) (assoc :children children)))
    `(def ~name
       (with-meta
         (fn [& args#]
           (apply ~component
                  ~(->> subscriptions
                        (map (fn [[k v]]
                               [k (list 'deref (list 're-frame.core/subscribe v))]))
                        (into {}))
                  args#))
         ~{:meta-component true
           :children children
           :subscriptions subscriptions}))))
