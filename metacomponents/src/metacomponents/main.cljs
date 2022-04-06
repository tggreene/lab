(ns metacomponents.main
  (:require [metacomponents.component :refer [defcomponent]]
            [re-frame.core :as rf]
            reagent.dom))

(rf/reg-sub
 :pets
 (fn [_]
   [{:name "Fido"
     :owner :alice}
    {:name "Alfred"
     :owner :bob}]))

(rf/reg-sub
 :people
 (fn [_]
   [{:name "Alice"
     :id :alice}
    {:name "Bob"
     :id :bob}]))

(defcomponent child-view
  {:subscriptions
   {:pets [:pets]}
   :component
   (fn [{:keys [pets]} owner-id]
     (prn owner-id)
     (into
      [:div
       [:p "Pets"]]
      (for [{:keys [name]} (filter (comp (partial = owner-id) :owner) pets)]
        [:p name])))})

(defcomponent view
  {:subscriptions
   {:people [:people]}
   :component
   (fn [{:keys [people]}]
     (into
      [:div
       [:p "Hello"]]
      (for [{:keys [name id]} people]
        [:<>
         [:p name]
         [child-view id]])))})

(defn app
  []
  [:div
   [view {}]
   [:h4 "Data required for `view`"]
   [:pre (pr-str (meta view))]])

(defn ^:dev/after-load start!
  []
  (reagent.dom/render [app] (.-body js/document)))

(start!)
