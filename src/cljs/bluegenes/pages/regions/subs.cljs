(ns bluegenes.pages.regions.subs
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [re-frame.core :refer [reg-sub]]
            [imcljs.entity :as entity]))

(reg-sub
 :regions/sequence-feature-types
 (fn [db]
   (let [model (get-in db [:mines (get db :current-mine) :service :model])]
     (entity/extended-by-tree model :SequenceFeature))))

(reg-sub
 :regions/sequence-feature-type-all-selected?
 (fn [db]
   (let [features  (get-in db [:regions :settings :feature-types])
         selected-features  (remove #(not (second %)) features)]
     (= (count features) (count selected-features)))))

(reg-sub
 :regions/error
 (fn [db]
   (get-in db [:regions :error])))

(reg-sub
 :regions/settings
 (fn [db]
   (get-in db [:regions :settings])))

(reg-sub
 :regions/feature-types
 :<- [:regions/settings]
 (fn [settings]
   (:feature-types settings)))

(reg-sub
 :regions/coordinates
 :<- [:regions/settings]
 (fn [settings]
   (:coordinates settings)))

(reg-sub
 :regions/strand-specific
 :<- [:regions/settings]
 (fn [settings]
   (:strand-specific settings)))

(reg-sub
 :regions/organism
 :<- [:regions/settings]
 (fn [settings]
   (:organism settings)))

(reg-sub
 :regions/extend-start
 :<- [:regions/settings]
 (fn [settings]
   (:extend-start settings)))

(reg-sub
 :regions/extend-end
 :<- [:regions/settings]
 (fn [settings]
   (:extend-end settings)))

(reg-sub
 :regions/unlock-extend
 :<- [:regions/settings]
 (fn [settings]
   (:unlock-extend settings)))

(reg-sub
 :regions/results
 (fn [db]
   (get-in db [:regions :results])))

(reg-sub
 :regions/loading
 (fn [db]
   (get-in db [:regions :loading])))

(reg-sub
 :regions/to-search
 (fn [db]
   (get-in db [:regions :to-search])))

(reg-sub
 :regions/example-search
 :<- [:current-mine]
 (fn [mine]
   (:regionsearch-example mine)))

(reg-sub
 :regions/highlighted?
 (fn [db [_ idx loc]]
   (= loc (get-in db [:regions :highlight idx]))))

(reg-sub
 :regions/query
 (fn [db]
   (get-in db [:regions :query])))

(reg-sub
 :regions/subquery
 (fn [db [_ idx]]
   (get-in db [:regions :subqueries idx])))
