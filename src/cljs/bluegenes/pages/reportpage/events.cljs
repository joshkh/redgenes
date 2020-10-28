(ns bluegenes.pages.reportpage.events
  (:require [re-frame.core :as re-frame :refer [reg-event-db reg-event-fx dispatch]]
            [imcljs.fetch :as fetch]
            [bluegenes.components.tools.events :as tools]
            [bluegenes.effects :refer [document-title]]
            [clojure.string :as string]
            [bluegenes.pages.reportpage.utils :as utils]
            [bluegenes.route :as route]
            [goog.dom :as gdom]
            [oops.core :refer [oget ocall]]))

(reg-event-fx
 :handle-report-summary
 [document-title]
 (fn [{db :db} [_ summary]]
   {:db (-> db
            (assoc-in [:report :summary] summary)
            (assoc-in [:report :title] (utils/title-column summary))
            (assoc-in [:report :active-toc] utils/pre-section-id)
            (assoc :fetching-report? false))
    :dispatch-n [[:viz/run-queries]]}))

(reg-event-fx
 :fetch-report
 (fn [{db :db} [_ mine type id]]
   (let [service (get-in db [:mines mine :service])
         attribs (->> (get-in service [:model :classes (keyword type) :attributes])
                      (mapv (comp #(str type "." %) :name val)))
         q       {:from type
                  :select attribs
                  :where [{:path (str type ".id")
                           :op "="
                           :value id}]}]
     {:im-chan {:chan (fetch/rows service q {:format "json"})
                :on-success [:handle-report-summary]}})))

(reg-event-db
 :handle-fasta
 (fn [db [_ fasta]]
   (cond-> db
     ;; If there's no FASTA the API will return "Nothing was found for export".
     (string/starts-with? fasta ">")
     (assoc-in [:report :fasta] fasta))))

(reg-event-fx
 :fetch-fasta
 (fn [{db :db} [_ mine id]]
   (let [service (get-in db [:mines mine :service])
         q {:from "Gene"
            :select ["Gene.id"]
            :where [{:path "Gene.id"
                     :op "="
                     :value id}]}]
     {:im-chan {:chan (fetch/fasta service q)
                :on-success [:handle-fasta]}})))

(defn class-has-dataset? [model-classes class-kw]
  (contains? (get-in model-classes [class-kw :collections])
             :dataSets))

(reg-event-fx
 :load-report
 (fn [{db :db} [_ mine type id]]
   (let [entity {:class type
                 :format "id"
                 :value id}]
     {:db (-> db
              (assoc :fetching-report? true)
              (dissoc :report)
              (assoc-in [:tools :entities (keyword type)] entity))
      :dispatch-n [[:fetch-report (keyword mine) type id]
                   (when (= type "Gene")
                     [:fetch-fasta (keyword mine) id])
                   [::fetch-lists (keyword mine) id]
                   (when (class-has-dataset? (get-in db [:mines (keyword mine) :service :model :classes])
                                             (keyword type))
                     [::fetch-sources (keyword mine) type id])]})))

(reg-event-fx
 ::fetch-lists
 (fn [{db :db} [_ mine-kw id]]
   (let [service (get-in db [:mines mine-kw :service])]
     {:im-chan {:chan (fetch/lists-containing service {:id id})
                :on-success [::handle-lists]}})))

(reg-event-db
 ::handle-lists
 (fn [db [_ lists]]
   (assoc-in db [:report :lists] lists)))

(reg-event-fx
 ::fetch-sources
 (fn [{db :db} [_ mine-kw class id]]
   (let [service (get-in db [:mines mine-kw :service])
         q {:from class
            :select ["dataSets.description"
                     "dataSets.url"
                     "dataSets.name"
                     "dataSets.id"]
            :where [{:path (str class ".id")
                     :op "="
                     :value id}]}]
     {:im-chan {:chan (fetch/rows service q {:format "json"})
                :on-success [::handle-sources]}})))

(defn views->results
  "Convert a `fetch/rows` response map into a vector of maps from keys
  corresponding to the tail of the view, and values being the pair of the key.
  Ex. [{:name 'BioGRID interaction data set' :url 'http://www.thebiogrid.org/downloads.php'}
       {:name 'Panther orthologue and paralogue predictions' :url 'http://pantherdb.org/'}]"
  [{:keys [views results] :as _res}]
  (let [concise-views (map #(-> % (string/split #"\.") last keyword)
                           views)]
    (mapv (partial zipmap concise-views)
          results)))

(reg-event-db
 ::handle-sources
 (fn [db [_ res]]
   (assoc-in db [:report :sources] (views->results res))))

(reg-event-fx
 ::open-in-region-search
 (fn [{db :db} [_ chromosome-location]]
   {:dispatch-n [[::route/navigate ::route/regions]
                 [:regions/set-to-search chromosome-location]
                 [:regions/run-query]]}))

(reg-event-db
 ::set-active-toc
 (fn [db [_ active-toc-id]]
   (assoc-in db [:report :active-toc] active-toc-id)))

(defn scrollspy
  "Returns a function (partially applied with a seq of string element IDs) to
  be called on document scroll. Will go through every element until it finds
  one that hasn't been scrolled passed, then dispatches an event with its ID."
  [ids]
  ;; Predefined top section is always initial value.
  (let [last-scrolled-id* (atom utils/pre-section-id)]
    (fn [_evt]
      (loop [ids ids]
        (when (seq ids)
          (let [id (first ids)
                ;; An element can be nil if its section is collapsed, in which
                ;; case we should skip to the next ID.
                top (some-> (gdom/getElement id)
                            (ocall :getBoundingClientRect)
                            (oget :top))]
            (if (pos? ^number top) ; We'll get a nil warning without the typehint.
              (when (not= id @last-scrolled-id*)
                (reset! last-scrolled-id* id)
                (dispatch [::set-active-toc id]))
              (recur (rest ids)))))))))

(defn categories->ids
  "Returns a seq of all the IDs of sections/categories and their children, in
  order of placement from top to bottom."
  [cats]
  (concat [(symbol utils/pre-section-id)] ; Predefined section on the report page.
          (mapcat #(cons (:id %) (map :id (:children %))) cats)))

;; Working around crappy DOM API... (we have to create an anonymous function to
;; pass arguments to the listener, and keep a reference to that so we can
;; remove it later)
(defonce scrollspy-fn* (atom nil))

(reg-event-fx
 ::start-scroll-handling
 (fn [{db :db} [_]]
   ;; TODO reading categories from db like here is not how we want to do it......
   (let [ids (->> (get-in db [:admin :categories (or (get-in db [:admin :categorize-class])
                                                     :default)])
                  (categories->ids)
                  (map str))]
     (.addEventListener js/window "scroll" (reset! scrollspy-fn* (scrollspy ids)))
     {})))

(reg-event-fx
 ::stop-scroll-handling
 (fn [_ [_]]
   (.removeEventListener js/window "scroll" @scrollspy-fn*)
   {}))
