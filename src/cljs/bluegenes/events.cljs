(ns bluegenes.events
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [re-frame.core :as re-frame :refer [reg-event-db reg-fx reg-event-fx dispatch subscribe inject-cofx]]
            [im-tables.events]
            [bluegenes.config :refer [server-vars]]
            [bluegenes.events.boot]
            [bluegenes.events.auth]
            [bluegenes.events.registry]
            [bluegenes.events.blog]
            [bluegenes.events.webproperties]
            [bluegenes.events.bgproperties]
            [bluegenes.components.idresolver.events]
            [day8.re-frame.http-fx]
            [day8.re-frame.forward-events-fx]
            [day8.re-frame.async-flow-fx]
            [bluegenes.pages.reportpage.events]
            [bluegenes.components.search.events]
            [bluegenes.components.navbar.events]
            [bluegenes.pages.results.enrichment.events]
            [bluegenes.pages.results.widgets.events]
            [bluegenes.components.search.events :as search-full]
            [bluegenes.pages.reportpage.events]
            [bluegenes.pages.querybuilder.events]
            [bluegenes.pages.profile.events]
            [bluegenes.pages.home.events]
            [bluegenes.pages.lists.events]
            [bluegenes.pages.developer.events]
            [bluegenes.pages.tools.events]
            [bluegenes.pages.regions.events]
            [bluegenes.components.viz.events]
            [bluegenes.effects :refer [document-title]]
            [bluegenes.components.tools.effects]
            [bluegenes.route :as route]
            [imcljs.fetch :as fetch]
            [imcljs.path :as im-path]
            [clojure.string :as str :refer [join split]]
            [cljs.core.async :refer [put! chan <! >! timeout close!]]
            [cljs-bean.core :refer [->clj]]
            [bluegenes.utils :refer [read-registry-mine]]))

;; If a requirement exists for the target panel, it will be called with the db
;; as argument and its return value decides whether the panel will be changed.
(let [requirements
      {:profile-panel #(map? (get-in % [:mines (:current-mine %) :auth :identity]))
       :admin-panel #(get-in % [:mines (:current-mine %) :auth :identity :superuser])
       :reset-password-panel #(empty? (get-in % [:mines (:current-mine %) :auth :identity]))}]
  ;; Change the main panel to a new view.
  (reg-event-fx
   :do-active-panel
   [document-title]
   (fn [{db :db} [_ active-panel panel-params evt]]
     (let [fullfill? (if-let [req-fn (requirements active-panel)]
                       (req-fn db)
                       true)]
       (if fullfill?
         (cond-> {:db (assoc db
                             :active-panel active-panel
                             :panel-params panel-params)}
           ;; Hide intro loader if this is the first panel change.
           (nil? (:active-panel db)) (assoc :hide-intro-loader nil)
           ;; Ensure that `:fetching-report?` is set to true *before* the panel change.
           (= active-panel :reportpage-panel) (assoc-in [:db :fetching-report?] true)
           ;; Dispatch any events paired with the panel change.
           evt (assoc :dispatch evt))
         {:dispatch-n [[::route/navigate ::route/home]
                       (case active-panel
                         ;; When you login while on the reset password page, just change to home page.
                         :reset-password-panel nil
                         [:messages/add
                          {:markup (case active-panel
                                     :profile-panel [:span "You need to be logged in to access this page."]
                                     :admin-panel [:span "You need to be a superuser to access this page."]
                                     [:span "You don't have access to this page."])
                           :style "warning"}])]})))))

; A buffer between booting and changing the view. We only change the view
; when the assets have been loaded
(reg-event-fx
 :set-active-panel
 (fn [{db :db} [_ active-panel panel-params evt]]
   (let [event [:do-active-panel active-panel panel-params evt]]
     (cond
       ;; If we're fetching assets then save the panel change for later.
       (:fetching-assets? db)
       {:db (update db :dispatch-after-boot (fnil conj []) event)}
       ;; If we failed to auth with mine, don't do a panel change.
       (:failed-auth? db)
       {:dispatch [:messages/add
                   {:markup [:span "Navigation has been disabled due to being unable to establish a connection with "
                             (let [current-mine (:current-mine db)
                                   mine-name (get-in db [:mines current-mine :name])]
                               [:em mine-name])
                             ". You can still use the mine switcher to connect to a different InterMine instance."]
                    :style "warning"
                    :timeout 5000}]}
       ;; Otherwise dispatch it now.
       :else {:dispatch event}))))

(reg-event-fx
 :save-state
 (fn [{db :db}]
   ;; We don't do any complex caching right now - on initial boot, these will
   ;; be loaded afresh and applied on top. It *does* mean that the assets can
   ;; be used before they are loaded.  Why isn't there caching? Because it gets
   ;; very complex deciding what and when to expire, so it's not really a
   ;; minimum use case feature.
   (let [saved-keys (select-keys db [:assets])]
     ;; Attach the client version to the saved state. This will be checked
     ;; the next time the client boots to make sure the local storage data
     ;; and the client version number are aligned.
     {:persist [:bluegenes/state
                (assoc saved-keys :version (:version @server-vars))]})))

(reg-event-fx
 :save-login
 [(inject-cofx :local-store :bluegenes/login)]
 (fn [{login :local-store} [_ mine-kw identity]]
   {:persist [:bluegenes/login (assoc login mine-kw identity)]}))

(reg-event-fx
 :remove-login
 [(inject-cofx :local-store :bluegenes/login)]
 (fn [{login :local-store} [_ mine-kw]]
   {:persist [:bluegenes/login (dissoc login mine-kw)]}))

;; There are lots of things that orchestrate the process of switching mines:
;; :bluegenes.events.registry/success-fetch-registry
;;   Makes sure that mine service data is populated. It can be empty in the
;;   case of a fresh boot where a non-default mine is selected.
;; :boot
;;   Makes sure that the async boot-flow waits for the above event when
;;   necessary, before proceeding with events that may require the data.
;; :set-current-mine
;;   Sets current-mine, fills in mine service data when it's available from the
;;   registry, and makes sure to reboot when mine is switched after booting.
;; Note that we don't rely on this event handler when booting, evident by the
;; `different-mine?` clause.
(reg-event-fx
 :set-current-mine
 [document-title]
 (fn [{db :db} [_ mine]]
   (let [mine-kw         (keyword mine)
         different-mine? (not= mine-kw (:current-mine db))
         not-home?       (not= (:active-panel db) :home-panel)
         in-mines?       (contains? (:mines db) mine-kw)
         registry-mine   (get-in db [:registry mine-kw])
         conf-mine       (get-in db [:env :mines mine-kw])
         active-flow?    (some? (:boot-flow db))]
     (if different-mine?
       (cond-> {:db (assoc db
                           :current-mine mine-kw
                           :fetching-assets? true)
                :visual-navbar-minechange []
                ;; Abort all active requests.
                :im-chan {:abort-active true}
                ;; Start the timer for showing the loading dialog.
                :mine-loader true
                ;; Switching mine always involves a reboot.
                :dispatch-n [[:messages/clear]
                             [:reboot]]}
         ;; Add mine if it's missing.
         (not in-mines?) (assoc-in [:db :mines mine-kw]
                                   (or conf-mine (read-registry-mine registry-mine)))
         ;; Switch to home page.
         not-home? (update :db assoc
                           :active-panel :home-panel
                           :panel-params nil)
         ;; We are being a bit naughty here, manually pulling the plug
         ;; on the async-flow, but it's the only way to abruptly halt
         ;; an in-progress flow (we don't want two overlapping!).
         active-flow? (-> (update :db dissoc :boot-flow)
                          (assoc
                           :forward-events {:unregister :async/custom-flow}
                           :deregister-event-handler :async/custom-flow)))
       ;; It's probably not a good idea to put something in the following branch.
       ;; It will run on initial boot, but you can't tell exactly when, and it
       ;; might run multiple times.
       {}))))

(reg-event-db
 :handle-suggestions
 (fn [db [_ results]]
   (-> db
       (assoc :suggestion-results (:results results))
       (dissoc :suggestion-error))))

(reg-event-fx
 :bounce-search
 (fn [{db :db} [_ term]]
   (let [service (get-in db [:mines (get db :current-mine) :service])]
     (if (empty? term)
       {:db (assoc db
                   :search-term term
                   :suggestion-results nil)}
       {:db (assoc db :search-term term)
        :im-chan {:chan (fetch/quicksearch service term {:size 5})
                  :abort :quicksearch
                  :on-success [:handle-suggestions]
                  :on-failure [:bounce-search-failure]}}))))

(reg-event-db
 :bounce-search-failure
 (fn [db [_ res]]
   (assoc db :suggestion-error
          {:type "failure"
           :message (get-in res [:body :error])})))

(reg-event-db
 :cache/store-organisms
 (fn [db [_ res]]
   (assoc-in db [:cache :organisms] (:results res))))

(reg-event-fx
 :cache/fetch-organisms-failure
 (fn [{db :db} [_ res]]
   {:log-error ["Failed query for organisms" res]}))

(reg-event-fx
 :cache/fetch-organisms
 (fn [{db :db}]
   (let [service (get-in db [:mines (:current-mine db) :service])
         q {:from "Organism"
            :select ["name" "taxonId" "species" "shortName" "genus" "commonName"]}]
     (if (contains? service :model)
       {:im-chan {:chan (fetch/rows service q {:format "jsonobjects"})
                  :on-success [:cache/store-organisms]
                  :on-failure [:cache/fetch-organisms-failure]}}
       ;; No point running this query if we failed to fetch the model.
       {}))))

(reg-event-db
 :cache/store-possible-values
 (fn [db [_ mine-kw view-vec results]]
   (if (false? results)
     (assoc-in db [:mines mine-kw :possible-values view-vec] false)
     (assoc-in db [:mines mine-kw :possible-values view-vec]
               (not-empty (sort (map :item (:results results))))))))

(reg-fx
 :cache/fetch-possible-values-fx
 (fn [{:keys [mine-kw service store-in summary-path query]}]
   (let [sum-chan (fetch/unique-values service query summary-path 7000)]
     (go
       (dispatch [:cache/store-possible-values mine-kw summary-path (<! sum-chan)])))))

(defn get-active-constraints [db]
  (case (:active-panel db)
    :templates-panel (get-in db [:components :template-chooser :selected-template :where])
    nil))

(reg-event-fx
 :cache/fetch-possible-values
 (fn [{db :db} [_ path model ?constraints]]
   (let [constraints (if (false? ?constraints)
                       (get-active-constraints db)
                       ?constraints)
         model (assoc model :type-constraints (filterv :type constraints))
         mine (get-in db [:mines (get db :current-mine)])
         split-path (split path ".")
         existing-value (get-in db [:mines (get db :current-mine) :possible-values split-path])]
     (if (and (nil? existing-value) (not (im-path/class? model path)))
       {:cache/fetch-possible-values-fx {:service (get mine :service)
                                         :query (merge
                                                 {:from (first split-path)
                                                  :select [path]}
                                                 (when-let [consts (seq (remove #(= path (:path %)) constraints))]
                                                   {:where (vec consts)}))
                                         :mine-kw (get mine :id)
                                         :summary-path path}}
       {:dispatch [:cache/store-possible-values (get mine :id) path false]}))))

(reg-event-db
 :flag-invalid-token
 (fn [db]
   (assoc db :invalid-token? true)))

(reg-event-fx
 :clear-invalid-token
 (fn [{db :db} [_]]
   (let [current-mine (:current-mine db)]
     {:db (-> db
              ;; Set token to nil so we fetch a new one.
              (assoc-in [:mines current-mine :service :token] nil)
              ;; Clear any auth/identity present if the user has logged in.
              (update-in [:mines current-mine] dissoc :auth)
              ;; Clear the invalid token flag.
              (dissoc :invalid-token?)
              (route/force-controllers-rerun))
      :dispatch-n [[:remove-login current-mine]
                   [:reboot]]})))

(reg-event-db
 :scramble-tokens
 (fn [db]
   (assoc-in db [:mines (:current-mine db) :service :token] "faketoken")))

(reg-event-fx
 :messages/add
 (fn [{db :db} [_ props]]
   (let [id (gensym)
         msg (assoc props
                    :id id
                    :when (.getTime (js/Date.)))]
     {:db (assoc-in db [:messages id] msg)
      :message (select-keys msg [:id :timeout])})))

(reg-event-db
 :messages/remove
 (fn [db [_ id]]
   (update db :messages dissoc id)))

(reg-event-db
 :messages/clear
 (fn [db]
   (update db :messages empty)))

(defn ^:export scrambleTokens []
  (dispatch [:scramble-tokens]))

(reg-event-fx
 :show-mine-loader
 (fn [{db :db}]
   {:db (assoc db :show-mine-loader? true)
    :hide-intro-loader nil}))

(reg-event-db
 :hide-mine-loader
 (fn [db]
   (assoc db :show-mine-loader? false)))

(reg-event-fx
 :scroll-to-top
 (fn [{db :db} [_]]
   {:scroll-to-top {}}))

(reg-event-fx
 :handle-permanent-url
 (fn [{db :db} [_ mine lookup-string]]
   (let [[object-type identifier] (split lookup-string #":")
         object-type (str/capitalize object-type)
         service (get-in db [:mines (keyword mine) :service])
         q {:from object-type
            :select [(str object-type ".id")]
            :where [{:path object-type
                     :op "LOOKUP"
                     :value identifier}]}]
     {:im-chan {:chan (fetch/rows service q)
                :on-success [:redirect-to-report-page lookup-string]
                :on-failure [:notify-invalid-permanent-url lookup-string]}})))

(reg-event-fx
 :redirect-to-report-page
 (fn [{db :db} [_ lookup-string res]]
   (if-let [object-id (get-in res [:results 0 0])]
     {:dispatch [::route/navigate ::route/report {:type (:rootClass res)
                                                  :id object-id}]}
     {:dispatch [:notify-invalid-permanent-url lookup-string res]})))

(reg-event-fx
 :notify-invalid-permanent-url
 (fn [{db :db} [_ lookup-string res]]
   {:dispatch-n [[::route/navigate ::route/home]
                 [:messages/add
                  {:markup [:span "The object referenced by the permanent URL " [:em lookup-string] " does not seem to be in the database anymore. "
                            (when-let [err (not-empty (get-in res [:body :error]))]
                              [:code err])]
                   :style "warning"
                   :timeout 0}]]}))
