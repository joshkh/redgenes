(ns bluegenes.components.idresolver.events
  (:require [re-frame.core :refer [reg-event-db reg-event-fx]]
            [bluegenes.effects :as fx]
            [oops.core :refer [oget]]
            [imcljs.fetch :as fetch]
            [imcljs.save :as save]
            [imcljs.path :as path]
            [clojure.string :as string]
            [bluegenes.route :as route]
            [bluegenes.interceptors :refer [datetime]]
            [bluegenes.time :refer [format-list-date]]))

(reg-event-db
 ::stage-files
 (fn [db [_ js-FileList]]
   (update-in db [:idresolver :stage :files] concat (array-seq js-FileList))))

(reg-event-db
 ::unstage-file
 (fn [db [_ js-File]]
   (update-in db [:idresolver :stage :files] #(remove (partial = js-File) %))))

(reg-event-db
 ::update-textbox-identifiers
 (fn [db [_ value]]
   (assoc-in db [:idresolver :stage :textbox] value)))

(reg-event-fx
 ::parse-staged-files
 (fn [{db :db} [_ js-Files text options]]
   {:db (assoc-in db [:idresolver :stage :status] {:action :parsing})
    ::fx/http {:uri "/api/ids/parse"
               :method :post
               :multipart-params
               (cond-> (map (fn [f] [(oget f :name) f]) js-Files)
                 ; After creating multipart params for files,
                 ; create one for text
                 ; if there's a value
                 (not (string/blank? text))
                 (conj ["text" text])
                 ; And also a param for the case sensitive option
                 true (conj ["caseSensitive" (:case-sensitive options)]))
               :on-success [::store-parsed-response options]}}))

(reg-event-fx
 ::store-parsed-response
 (fn [{db :db} [_ options {:keys [identifiers] :as response}]]
   {:db (update db :idresolver
                #(-> %
                     (assoc-in [:stage :status :action] nil)
                     (assoc-in [:stage :flags :parsed] true)
                     (assoc :to-resolve response)))
    :dispatch-n [[::resolve-identifiers
                  {:identifiers identifiers
                   :case-sensitive (:case-sensitive options)
                   :type (:type options)
                   :extra (:organism options)}]
                 [::route/navigate ::route/upload-step {:step "save"}]]}))

(reg-event-fx
 ::resolve-identifiers
 (fn [{db :db} [_ body]]
   (let [service (get-in db [:mines (get db :current-mine) :service])]
     {:db (-> db
              (assoc-in [:idresolver :stage :view] :review)
              (assoc-in [:idresolver :stage :options :review-tab] :matches)
              (update :idresolver dissoc :response :error))
      :im-chan {:chan (fetch/resolve-identifiers service body)
                :on-success [::store-identifiers]
                :on-failure [::resolve-identifiers-failure]}})))

(reg-event-db
 ::resolve-identifiers-failure
 (fn [db [_ res]]
   (assoc-in db [:idresolver :error] res)))

(defn default-list-name [data-type organism now]
  (str data-type
       " list for "
       (if (string/blank? organism) "all organisms" organism)
       " "
       (format-list-date now)))

(reg-event-fx
 ::finished-review
 (datetime)
 (fn [{db :db now :datetime}]
   (let [{:keys [type organism]} (get-in db [:idresolver :stage :options])]
     {:db (-> db
              (assoc-in [:idresolver :stage :flags] {:reviewed true})
              (assoc-in [:idresolver :stage :view] :review)
              (assoc-in [:idresolver :save :list-name]
                        (default-list-name type organism now)))})))

(reg-event-fx
 ::store-identifiers
 (datetime)
 (fn [{db :db now :datetime} [_ response]]
   (let [{:keys [type organism]} (get-in db [:idresolver :stage :options])]
     {:db (-> db
              (assoc-in [:idresolver :response] response)
              (assoc-in [:idresolver :stage :view] :review)
              (assoc-in [:idresolver :save :list-name]
                        (default-list-name type organism now)))})))

(reg-event-db
 ::set-view
 (fn [db [_ view]]
   (assoc-in db [:idresolver :stage :view] view)))

(reg-event-db
 ::toggle-case-sensitive
 (fn [db [_ response]]
   (update-in db [:idresolver :stage :options :case-sensitive?] not)))

(reg-event-db
 ::update-type
 (fn [db [_ model value]]
   ;; why are we disabling organism?
   (let [disable-organism? (when (not= value "_")
                             (not (contains? (path/relationships model value)
                                             :organism)))
         mine-details      (get-in db [:mines (get db :current-mine)])]
     (if disable-organism?
       (update-in db [:idresolver :stage :options]
                  assoc :type value :disable-organism? true :organism nil)
       (update-in db [:idresolver :stage :options]
                  #(cond-> %
                      ; Assoc the new Type value to the options
                     value (assoc :type value)
                      ; Enabled organism dropdown
                     true (assoc :disable-organism? false)
                      ; Give the organism dropdown a value if nil
                     (nil? (:organism %))
                     (assoc :organism
                            (-> mine-details :default-organism))))))))

(reg-event-db
 ::update-option
 (fn [db [_ option-kw value]]
   (assoc-in db [:idresolver :stage :options option-kw] value)))

(reg-event-db
 ::toggle-keep-duplicate
 (fn [db [_ duplicate-idx match-idx]]
   (update-in db [:idresolver :response
                  :matches :DUPLICATE duplicate-idx
                  :matches match-idx :keep?] not)))

(reg-event-db
 ::update-list-name
 (fn [db [_ value]]
   (assoc-in db [:idresolver :save :list-name] value)))

(defn resolution-matches-ids
  "Takes the matches map of a successful ID resolution job response and returns
  a seq of object IDs. The included objects will be those successfully matched
  (excluding wildcards), in addition to duplicate/ambiguous matches that the
  user checked to keep."
  [{:keys [OTHER DUPLICATE TYPE_CONVERTED MATCH]}]
  (map :id
       (concat (mapcat (comp #(filter :keep? %) :matches) DUPLICATE)
               (mapcat :matches TYPE_CONVERTED)
               (mapcat :matches OTHER)
               MATCH)))

(reg-event-fx
 ::upgrade-list
 (fn [{db :db} [_ list-name]]
   (let [service (get-in db [:mines (get db :current-mine) :service])
         ids (resolution-matches-ids (get-in db [:idresolver :response :matches]))]
     {:im-chan {:chan (save/im-list-upgrade service list-name ids)
                :on-success [::upgrade-list-success list-name]
                :on-failure [::upgrade-list-failure list-name]}})))

(reg-event-fx
 ::upgrade-list-success
 (fn [{db :db} [_ list-name _res]]
   {:dispatch ;; Re-fetch our lists so that it shows in Lists page.
    [:assets/fetch-lists
     [::route/navigate ::route/results {:title list-name}]]}))

(reg-event-fx
 ::upgrade-list-failure
 (fn [{db :db} [_ list-name res]]
   {:dispatch [:messages/add
               {:markup [:span [:strong "Failed to upgrade and save " [:em list-name]] " "
                         [:code (if-let [err (not-empty (get-in res [:body :error]))]
                                  err
                                  "Please check your connection and try again later.")]]
                :timeout 10000
                :style "danger"}]}))

(reg-event-fx
 ::save-list
 (fn [{db :db} [_]]
   (let [ids (resolution-matches-ids (get-in db [:idresolver :response :matches]))
         object-type (get-in db [:idresolver :stage :options :type])
         service     (get-in db [:mines (get db :current-mine) :service])
         list-name   (get-in db [:idresolver :save :list-name])]
     {:im-chan {:chan (save/im-list-from-query
                       service
                       list-name
                       {:from object-type
                        :select [(str object-type ".id")]
                        :where [{:path (str object-type ".id")
                                 :op "ONE OF"
                                 :values ids}]})
                :on-success [::save-list-success list-name object-type]
                :on-failure [::save-list-failure]}})))

(reg-event-fx
 ::save-list-success
 (fn [{db :db} [_ list-name object-type response]]
   ; Get the summary fields for this object type (used to construct the
   ;query for the List Analysis page
   (let [{:keys [listName]} response
         summary-fields
         (get-in db
                 [:assets :summary-fields
                  (get db :current-mine) (keyword object-type)])]
     {:db db
      :dispatch-n [; Re-fetch our lists so that it shows in Lists page
                   [:assets/fetch-lists]
                   ; Show the list results in the Results page
                   [:results/history+
                    {:source (get db :current-mine)
                     :type :query
                     :intent :list
                     :value {:title list-name
                             :from object-type
                             :select summary-fields
                             :where [{:path object-type
                                      :op "IN"
                                      :value list-name}]}}]]})))

(reg-event-fx
 ::save-list-failure
 (fn [{db :db} [_ res]]
   {:dispatch [:messages/add
               {:markup [:span [:strong "Failed to save list: "]
                         [:code (if-let [err (not-empty (get-in res [:body :error]))]
                                  err
                                  "Please check your connection and try again later.")]]
                :timeout 10000
                :style "danger"}]}))

(defn validate-default-organism [db mine-details]
  " this handles the fact that a mine could be misconfigured
    and might return an organism that isn't present
    in the list. Learned from the school of hard knocks, aka
    beta humanmine"

  (let [default-organism (:default-organism mine-details)
        all-organisms (get-in db [:cache :organisms])
        all-organism-shortnames
        (reduce (fn [new-vector single-organism]
                  (conj new-vector (:shortName single-organism)))
                #{} all-organisms)]
    (if (contains? all-organism-shortnames default-organism)
      ;;return the default if it exists in the dropdown
      default-organism
      ;;if the default isn't preset, we default to "all", which is equivalent to
      ;;sending a blank string for the organism value
      "")))

(reg-event-db
 ::reset
 (fn [db [_ example-type example-text]]
   (let [mine-details (get-in db [:mines (get db :current-mine)])
         organism (validate-default-organism db mine-details)]
     (assoc db :idresolver
            {:stage {:files nil
                     :textbox example-text
                     :options {:case-sensitive false
                               :type (or example-type
                                         (-> mine-details
                                             :default-object-types first name))
                               :organism organism}
                     :status nil
                     :flags nil}
             :response nil
             :error nil}))))

(reg-event-fx
 ::load-example
 (fn [{db :db} [_]]
   (let [ids (get-in db [:mines (:current-mine db) :idresolver-example])
         ;; Prioritise getting a :Gene example, but if it doesn't exist,
         ;; just fall back to taking the first example.
         [class-type example] (if-let [gene-example (:Gene ids)]
                                [:Gene gene-example]
                                (first ids))]
     {:dispatch [::reset (name class-type) example]})))
