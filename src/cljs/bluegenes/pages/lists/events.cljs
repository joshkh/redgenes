(ns bluegenes.pages.lists.events
  (:require [re-frame.core :refer [reg-event-db reg-event-fx reg-fx]]
            [re-frame.std-interceptors :refer [path]]
            [bluegenes.pages.lists.utils :refer [denormalize-lists normalize-lists path-prefix? internal-tag? split-path join-path list->path filtered-list-ids-set copy-list-name ->filterf folder?]]
            [imcljs.save :as save]
            [clojure.set :as set]
            [clojure.string :as str]
            [bluegenes.db :refer [default-db]]
            [goog.dom :as gdom]
            [oops.core :refer [oset!]]
            [bluegenes.interceptors :refer [datetime]]
            [bluegenes.time :refer [format-list-date]]))

;; Idea for performance improvement:
;; All lists are re-fetched via `:assets/fetch-lists` whenever something
;; changes, although often fetching a single list would be enough. By using
;; imcljs' `fetch/one-list` you can perhaps make things faster.

(def root [:lists])

;; The vector of lists which the webservice gives us is not very amenable to
;; perform computing on, so we denormalize it into suitable data structures.
(reg-event-db
 :lists/initialize
 (fn [db]
   (let [all-lists (get-in db [:assets :lists (:current-mine db)])
         all-tags (->> all-lists (mapcat :tags) distinct)
         old-by-id (get-in db (concat root [:by-id]))
         new-by-id (denormalize-lists all-lists)
         new-lists (if (empty? old-by-id)
                     #{}
                     (-> (set/difference (->> new-by-id vals set (set/select (complement folder?)))
                                         (->> old-by-id vals set (set/select (complement folder?))))
                         (set/project [:id])))]
     (update-in db root assoc
                :fetching-lists? false
                :by-id new-by-id
                :all-tags (->> all-tags (remove internal-tag?) sort)
                :all-types (->> all-lists (map :type) distinct sort)
                :all-paths (->> (filter path-prefix? all-tags)
                                (map (comp #(subvec % 1) split-path)))
                :new-lists new-lists
                :new-hidden-lists (if (empty? new-lists)
                                    #{}
                                    (let [visible-lists (-> (->> (normalize-lists
                                                                  (->filterf (get-in db (concat root [:controls :filters])))
                                                                  identity
                                                                  {:by-id new-by-id :expanded-paths (constantly true)})
                                                                 (set)
                                                                 (set/select (complement folder?)))
                                                            (set/project [:id]))]
                                      (set/difference new-lists visible-lists)))))))
;; We have to do a bit of heavy lifting to compute :new-lists and
;; :new-hidden-lists (respectively; red icon to indicate a newly changed list,
;; and alert to indicate newly changed lists not visible under current
;; filters). I have thought about this a lot and explored other approaches, but
;; this looks to be the best way.

(reg-event-db
 :lists/expand-path
 (path root)
 (fn [lists [_ path]]
   (update lists :expanded-paths (fnil conj #{}) path)))

(reg-event-db
 :lists/collapse-path
 (path root)
 (fn [lists [_ path]]
   (update lists :expanded-paths (fnil disj #{}) path)))

(defn set-current-page-1 [lists]
  (assoc-in lists [:pagination :current-page] 1))

(defn clear-new-hidden-lists [lists]
  (update lists :new-hidden-lists empty))

(defn reset-filters [lists]
  (let [default-filters (get-in default-db (concat root [:controls :filters]))]
    (assoc-in lists [:controls :filters] default-filters)))

;; Note: Do not dispatch this from more than one place.
;; The input field which changes this value uses debouncing and internal state,
;; so it won't sync with this value except when first mounting.
(reg-event-db
 :lists/set-keywords-filter
 (path root)
 (fn [lists [_ keywords-string]]
   (-> lists
       (assoc-in [:controls :filters :keywords] keywords-string)
       (set-current-page-1)
       (clear-new-hidden-lists))))

(reg-event-db
 :lists/toggle-sort
 (path root)
 (fn [lists [_ column]]
   (update-in lists [:controls :sort]
              (fn [{old-column :column old-order :order}]
                {:column column
                 :order (if (= old-column column)
                          (case old-order
                            :asc :desc
                            :desc :asc)
                          :asc)}))))

(reg-event-db
 :lists/set-filter
 (path root)
 (fn [lists [_ filter-name value]]
   (-> lists
       (assoc-in [:controls :filters filter-name] value)
       (set-current-page-1)
       (clear-new-hidden-lists))))

;; We don't want to make the keyword filter a controlled input as we want to be
;; able to debounce its event. Leading to this lesser evil of DOM manipulation.
(reg-fx
 ::clear-keyword-filter
 (fn [_]
   (oset! (gdom/getElement "lists-keyword-filter") :value "")))

(reg-event-fx
 :lists/reset-filters
 (fn [{db :db} [_]]
   {:db (update-in db root reset-filters)
    ::clear-keyword-filter {}}))

(reg-event-fx
 :lists/show-new-lists
 (fn [{db :db} [_]]
   {:db (-> db
            (update-in root reset-filters)
            (update-in root clear-new-hidden-lists))
    ::clear-keyword-filter {}}))

(reg-event-db
 :lists/set-per-page
 (path root)
 (fn [lists [_ new-value]]
   (assoc-in lists [:pagination :per-page] new-value)))

(reg-event-fx
 :lists/set-current-page
 (fn [{db :db} [_ new-value scroll-top?]]
   (cond-> {:db (assoc-in db (concat root [:pagination :current-page]) new-value)}
     scroll-top? (assoc :scroll-to-top {:ms 0}))))

(reg-event-db
 :lists/select-list
 (path root)
 (fn [lists [_ list-id]]
   (update lists :selected-lists (fnil conj #{}) list-id)))

;; It would be more efficient to use a special value like `:all`, but this would
;; have to be handled in all event handlers reading `:selected-lists`. Not worth
;; it when this is a feature that should be rarely used.
(reg-event-db
 :lists/select-all-lists
 (path root)
 (fn [lists]
   (assoc lists :selected-lists
          (filtered-list-ids-set
           (:by-id lists)
           (get-in lists [:controls :filters])))))

(reg-event-db
 :lists/deselect-list
 (path root)
 (fn [lists [_ list-id]]
   (if (= :subtract (get-in lists [:modal :active]))
     ;; We need to remove list-id from more places if subtract modal is active.
     (-> lists
         (update :selected-lists (fnil disj #{}) list-id)
         (update-in [:modal :keep-lists] (partial filterv #(not= list-id %)))
         (update-in [:modal :subtract-lists] (partial filterv #(not= list-id %))))
     (update lists :selected-lists (fnil disj #{}) list-id))))

(reg-event-db
 :lists/clear-selected
 (path root)
 (fn [lists [_]]
   (assoc lists :selected-lists #{})))

(reg-event-db
 :lists/clear-target
 (path root)
 (fn [lists [_]]
   (let [target-id (get-in lists [:modal :target-id])]
     (update lists :selected-lists (fnil disj #{}) target-id))))

(defn default-list-title [operation data-type now]
  (str (case operation
         :combine "Combined" :intersect "Intersected"
         :difference "Differenced" :subtract "Subtracted")
       " "
       data-type
       " List ("
       (format-list-date now)
       ")"))

(defn lists-root->data-type [{:keys [by-id] :as lists}]
  (-> lists :selected-lists first by-id :type))

(reg-event-fx
 :lists/open-modal
 [(path root) (datetime)]
 (fn [{lists :db now :datetime} [_ modal-kw ?list-id]]
   {:db (case modal-kw
          ;; Subtract modal needs some prepared data.
          :subtract (let [selected-lists (vec (:selected-lists lists))]
                      (assoc lists :modal
                             {:active modal-kw
                              :open? true
                              :subtract-lists (pop selected-lists)
                              :keep-lists (vector (peek selected-lists))
                              :title (default-list-title modal-kw (lists-root->data-type lists) now)}))
          ;; Edit modal needs the list fields preset.
          :edit (let [{:keys [title tags description] :as listm} (get (:by-id lists) ?list-id)]
                  (assoc lists :modal
                         {:active modal-kw
                          :target-id ?list-id
                          :open? true
                          :title title
                          :tags (remove path-prefix? tags)
                          :description description
                          :folder-path (-> listm list->path split-path (subvec 1))}))
          ;; Default for all other modals.
          (assoc lists :modal
                 (merge
                  {:active modal-kw
                   :target-id ?list-id
                   :open? true}
                  (case modal-kw
                    (:combine :intersect :difference :subtract)
                    {:title (default-list-title modal-kw (lists-root->data-type lists) now)}
                    nil))))}))

(reg-event-db
 :lists/close-modal
 (path root)
 (fn [lists [_]]
   (assoc-in lists [:modal :open?] false)))

(reg-event-db
 :lists-modal/set-new-list-tags
 (path root)
 (fn [lists [_ tags]]
   (assoc-in lists [:modal :tags] tags)))

(reg-event-db
 :lists-modal/set-new-list-title
 (path root)
 (fn [lists [_ title]]
   (assoc-in lists [:modal :title] title)))

(reg-event-db
 :lists-modal/set-new-list-description
 (path root)
 (fn [lists [_ description]]
   (assoc-in lists [:modal :description] description)))

(reg-event-db
 :lists-modal/subtract-list
 (path root)
 (fn [lists [_ id]]
   (-> lists
       (update-in [:modal :keep-lists] (partial filterv #(not= id %)))
       (update-in [:modal :subtract-lists] conj id))))

(reg-event-db
 :lists-modal/keep-list
 (path root)
 (fn [lists [_ id]]
   (-> lists
       (update-in [:modal :keep-lists] conj id)
       (update-in [:modal :subtract-lists] (partial filterv #(not= id %))))))

(reg-event-db
 :lists-modal/nest-folder
 (path root)
 (fn [lists [_ new-folder]]
   (update-in lists [:modal :folder-path] (fnil conj []) new-folder)))

(reg-event-db
 :lists-modal/denest-folder
 (path root)
 (fn [lists [_]]
   (update-in lists [:modal :folder-path] pop)))

(def list-operation->im-req
  {:combine save/im-list-union
   :intersect save/im-list-intersect
   :difference save/im-list-difference
   :subtract save/im-list-subtraction})

(reg-event-fx
 :lists/set-operation
 (fn [{db :db} [_ list-operation]]
   (let [service (get-in db [:mines (:current-mine db) :service])
         im-req (list-operation->im-req list-operation)
         {:keys [by-id selected-lists]
          {:keys [title tags description
                  keep-lists subtract-lists]} :modal} (:lists db)
         keep-lists     (->> keep-lists     (map by-id) (map :name))
         subtract-lists (->> subtract-lists (map by-id) (map :name))
         source-lists   (->> selected-lists (map by-id) (map :name))
         subtract? (= list-operation :subtract)
         enough-lists? (if subtract?
                         (every? seq [keep-lists subtract-lists])
                         (> (count source-lists) 1))
         options {:description description :tags tags}]
     (if (and im-req enough-lists? (not-empty title))
       {:im-chan {:chan (if subtract?
                          (im-req service title keep-lists subtract-lists options)
                          (im-req service title source-lists options))
                  :on-success [:lists/operation-success]
                  :on-failure [:lists/set-operation-failure title]}
        :db (assoc-in db [:lists :modal :error] nil)}
       {:db (assoc-in db [:lists :modal :error]
                      (cond
                        (not im-req) (str "Invalid list operation: " list-operation)
                        (and (not enough-lists?) subtract?) "You need at least 1 list in each set to perform a list subtraction"
                        (not enough-lists?) "You need at least 2 lists to perform a list set operation"
                        (empty? title) "You need to specify a title for the new list"
                        :else "Unexpected error"))}))))

;; Doesn't have `set-` in its name as it's dispatched by all successful events.
(reg-event-fx
 :lists/operation-success
 (fn [{db :db} [_ res]] ; Note that `res` can be nil or a vector of responses.
   {:dispatch-n [[:lists/close-modal]
                 [:lists/clear-selected]
                 [:assets/fetch-lists]]}))

;; Identical to above except it uses `clear-target` instead of `clear-selected`.
(reg-event-fx
 :lists/operation-success-target
 (fn [{db :db} [_ res]] ; Note that `res` can be nil or a vector of responses.
   {:dispatch-n [[:lists/close-modal]
                 [:lists/clear-target]
                 [:assets/fetch-lists]]}))

(reg-event-fx
 :lists/set-operation-failure
 (fn [{db :db} [_ list-name res]]
   {:db (assoc-in db [:lists :modal :error]
                  (str "Failed to create list " list-name
                       (when-let [error (get-in res [:body :error])]
                         (str ": " error))))
    :log-error ["List set operation failure" res]}))

(reg-event-fx
 :lists/move-lists
 (fn [{db :db} [_]]
   (let [service (get-in db [:mines (:current-mine db) :service])
         {:keys [by-id selected-lists]
          {:keys [folder-path]} :modal} (:lists db)
         folder-tag (when (seq folder-path) (join-path folder-path))
         source-list-maps (map by-id selected-lists)
         new-list->tags (zipmap (map :name source-list-maps)
                                (repeat folder-tag))
         old-list->tags (zipmap (map :name source-list-maps)
                                (map list->path source-list-maps))
         update-tag-chans
         (->> (set/difference (set new-list->tags) (set old-list->tags))
              (map (fn [[list-name new-tag]]
                     (let [old-tag (get old-list->tags list-name)]
                       [(when old-tag (save/im-list-remove-tag service list-name old-tag))
                        (when new-tag (save/im-list-add-tag service list-name new-tag))])))
              (apply concat)
              (filter some?))]
     (cond
       (and (seq source-list-maps) (seq update-tag-chans))
       {:im-chan {:chans update-tag-chans
                  :on-success [:lists/operation-success]
                  :on-failure [:lists/move-lists-failure]}
        :db (assoc-in db [:lists :modal :error] nil)}

       (seq source-list-maps) ; Lists haven't actually moved around, so signal success.
       {:dispatch [:lists/operation-success nil]
        :db (assoc-in db [:lists :modal :error] nil)}

       :else
       {:db (assoc-in db [:lists :modal :error]
                      (cond
                        (empty? source-list-maps) "You need to select at least 1 list to move"
                        :else "Unexpected error"))}))))

(reg-event-fx
 :lists/move-lists-failure
 (fn [{db :db} [_ all-res]]
   {:db (assoc-in db [:lists :modal :error] "Error occured when moving lists")
    :dispatch [:assets/fetch-lists] ; In case some of them were moved successfully.
    :log-error ["Move lists failure" all-res]}))

(reg-event-fx
 :lists/copy-lists
 (fn [{db :db} [_]]
   (let [service (get-in db [:mines (:current-mine db) :service])
         {:keys [by-id selected-lists]
          {:keys [folder-path target-id]} :modal} (:lists db)
         lists (->> (vals by-id) (remove folder?))
         folder-tag (when (seq folder-path) (join-path folder-path))
         target-lists (->> (if target-id [target-id] selected-lists) (map by-id) (map :name))
         copied-lists (map #(copy-list-name lists %) target-lists)]
     (if (seq target-lists)
       ;; If you look at the source of `save/im-list-copy` you can see it's
       ;; inefficient due to it running a query to fetch information we already
       ;; have, so there's room for improvement there. Same with adding folder
       ;; tags in a separate request, as `tolist` takes the tags parameter.
       ;; All this would require changes to imcljs.
       {:im-chan {:chans (map #(save/im-list-copy service %1 %2)
                              target-lists copied-lists)
                  :on-success (if folder-tag
                                [:lists/copy-lists-add-tag folder-tag (boolean target-id)]
                                (if target-id
                                  [:lists/operation-success-target]
                                  [:lists/operation-success]))
                  :on-failure [:lists/copy-lists-failure]}
        :db (assoc-in db [:lists :modal :error] nil)}
       {:db (assoc-in db [:lists :modal :error]
                      (cond
                        (empty? target-lists) "You need to select at least 1 list to copy"
                        :else "Unexpected error"))}))))

(reg-event-fx
 :lists/copy-lists-add-tag
 (fn [{db :db} [_ folder-tag target? all-res]]
   (let [service (get-in db [:mines (:current-mine db) :service])
         copied-lists (map :listName all-res)]
     (if (seq copied-lists)
       {:im-chan {:chans (map #(save/im-list-add-tag service % folder-tag)
                              copied-lists)
                  :on-success (if target?
                                [:lists/operation-success-target]
                                [:lists/operation-success])
                  :on-failure [:lists/copy-lists-failure]}}
       {:db (assoc-in db [:lists :modal :error]
                      (cond
                        (empty? copied-lists) "Failed to move copied lists into specified folder"
                        :else "Unexpected error"))}))))

(reg-event-fx
 :lists/copy-lists-failure
 (fn [{db :db} [_ all-res]]
   {:db (assoc-in db [:lists :modal :error] "Error occured when copying lists")
    :dispatch [:assets/fetch-lists] ; In case some of them were copied successfully.
    :log-error ["Copy lists failure" all-res]}))

(reg-event-fx
 :lists/delete-lists
 (fn [{db :db} [_]]
   (let [service (get-in db [:mines (:current-mine db) :service])
         {:keys [by-id selected-lists]
          {:keys [target-id]} :modal} (:lists db)
         source-list-maps (map by-id (if target-id [target-id] selected-lists))
         target-lists (map :name source-list-maps)
         unauthorized-lists (->> source-list-maps (remove :authorized) (map :name))]
     (if (and (seq target-lists) (empty? unauthorized-lists))
       {:im-chan {:chans (map #(save/im-list-delete service %) target-lists)
                  :on-success (if target-id
                                [:lists/operation-success-target]
                                [:lists/operation-success])
                  :on-failure [:lists/delete-lists-failure]}
        :db (assoc-in db [:lists :modal :error] nil)}
       {:db (assoc-in db [:lists :modal :error]
                      (cond
                        (empty? target-lists) "You need to select at least 1 list to delete"
                        (seq unauthorized-lists) (str "You are not authorized to delete the following lists: " (str/join ", " unauthorized-lists))
                        :else "Unexpected error"))}))))

(reg-event-fx
 :lists/delete-lists-failure
 (fn [{db :db} [_ all-res]]
   {:db (assoc-in db [:lists :modal :error] "Error occured when deleting lists")
    :dispatch [:assets/fetch-lists] ; In case some of them were deleted successfully.
    :log-error ["Delete lists failure" all-res]}))

(reg-event-fx
 :lists/edit-list
 (fn [{db :db} [_]]
   (let [service (get-in db [:mines (:current-mine db) :service])
         {:keys [by-id]
          {:keys [target-id title tags description folder-path]} :modal} (:lists db)
         {old-title :title old-tags :tags old-description :description
          authorized :authorized} (get by-id target-id)
         ;; We want an empty string instead of nil when not defined.
         old-description (or old-description "")
         description     (or description "")
         path-tag (join-path folder-path)
         old-all-tags (set old-tags)
         ;; The `cond->` is to avoid conj'ing if path-tag is nil.
         all-tags (-> tags set (cond-> path-tag (conj path-tag)))
         tags-to-delete (set/difference old-all-tags all-tags)
         tags-to-add (set/difference all-tags old-all-tags)
         next-actions {:description    (when (not= description old-description) description)
                       :tags-to-delete (when (seq tags-to-delete) tags-to-delete)
                       :tags-to-add    (when (seq tags-to-add) tags-to-add)}
         next-event? (not-every? nil? (vals next-actions))]
     (cond
       (and (not authorized) (or (not= title old-title)
                                 (not= description old-description)))
       {:db (assoc-in db [:lists :modal :error]
                      "You are not authorized to edit the title or description of this list.")}

       (and (not-empty title) (not= title old-title))
       {:im-chan {:chan (save/im-list-rename service old-title title)
                  :on-success (if next-event?
                                [:lists/edit-list-title-success title next-actions]
                                [:lists/operation-success-target])
                  :on-failure [:lists/edit-list-failure]}
        :db (assoc-in db [:lists :modal :error] nil)}

       (and (not-empty title) next-event?) ; Title hasn't changed, but there are more things to do.
       {:dispatch [:lists/edit-list-title-success title next-actions]
        :db (assoc-in db [:lists :modal :error] nil)}

       (not-empty title) ; Nothing has changed; signal success.
       {:dispatch [:lists/operation-success-target nil]
        :db (assoc-in db [:lists :modal :error] nil)}

       :else
       {:db (assoc-in db [:lists :modal :error]
                      (cond
                        (empty? title) "The title of a list cannot be empty"
                        :else "Unexpected error"))}))))

;; We need to make sure any change to the list title/name goes through first,
;; as otherwise we could make changes to a different list that has the new title!
;; (Oh boy, how we wish list webservices took a list ID instead!)
(reg-event-fx
 :lists/edit-list-title-success
 (fn [{db :db} [_ title {:keys [description tags-to-delete tags-to-add]}]]
   (let [service (get-in db [:mines (:current-mine db) :service])
         ;; Any of these values are nil when they haven't changed.
         chans (->> [(when description
                       (save/im-list-update service title {:newDescription description}))
                     (when tags-to-delete
                       (save/im-list-remove-tag service title tags-to-delete))
                     (when tags-to-add
                       (save/im-list-add-tag service title tags-to-add))]
                    (filter some?))]
     (if (seq chans)
       {:im-chan {:chans chans
                  :on-success [:lists/operation-success-target]
                  :on-failure [:lists/edit-list-failure]}
        :db (assoc-in db [:lists :modal :error] nil)}
       ;; This clause shouldn't run, but it's here for safety.
       {:dispatch [:lists/operation-success-target nil]
        :db (assoc-in db [:lists :modal :error] nil)}))))

(reg-event-fx
 :lists/edit-list-failure
 (fn [{db :db} [_ res]] ; `res` can be a map or a vector of multiple.
   {:db (assoc-in db [:lists :modal :error]
                  (if-let [err (not-empty (if (sequential? res)
                                            (->> (map #(get-in % [:body :error]) res)
                                                 (filter not-empty)
                                                 (str/join \newline))
                                            (get-in res [:body :error])))]
                    err
                    "Failed to edit list"))
    :dispatch [:assets/fetch-lists] ; In case some changes were successful.
    :log-error ["Edit list failure" res]}))
