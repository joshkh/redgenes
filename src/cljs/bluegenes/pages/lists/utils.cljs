(ns bluegenes.pages.lists.utils
  (:require [clojure.string :as str])
  (:import goog.date.Date))

(def path-tag-prefix "bgpath")

(defn path-prefix? [s]
  (str/starts-with? s (str path-tag-prefix ":")))

(defn internal-tag? [tag]
  (str/includes? tag ":"))

(defn split-path [path]
  (str/split path #"[\.:]"))

(defn join-path [pathv]
  (when (seq pathv)
    (str path-tag-prefix ":" (str/join "." pathv))))

(defn path-title [path]
  (last (split-path path)))

(defn when-fn
  "Returns x when (f x) is truthy and nil otherwise.
  Useful when using a predicate function with `some`."
  [f]
  (fn [x]
    (when (f x) x)))

(defn top-level-list? [{:keys [tags] :as _listm}]
  (if (sequential? tags)
    (every? (complement path-prefix?) tags)
    false))

(defn top-level-folder? [{:keys [path] :as _folderm}]
  (if (some? path)
    (= 2 (-> path split-path count))
    false))

(defn list->path [{:keys [tags] :as _listm}]
  (some (when-fn path-prefix?) tags))

(defn nesting [path]
  (count (re-seq #"\." path)))

(defn descendant-of? [parent child]
  (str/starts-with? child (str parent ".")))

(defn child-of? [parent child]
  (and (descendant-of? parent child)
       (= (nesting parent) (dec (nesting child)))))

(defn newest-descendant-list
  "Takes a map from path tags to vectors of list maps, and a path; returns the
  newest list that is in the same path, or a descendant path."
  [path->lists path]
  (->> path->lists
       (filter (comp (some-fn (partial = path)
                              (partial descendant-of? path))
                     key))
       (mapcat val)
       (sort-by :timestamp >)
       (first)))

(defn ensure-subpaths
  "If there are nested paths, where a higher level of nesting don't have lists,
  that level won't be visible as a tag (e.g. `foo.bar` has lists but not `foo`).
  This goes through all paths and add any subpaths that are missing."
  [paths]
  (distinct
   (mapcat (fn [path]
             (let [p (split-path path)]
               (map (comp join-path #(subvec p 1 %))
                    (range 2 (inc (count p))))))
           paths)))

(defn denormalize-lists
  "Takes a sequential of list maps, as delivered by the webservice, returning
  a map from list IDs or folder paths, to list or folder maps.
  Folders only exist on the backend as a tag present on lists, so this process
  realizes them into distinct maps, with metadata necessary for the interface
  and a children key containing list IDs and folder paths that are under it."
  [lists]
  (let [id->list (zipmap (map :id lists) lists)
        folder-paths (->> (mapcat :tags lists)
                          (distinct)
                          (filter path-prefix?)
                          (ensure-subpaths))
        path->lists (->> (filter (complement top-level-list?) lists)
                         (group-by list->path))]
    (reduce (fn [id->list+folder path]
              (let [list-children (path->lists path)
                    child-list-ids (map :id list-children)
                    child-paths (filter (partial child-of? path) folder-paths)
                    children (concat child-list-ids child-paths)
                    {:keys [authorized dateCreated timestamp]}
                    (newest-descendant-list path->lists path)]
                (assoc id->list+folder path
                       {:id path
                        :path path ; Main way to identify a folder is the presence of this key.
                        :title (path-title path)
                        :size (count children)
                        :authorized authorized
                        :dateCreated dateCreated
                        :timestamp timestamp
                        :children children})))
            id->list
            folder-paths)))

(defn folder? [folderm]
  (contains? folderm :path))

(defn reduce-folders
  "Walk through `items` adding children after folders that are expanded and
  stripping folders without children. A folder can only exist if it has
  children, so if there are no children, they have likely become filtered away,
  in which case there's no point in showing this folder. Also adds an
  `:is-last` key to the last child of a folder."
  [expanded-paths expand-children items]
  (reduce (fn [items {:keys [path children] :as item}]
            (if (folder? item)
              (let [expanded (expand-children children)]
                (cond
                  ;; `expanded-paths` can be a set or function.
                  (and (expanded-paths path)
                       (seq expanded)) (into items (cons item
                                                         (update expanded (-> expanded count dec)
                                                                 assoc :is-last true)))
                  (seq expanded)       (conj items item)
                  :else                items))
              (conj items item)))
          []
          items))

(defn expand-folders
  "Helper function for `normalize-lists` to handle children nested in folders.
  It is mutually recursive with `reduce-folders`."
  [filterf sortf {:keys [by-id expanded-paths] :as denormalized} children]
  (let [children-maps (mapv by-id children)]
    (reduce-folders
     expanded-paths
     (partial expand-folders filterf sortf denormalized)
     (sortf
      (concat (filterf (filter (complement folder?) children-maps))
              (filter folder? children-maps))))))

(defn normalize-lists
  "Takes the denormalized lists and folders data and converts it into a flat
  sequence of maps for rendering. Folders will be recursively expanded, with
  children placed proceeding the parent folder maps.
  Takes functions to filter and sort, which should be created with the helpers
  below. Optionally takes pagination data."
  [filterf sortf {:keys [by-id expanded-paths] :as denormalized}
   & [{:keys [per-page current-page] :as pagination}]]
  (let [top-level-maps (vals by-id)]
    (reduce-folders
     expanded-paths
     (partial expand-folders filterf sortf denormalized)
     (cond-> (sortf
              (concat (filterf (filter top-level-list? top-level-maps))
                      (filter top-level-folder? top-level-maps)))
       pagination (->> (drop (* per-page (dec current-page)))
                       (take per-page))))))

(defn ->filterf
  "Create a filter function from the filters map in controls, to be passed to
  `normalize-lists`. Remember that comp's arguments are run in reverse order!
  Will only be passed list maps, and not folders."
  [{:keys [keywords lists date type tags] :as _filterm}]
  (comp
   ;; Keyword filter should be done last.
   (if (empty? keywords)
     identity
     (let [keyws (map str/lower-case (-> keywords str/trim (str/split #"\s+")))]
       ;; Slightly faster; consider it if you wish to improve performance.
       #_(partial filter (fn [{:keys [title description]}]
                           (let [s (-> (str title " " description)
                                       (str/lower-case))]
                             (every? #(str/includes? s %) keyws))))
       ;; The following function filters by matching all the different fields
       ;; belonging to a list. Performance seems quite good even for 200 lists.
       (partial filter (fn [listm]
                         (let [all-text (->> listm
                                             ((juxt :title :size :description :type
                                                    ;; Note that internal tags aren't removed!
                                                    (comp #(str/join " " %) :tags)))
                                             (str/join " ")
                                             (str/lower-case))]
                           (every? #(str/includes? all-text %) keyws))))))
   ;; Filter by tag.
   (if (nil? tags)
     identity
     (partial filter (comp #(contains? % tags) set :tags)))
   ;; Filter by type.
   (if (nil? type)
     identity
     (partial filter (comp #{type} :type)))
   ;; Filter by details.
   (if (or (nil? lists) (= lists :folder)) ; Folders first handled in `->sortf`.
     identity
     (partial filter (case lists
                       :private (comp true? :authorized)
                       :public (comp false? :authorized)
                       :upgrade (comp #{"TO_UPGRADE"} :status))))
   ;; Filter by date.
   (if (nil? date)
     identity
     ;; This uses `goog.date.Date` instead of `js/Date`. The main difference is
     ;; that the former defaults to midnight of the current date, while the
     ;; latter defaults to the current time in UTC.
     (let [now (.getTime (Date.))]
       (partial filter (case date
                         :day   (comp #(> (+ % 8.64e+7) now) :timestamp)
                         :week  (comp #(> (+ % 6.048e+8) now) :timestamp)
                         :month (comp #(> (+ % 2.628e+9) now) :timestamp)
                         :year  (comp #(> (+ % 3.154e+10) now) :timestamp)))))))

(defn ->sortf
  "Create a sort function from the sort map in controls, to be passed to
  `normalize-lists`. Remember that comp's arguments are run in reverse order!
  Will be passed both list and folder maps."
  [{:keys [column order] :as _sortm} & {:keys [folders-first?]}]
  (comp
   ;; Show private lists first.
   (partial sort-by :authorized (comp - compare))
   ;; Show folders first if the filter is applied.
   (if folders-first?
     (partial sort-by folder? (comp - compare))
     identity)
   ;; Sort according to active control.
   (partial sort-by
            ;; We don't want "B" to come before "a", so we lowercase strings.
            (comp #(cond-> % (string? %) str/lower-case)
                  ;; Filter away internal tags, which we don't care to sort after.
                  (if (= column :tags)
                    (partial filterv (complement internal-tag?))
                    identity)
                  column)
            ;; `compare` also works great for vectors, for which it will first
            ;; sort by length, then by each element.
            (case order
              :asc compare
              :desc (comp - compare)))))

(defn filtered-list-ids-set
  "Returns a set of all list IDs that are selectable with the currently active
  filters. This is needed when we want to select all lists, or check if all
  lists are currently selected."
  [items-by-id active-filters]
  (->> (normalize-lists
        (->filterf active-filters)
        identity
        {:by-id items-by-id :expanded-paths (constantly true)})
       (remove folder?)
       (map :id)
       (set)))

(defn copy-list-name
  "Returns a new unique list name to be used for a copied list. Usually this
  is achieved by appending '_1', but as there might already exist a list with
  this name, we instead find the identical list names with a '_*' postfix and
  grab the one with the highest number. The increment of this number will then
  be used as postfix for the new list name."
  [lists list-name]
  (let [greatest-postfix-num (->> lists
                                  (map :name)
                                  (filter #(str/starts-with? % (str list-name "_")))
                                  (map #(-> % (str/split #"_") peek js/parseInt))
                                  (apply max))
        ;; We need to handle nil (no lists with postfix) and NaN (empty after postfix).
        new-postfix (if (or (not (number? greatest-postfix-num))
                            (.isNaN js/Number greatest-postfix-num))
                      1
                      (inc greatest-postfix-num))]
    (str list-name "_" new-postfix)))
