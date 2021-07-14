(ns bluegenes.subs
  (:require [re-frame.core :refer [reg-sub subscribe]]
            [bluegenes.pages.results.enrichment.subs]
            [clojure.string :refer [ends-with?]]
            [bluegenes.pages.querybuilder.subs]
            [bluegenes.components.search.subs]
            [bluegenes.subs.auth]
            [bluegenes.components.idresolver.subs]
            [bluegenes.pages.profile.subs]
            [bluegenes.components.viz.subs]
            [bluegenes.pages.home.subs]
            [bluegenes.pages.lists.subs]
            [bluegenes.pages.tools.subs]
            [bluegenes.pages.developer.subs]
            [bluegenes.pages.results.widgets.subs]
            [bluegenes.pages.regions.subs]
            [bluegenes.version :as version]
            [bluegenes.utils :as utils]
            [lambdaisland.uri :refer [uri]]
            [clojure.set :as set]))

(reg-sub
 :name
 (fn [db]
   (:name db)))

(reg-sub
 :registry
 (fn [db]
   (:registry db)))

;; Combines registry and configured mines, merging mines with the same namespace.
;; For when we want to display them all together!
(reg-sub
 :registry+configured-mines
 :<- [:registry]
 :<- [:env/mines]
 (fn [[registry configured]]
   (merge configured registry)))

;; Removes configured mines from registry.
;; For when we want to display them separate!
(reg-sub
 :registry-wo-configured-mines
 :<- [:registry]
 :<- [:env/mines]
 (fn [[registry configured]]
   (let [registry-only (set/difference (set (keys registry))
                                       (set (keys configured)))]
     (select-keys registry registry-only))))

(reg-sub
 :short-name
 (fn [db]
   (:short-name db)))

(reg-sub
 :mine-url
 (fn [db]
   (let [mine-name (:current-mine db)
         url       (:url (:mine (mine-name (:mines db))))]
     (str "http://" url))))

(reg-sub :mine-default-organism
         (fn [db]
           (let [mine-name (:current-mine db)
                 organism  (:abbrev (mine-name (:mines db)))]
             organism)))

(reg-sub :mines
         (fn [db]
           (:mines db)))

(reg-sub
 :mine-name
 (fn [db]
   (:current-mine db)))

(reg-sub
 :current-mine-name
 (fn [db]
   (:current-mine db)))

(reg-sub
 :active-panel
 (fn [db _]
   (:active-panel db)))

(reg-sub
 :panel-params
 (fn [db _]
   (:panel-params db)))

(reg-sub
 :app-db
 (fn [db _] db))

(reg-sub
 :who-am-i
 (fn [db _]
   (:who-am-i db)))

(reg-sub
 :fetching-report?
 (fn [db _]
   (:fetching-report? db)))

(reg-sub
 :model
 (fn [db _]
   (let [current-mine  (get-in db [:mines (get db :current-mine)])
         current-model (get-in current-mine [:service :model :classes])]
     current-model)))

(reg-sub
 :current-model
 (fn [db _]
   (let [current-mine (get-in db [:mines (get db :current-mine)])]
     (get-in current-mine [:service :model]))))

(reg-sub
 :current-model-hier
 (fn [db]
   (let [current-mine (get-in db [:mines (get db :current-mine)])]
     (get current-mine :model-hier))))

(reg-sub
 :assets
 (fn [db]
   (:assets db)))

(reg-sub
 :lists
 (fn [db [_]]
   (get-in db [:assets :lists])))

(reg-sub
 :summary-fields
 (fn [db _]
   (:summary-fields (:assets db))))

(reg-sub
 :current-summary-fields
 (fn [db [_ class-kw]]
   (get-in db [:assets :summary-fields (:current-mine db)])))

(reg-sub
 :progress-bar-percent
 (fn [db _]
   (:progress-bar-percent db)))

(reg-sub
 :cache/organisms
 (fn [db]
   (get-in db [:cache :organisms])))

(reg-sub
 :cache/rss
 (fn [db [_ rss]]
   (get-in db [:cache :rss rss])))

(reg-sub
 :current-mine
 (fn [db]
   (get-in db [:mines (get db :current-mine)])))

(reg-sub
 :current-mine/news
 :<- [:current-mine]
 (fn [current-mine]
   (:news current-mine)))

(reg-sub
 :current-mine/citation
 :<- [:current-mine]
 (fn [current-mine]
   (:citation current-mine)))

(reg-sub
 :current-mine/credits
 :<- [:current-mine]
 (fn [current-mine]
   (:credits current-mine)))

(reg-sub
 :current-mine/description
 :<- [:current-mine]
 (fn [current-mine]
   (:description current-mine)))

(reg-sub
 :current-mine/oauth2-providers
 :<- [:current-mine]
 (fn [current-mine]
   (:oauth2-providers current-mine)))

(reg-sub
 :current-mine/notice
 :<- [:current-mine]
 (fn [current-mine]
   (:notice current-mine)))

(reg-sub
 :current-mine/report-layout
 (fn [[_ class]]
   [(subscribe [:current-mine])
    (subscribe [:bluegenes.pages.admin.subs/categories-fallback class])])
 (fn [[current-mine fallback-layout] [_ class]]
   (or (not-empty (get-in current-mine [:report-layout (some-> class name)]))
       fallback-layout)))

(reg-sub
 :current-mine-human-name
 :<- [:current-mine]
 (fn [current-mine]
   (:name current-mine)))

(reg-sub
 :active-token
 :<- [:current-mine]
 (fn [current-mine]
   (get-in current-mine [:service :token])))

(reg-sub
 :active-service
 :<- [:current-mine]
 (fn [current-mine]
   (get-in current-mine [:service])))

(reg-sub
 :version
 :<- [:assets]
 :<- [:current-mine-name]
 (fn [[assets mine-keyword]]
   (get-in assets [:intermine-version mine-keyword])))

;; Returned as number, due to mostly being used in comparisons for checking
;; compatibility.
(reg-sub
 :api-version
 :<- [:assets]
 :<- [:current-mine-name]
 (fn [[assets mine-keyword]]
   (-> (get-in assets [:web-service-version mine-keyword])
       (utils/version-string->vec)
       (first))))

(reg-sub
 :release-version
 :<- [:assets]
 :<- [:current-mine-name]
 (fn [[assets mine-keyword]]
   (get-in assets [:release-version mine-keyword])))

(reg-sub
 :current-lists
 :<- [:lists]
 :<- [:current-mine-name]
 (fn [[all-lists current-mine-name]]
   (get all-lists current-mine-name)))

(reg-sub
 :current-possible-values
 :<- [:current-mine]
 (fn [current-mine [_ path]]
   (get-in current-mine [:possible-values path])))

(reg-sub
 :current-class-keys
 :<- [:current-mine]
 (fn [current-mine]
   (:class-keys current-mine)))

(reg-sub
 :invalid-token?
 (fn [db]
   (get db :invalid-token?)))

(reg-sub
 :messages
 (fn [db]
   (sort-by :when > (vals (:messages db)))))

;; Note that this returns a string similar to "\"4.2.0\"\n".
(reg-sub
 :current-intermine-version
 (fn [db]
   (get-in db [:assets :intermine-version (:current-mine db)])))

(reg-sub
 :list-tags-support?
 :<- [:current-intermine-version]
 (fn [current-version]
   (utils/compatible-version? version/list-tags-support current-version)))

(reg-sub
 :bg-properties-support?
 :<- [:current-intermine-version]
 (fn [current-version]
   (utils/compatible-version? version/bg-properties-support current-version)))

(reg-sub
 :widget-support?
 :<- [:current-intermine-version]
 (fn [current-version]
   (utils/compatible-version? version/widget-support current-version)))

(reg-sub
 :rdf-support?
 :<- [:current-intermine-version]
 (fn [current-version]
   (utils/compatible-version? version/rdf-support current-version)))

(reg-sub
 :show-mine-loader?
 (fn [db]
   (get db :show-mine-loader?)))

(reg-sub
 :registry/description
 :<- [:registry]
 :<- [:current-mine-name]
 (fn [[registry current-mine]]
   (get-in registry [current-mine :description])))

(reg-sub
 :registry/twitter
 :<- [:registry]
 :<- [:current-mine-name]
 (fn [[registry current-mine]]
   (not-empty (get-in registry [current-mine :twitter]))))

(reg-sub
 :registry/email
 :<- [:registry]
 :<- [:current-mine-name]
 (fn [[registry current-mine]]
   (not-empty (get-in registry [current-mine :maintainerEmail]))))

;;;; Branding

(reg-sub
 :branding
 :<- [:current-mine]
 (fn [current-mine]
   (get current-mine :branding)))

(reg-sub
 :branding/images
 :<- [:branding]
 (fn [branding]
   (get branding :images)))

(reg-sub
 :branding/logo
 :<- [:branding/images]
 (fn [images]
   (get images :logo)))

(reg-sub
 :branding/colors
 :<- [:branding]
 (fn [branding]
   (get branding :colors)))

(reg-sub
 :branding/header-main
 :<- [:branding/colors]
 (fn [colors]
   (get-in colors [:header :main])))

;; Secondary is a new value, so don't assume it to be present in all mines.
(reg-sub
 :branding/header-secondary
 :<- [:branding/colors]
 (fn [colors]
   (get-in colors [:header :secondary])))

(reg-sub
 :branding/header-text
 :<- [:branding/colors]
 (fn [colors]
   (get-in colors [:header :text])))

;; Environment (stuff from config.edn and/or envvars)

(reg-sub
 :env
 (fn [db]
   (:env db)))

(reg-sub
 :env/mines
 :<- [:env]
 (fn [env]
   (:mines env)))

(reg-sub
 :current-mine-is-env?
 :<- [:env/mines]
 :<- [:current-mine-name]
 (fn [[env-mines current-mine]]
   (contains? env-mines current-mine)))
