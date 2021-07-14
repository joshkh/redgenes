(ns bluegenes.components.export-query
  (:require [re-frame.core :refer [subscribe dispatch]]
            [reagent.core :as reagent]
            [imcljs.query :as im-query]))

(def export-types
  ^{:doc "Supported export types as map with keys:
    label , format | endpoint , [update-query-fn] , [rdf-required]"}
  [{:label "TAB" :format "tab"}
   {:label "CSV" :format "csv"}
   {:label "GFF3" :endpoint "gff3"}
   {:label "BED" :endpoint "bed"}
   {:label "FASTA" :endpoint "fasta"}
    ;; Does not seem to be necessary for the use case of region search.
    ; :update-query-fn #(-> % (assoc :select ["id" "symbol" "organism.name"]) (dissoc :sortOrder :orderBy))}
   {:label "RDF" :format "rdf" :rdf-required true}
   {:label "N-Triples" :format "ntriples" :rdf-required true}])

(defn export-button [query {:keys [label format endpoint update-query-fn]
                            :or {update-query-fn identity}}]
  (let [{:keys [root model token]} @(subscribe [:active-service])]
    [:a.btn.btn-default.btn-raised.btn-xs
     {:href (str root "/service/query/results" (when endpoint (str "/" endpoint))
                 "?query=" (js/encodeURIComponent
                            (im-query/->xml model (update-query-fn query)))
                 (when format (str "&format=" format))
                 (when-not endpoint (str "&columnheaders=" "friendly"))
                 "&token=" token)}
     label]))

(defn main [query & {:keys [label]}]
  (let [rdf-support? @(subscribe [:rdf-support?])]
    (into [:div
           (when label [:label label])]
          (for [export-data (cond->> export-types
                              (not rdf-support?) (remove :rdf-required))]
            [export-button query export-data]))))
