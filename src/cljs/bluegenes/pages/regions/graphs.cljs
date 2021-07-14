(ns bluegenes.pages.regions.graphs
  (:require [re-frame.core :refer [subscribe dispatch]]
            [reagent.core :as reagent]
            [bluegenes.components.table :as table]
            [bluegenes.pages.regions.events]
            [bluegenes.pages.regions.subs]
            [bluegenes.components.imcontrols.views :as im-controls]
            [bluegenes.components.bootstrap :refer [popover tooltip]]
            [clojure.string :refer [split]]
            [oops.core :refer [oget ocall]]))

(defn linear-scale [[r1-lower r1-upper] [r2-lower r2-upper]]
  (fn [v]
    (+ (/ (* (- v r1-lower) (- r2-upper r2-lower))
          (- r1-upper r1-lower))
       r2-lower)))

(defn datum []
  (fn [idx scale {{:keys [start end locatedOn]} :chromosomeLocation} features-count]
    (let [highlighted? @(subscribe [:regions/highlighted? idx
                                    {:chromosome (:primaryIdentifier locatedOn)
                                     :start start :end end}])]
      [:rect.datum
       {:x      (scale start)
        :y      0
        :width  (- (scale end) (scale start))
        :height 60
        :style {:opacity (/ 1 features-count)}
        :class (when highlighted? :highlighted)}])))

(defn bar []
  (fn [scale loc]
    [:path.guide
     {:stroke-width 0.002
      :d            (clojure.string/join
                     " "
                     ["M" (scale loc) 0
                      "L" (scale loc) 75])}]))

(defn label-text [scale text width properties]
  [:div.layout
   [:div.label-text
    (merge properties
           {:style {:left (* width (scale text))}})
    [:span.value text]]])

(defn svg []
  (fn [idx scale-fn data-points to from min max]
    [:svg.region
     {:width                 "100%"
      :height                "60px"
      :preserve-aspect-ratio "none"
      :view-box              "0 0 1 100"}
     (into [:g]
           (map (fn [d]
                  [datum idx scale-fn d (count data-points)])
                ;; Render regions in same order as displayed in table - in
                ;; effect this leads to smaller regions to be placed on top of
                ;; larger regions, making them more visible.
                (sort-by (comp :start :chromosomeLocation)
                         data-points)))
     [bar scale-fn from]
     [bar scale-fn to]
     [bar scale-fn min]
     [bar scale-fn max]]))

(defn legend-width [width]
  (reset! width
          (.-clientWidth (.querySelector js/document ".legend"))))

(defn main []
  (let [width (reagent/atom 1000)] ;;1000 is completely arbitrary. could equally be 0.
    (reagent/create-class
     {:reagent-render
      (fn [idx {:keys [results to from]}]
        (reagent/with-let
          [min-start (apply min (conj (map (comp :start :chromosomeLocation) results) from))
           max-end   (apply max (conj (map (comp :end :chromosomeLocation) results) to))
           scale     (linear-scale [min-start max-end] [0 1])
           handler   #(legend-width width)
           listener  (.addEventListener js/window "resize" handler)]
          [:div.graph
           [svg idx scale results to from min-start max-end]
           [:div.legend
            [:span.start
             {:title (str min-start " \n"
                          "Starting location of the earliest overlapping feature(s)")} min-start]
            [:span.end
             {:title (str max-end " \n"
                          "End location of the last overlapping feature(s)")} max-end]]
           [label-text scale from @width
            {:class "from"
             :title (str from " \n"
                         "This is the start location you input in the box above")}]
           [label-text scale to @width
            {:class "to"
             :title (str to " \n"
                         "This is the end location you input into the box above")}]]
          (finally
            (.removeEventListener js/window "resize" handler))))
      :component-did-mount #(legend-width width)})))
