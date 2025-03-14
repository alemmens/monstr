(ns nuestr.tab-views
  (:require
   [cljfx.api :as fx]
   [cljfx.ext.list-view :as fx.ext.list-view]
   [clojure.java.io :as io]
   [clojure.tools.logging :as log]
   [clojure.set :as set]
   [nuestr.domain :as domain]
   [nuestr.file-sys :as file-sys]
   [nuestr.hydrate :as hydrate]
   [nuestr.metadata :as metadata]
   [nuestr.status-bar :as status-bar]
   )
  (:import (javafx.geometry Insets)))

(defn- field-row
  [label content]
  {:fx/type :h-box
   :children [{:fx/type :label
               :alignment :center
               :min-width 60
               :max-width 60
               :text label}
              content
              ]})

(defn- field-label [{:keys [text]}]
  {:fx/type :label
   :padding 5
   :min-width 100
   :max-width 100
   :text text})

(defn- text-input [{:keys [label value on-text-changed]}]
  {:fx/type :h-box
   :children [{:fx/type field-label
               :text label}
              {:fx/type :text-field
               :pref-column-count 20
               :text value
               :style-class ["text-input" "nuestr-view-name"]
               :on-text-changed on-text-changed}]})

(defn update-temp-view!
  [property value]
  ;; Create a temp view if we don't have one yet.
  (when-not (:temp-view @domain/*state)
    (let [temp-view (first (vals (:views @domain/*state)))]
      (swap! domain/*state assoc
             :selected-view (:name temp-view)
             :temp-view temp-view)))
  ;; Update the temp view's property value.
  (log/debugf "Updating temp-view's %s to %s" property value)
  (swap! domain/*state assoc-in
         [:temp-view property]
         value)
  (swap! domain/*state assoc :temp-view-changed? true))
      
    
                                      
(def follow-options
   {:all "all (global)"
    :use-identity "contacts of active account"
    :use-list "user list"})

(defn- follow-set-pane [{:keys [temp-view metadata-cache]}]
  {:fx/type :h-box
   :padding (Insets. 5.0 0.0 0.0 25)
   :style-class "nuestr-follow-set"
   :children [{:fx/type :list-view
               :padding 10
               :pref-height 220
               :pref-width 120
               :items (sort (map metadata/user-short-name (:follow-set temp-view)))}]})

(defn- radio-group [{:keys [options value temp-view metadata-cache]}]
  {:fx/type fx/ext-let-refs
   :refs {::toggle-group {:fx/type :toggle-group}}
   :desc {:fx/type :v-box
          :spacing 10
          :children (for [option options]
                      (let [button {:fx/type :radio-button
                                    :toggle-group {:fx/type fx/ext-get-ref
                                                   :ref ::toggle-group}
                                    :selected (= option value)
                                    :text (str option)
                                    :on-action (fn [_]
                                                 (update-temp-view! :follow
                                                                    (get (set/map-invert follow-options)
                                                                         option)))}]
                        (if (= option "user list")
                          {:fx/type :v-box
                           :children [button
                                      {:fx/type follow-set-pane
                                       :temp-view temp-view
                                       :metadata-cache metadata-cache}]}
                          button)))}})

(defn- follow-radiogroup [{:keys [temp-view metadata-cache]}]
  {:fx/type :h-box
   :padding 5
   :children [{:fx/type radio-group
               :options (vals follow-options)
               :temp-view temp-view
               :metadata-cache metadata-cache
               :value (get follow-options (:follow temp-view))}]})


(defn- relay-group
  [{:keys [temp-view]}]
  {:fx/type :v-box
   :children (let [relay-urls (:relay-urls temp-view)]
               (for [r (sort (domain/relay-urls @domain/*state))]
                 {:fx/type :h-box
                  :padding 5
                  :spacing 5
                  :children [{:fx/type :check-box
                              :selected (boolean (when relay-urls (get relay-urls r)))
                              :on-selected-changed (fn [e]
                                                     (update-temp-view! :relay-urls
                                                                        (if e
                                                                          (conj relay-urls r)
                                                                          (disj relay-urls r))))}
                             {:fx/type :label
                              :text r}]}))})

(defn- show-pictures-checkbox
  [{:keys [temp-view]}]
  {:fx/type :h-box
   :padding 5
   :spacing 5
   :children [{:fx/type :check-box
               :selected (boolean (:show-pictures temp-view))
               :on-selected-changed (fn [e]
                                      (update-temp-view! :show-pictures (boolean e)))}]})

  
(defn- right-hand-side
  [{:keys [views selected-view temp-view temp-view-changed? name metadata-cache]}]
  {:fx/type :v-box
   :padding 30
   :spacing 20
   :children [{:fx/type text-input
               :label "Name:"
               :style "nuestr-view-name"
               :value name
               :on-text-changed (fn [new-name]
                                  (update-temp-view! :name new-name))}
              {:fx/type :h-box
               :children [{:fx/type field-label :text "Show pictures:"}
                          {:fx/type show-pictures-checkbox
                           :temp-view temp-view}]}
              {:fx/type :h-box
               :spacing 60
               :children [{:fx/type :h-box
                           :children [{:fx/type field-label :text "Relays:"}
                                      {:fx/type relay-group
                                       :temp-view temp-view}]}
                          {:fx/type :h-box
                           :children [{:fx/type field-label :text "Follow:"}
                                      {:fx/type follow-radiogroup
                                       :temp-view temp-view
                                       :metadata-cache metadata-cache}]}]}
              ;; Save and Delete buttons
              {:fx/type :h-box
               :padding 5
               :children [{:fx/type field-label :text ""}
                          {:fx/type :h-box
                           :spacing 100
                           :children [;; Save button
                                      {:fx/type :button
                                       :disable (not temp-view-changed?)
                                       :text "Save"
                                       :padding 5
                                       :on-mouse-pressed {:event/type :save-view}}
                                      ;; Delete button (disabled when temp-view is for a
                                      ;; new view that hasn't been saved yet).
                                      {:fx/type :button
                                       :disable (not (domain/find-view selected-view))
                                       :text "Delete"
                                       :padding 5
                                       :on-mouse-pressed {:event/type :delete-view}}]}]} ]})

(defn show-tab
  [{:keys [views selected-view temp-view temp-view-changed? metadata-cache]}]
  (log/debugf "Views tab with selected=%s, temp=%s, changed=%s metadata-cache=%s"
              selected-view temp-view temp-view-changed? metadata-cache)
  {:fx/type :h-box
   :padding 10
   :children (let [items (sort (keys views))
                   value (or selected-view (first items))]
               (swap! domain/*state assoc
                      :selected-view value)
               [{:fx/type :v-box
                 :padding 20
                 :spacing 20
                 :children [;; List with the names of all views.
                            {:fx/type fx.ext.list-view/with-selection-props
                             :props {:selected-item value
                                     :on-selected-item-changed (fn [new-value]
                                                                 (let [view (domain/find-view new-value)]
                                                                   (assert view
                                                                           (format "Can't find view for %s" new-value))
                                                                   #_(log/debugf "View for %s = %s" new-value view)
                                                                   (swap! domain/*state assoc
                                                                          :selected-view new-value
                                                                          :temp-view view
                                                                          :temp-view-changed? false)))}
                             :desc {:fx/type :list-view
                                    :focus-traversable true
                                    :padding 10
                                    :pref-height 1000000 ; make it stretch vertically
                                    :items items}}
                            ;; 'Add new view' button (centered horizontally)
                            {:fx/type :h-box
                             :padding 20
                             :children [{:fx/type :h-box :h-box/hgrow :always}
                                        {:fx/type :button
                                         :text "Add new view"
                                         :padding 5
                                         :on-mouse-pressed {:event/type :add-view}}
                                        {:fx/type :h-box :h-box/hgrow :always}]}]}
                ;; Form to edit, save and delete the view.
                {:fx/type right-hand-side
                 :temp-view temp-view
                 :name value
                 :metadata-cache metadata-cache
                 :temp-view-changed? temp-view-changed?
                 :selected-view selected-view
                 :views views}])})


