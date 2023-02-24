(ns monstr.tab-views
  (:require
   [cljfx.api :as fx]
   [cljfx.ext.list-view :as fx.ext.list-view]
   [clojure.java.io :as io]
   [clojure.tools.logging :as log]
   [clojure.set :as set]
   [monstr.domain :as domain]
   [monstr.file-sys :as file-sys]))

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
   :min-width 60
   :max-width 60
   :text text})

(defn- text-input [{:keys [label value on-text-changed]}]
  {:fx/type :h-box
   :children [{:fx/type field-label
               :text label}
              {:fx/type :text-field
               :pref-column-count 20
               :text value
               :style-class ["text-input" "monstr-view-name"]
               :on-text-changed on-text-changed}]})

(defn- save-view!
  []
  (let [temp-view (:temp-view @domain/*state)
        old-name (:selected-view @domain/*state)
        new-name (:name temp-view)]
    (log/debugf "Saving view with new name %s and old name %s and relays %s"
                new-name
                old-name
                (:relay-urls temp-view))
    (swap! domain/*state assoc-in
           [:views new-name]
           temp-view)
    (swap! domain/*state assoc :temp-view-changed? false)
    (when-not (= old-name new-name)
      (swap! domain/*state assoc
             :views (dissoc (:views @domain/*state) old-name)
             :selected-view new-name))
    (file-sys/save-views (:views @domain/*state))))

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
   {:all "all (global)",
    :use-identity "contacts of active identity"})

(defn- radio-group [{:keys [options value on-action]}]
  {:fx/type fx/ext-let-refs
   :refs {::toggle-group {:fx/type :toggle-group}}
   :desc {:fx/type :v-box
          :spacing 5
          :children (for [option options]
                      {:fx/type :radio-button
                       :toggle-group {:fx/type fx/ext-get-ref
                                      :ref ::toggle-group}
                       :selected (= option value)
                       :text (str option)
                       :on-action (fn [_]
                                    (update-temp-view! :follow
                                                       (get (set/map-invert follow-options)
                                                            option)))})}})

(defn- follow-radiogroup [{:keys [temp-view]}]
  {:fx/type :h-box
   :padding 5
   :children [{:fx/type radio-group
               :options (vals follow-options)
               :value (get follow-options (:follow temp-view))}]})

(defn- relay-checkbox [relay-url selected]
  {:fx/type :h-box
   :padding 5
   :spacing 5
   :children [{:fx/type :check-box
               :selected selected
               :on-selected-changed (fn [_] :later)}
              {:fx/type :label
               :text relay-url}]})

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
                                                     (log/debugf "Selected of %s changed to %s"
                                                                 r e)
                                                     (update-temp-view! :relay-urls
                                                                        (if e
                                                                          (conj relay-urls r)
                                                                          (disj relay-urls r))))}
                             {:fx/type :label
                              :text r}]}))})

(defn show-tab
  [{:keys [views selected-view temp-view temp-view-changed?]}]
  (log/debugf "Views tab with views=%s, selected=%s, temp=%s, changed=%s"
              views selected-view temp-view temp-view-changed?)
  {:fx/type :h-box
   :padding 10
   :children (let [items (sort (keys views))
                   value (or selected-view (first items))]
               [;; List with the names of all views.
                {:fx/type fx.ext.list-view/with-selection-props
                 :props {:selected-item value
                         :on-selected-item-changed (fn [new-value]
                                                     (let [view (domain/find-view new-value)]
                                                       (assert view
                                                               (format "Can't find view for %s" new-value))
                                                       (log/debugf "View for %s = %s" new-value view)
                                                       (swap! domain/*state assoc
                                                              :selected-view new-value
                                                              :temp-view view
                                                              :temp-view-changed? false)))}
                 :desc {:fx/type :list-view
                        :focus-traversable true
                        :padding 10
                        :pref-height 1000000 ; make it stretch vertically
                        :items items}}
                ;; Fields to edit the view.
                {:fx/type :v-box
                 :padding 30
                 :spacing 20
                 :children [;; Name
                            {:fx/type text-input
                             :label "Name:"
                             :style "monstr-view-name"
                             :value value
                             :on-text-changed (fn [new-name]
                                                (update-temp-view! :name new-name))}
                            ;; Follow
                            {:fx/type :h-box
                             :children [{:fx/type field-label :text "Follow:"}
                                        {:fx/type follow-radiogroup
                                         :temp-view temp-view}]}
                            ;; Relays
                            {:fx/type :h-box
                             :children [{:fx/type field-label :text "Relays:"}
                                        {:fx/type relay-group
                                         :temp-view temp-view}]}
                            ;; Save button
                            {:fx/type :h-box
                             :children [{:fx/type field-label :text ""}
                                        {:fx/type :button
                                         :disable (not temp-view-changed?)
                                         :text "Save"
                                         :padding 5
                                         :on-mouse-pressed (fn [e] (save-view!))}]}
                            ]}])})

