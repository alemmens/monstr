(ns monstr.tab-views
  (:require
   [cljfx.api :as fx]
   [cljfx.ext.list-view :as fx.ext.list-view]
   [clojure.java.io :as io]
   [clojure.tools.logging :as log]
   [clojure.set :as set]
   [monstr.domain :as domain]
   [monstr.file-sys :as file-sys]
   [monstr.status-bar :as status-bar]
   ))

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
    (file-sys/save-views (:views @domain/*state))
    (status-bar/message! (format "Saved view '%s'" new-name))))

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

(defn new-view-name
  ([] (if (domain/find-view "New view name")
        (new-view-name 1)
        "New view name"))
  ([n] (if (domain/find-view (format "New view name %d" n))
         (new-view-name (inc 1))
         (format "New view name %d" n))))
                        
(defn add-view! []
  (let [new-name (new-view-name)
        new-view (domain/make-view new-name
                                   #{(first (sort (domain/relay-urls @domain/*state)))}
                                   #{})]
    (swap! domain/*state assoc-in
           [:views new-name]
           new-view)
    (swap! domain/*state assoc
           :selected-view (:name new-view)
           :temp-view new-view
           :temp-view-changed? true)
    (status-bar/message! (format "Added view '%s'" new-name))))

#_
(defn confirmation-alert
  ;; TODO: FINISH THIS.
  [_]
  {:fx/type :dialog
   :showing true
   :on-hidden (fn [^DialogEvent e]
                (condp = (.getButtonData ^ButtonType (.getResult ^Dialog (.getSource e)))
                  ButtonBar$ButtonData/NO (reset! *state :select-action)
                  ButtonBar$ButtonData/YES (reset! *state :confirmed)))
   :dialog-pane {:fx/type :dialog-pane
                 :header-text "Confirm"
                 :content-text "This view is used by one or more columns. These columns
will be removed when the view is deleted. Continue?"
                 :expandable-content {:fx/type :label
                                      :text "This action can't be undone."}
                 :button-types [:no :yes]}})

(defn delete-view! [selected-view]
  (let [views (:views @domain/*state)
        using-columns (domain/columns-using-view selected-view)]
    ;; TODO: Check if the view is in use. If so, ask for confirmation first.
    (when (> (count views) 1)
      ;; Move listview focus to the first view in the list.
      (let [new-view-name (first (sort (keys views)))
            new-view (domain/find-view new-view-name)
            new-columns (remove #(domain/column-uses-view? % selected-view)
                                (:all-columns @domain/*state))]
        (swap! domain/*state assoc
               :selected-view new-view-name
               :temp-view new-view
               :temp-view-changed? false
               :views (dissoc views selected-view)
               ;; Remove all columns (both visible and hidden) that use the deleted view.               
               :all-columns new-columns
               :visible-column-ids (map :id
                                        (keep domain/find-column-by-id
                                              (:visible-column-ids @domain/*state)))))
      ;; Remember that this view is now gone.
      (file-sys/save-views (:views @domain/*state))
      ;; Show status.
      (status-bar/message! (format "Deleted view '%s'" selected-view)))))

(defn right-hand-side
  [{:keys [views selected-view temp-view temp-view-changed? name]}]
  {:fx/type :v-box
   :padding 30
   :spacing 20
   :children [;; Name
              {:fx/type text-input
               :label "Name:"
               :style "monstr-view-name"
               :value name
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
                          {:fx/type :h-box
                           :spacing 100
                           :children [;; Save button
                                      {:fx/type :button
                                       :disable (not temp-view-changed?)
                                       :text "Save"
                                       :padding 5
                                       :on-mouse-pressed (fn [e] (save-view!))}
                                      ;; Delete button (disable when temp-view is for a
                                      ;; new view that hasn't been saved yet).
                                      {:fx/type :button
                                       :disable (not (domain/find-view selected-view))
                                       :text "Delete"
                                       :padding 5
                                       :on-mouse-pressed (fn [e] (delete-view! selected-view))}]}]}]})

(defn show-tab
  [{:keys [views selected-view temp-view temp-view-changed?]}]
  (log/debugf "Views tab with views=%s, selected=%s, temp=%s, changed=%s"
              views selected-view temp-view temp-view-changed?)
  {:fx/type :h-box
   :padding 10
   :children (let [items (sort (keys views))
                   value (or selected-view (first items))]
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
                            ;; 'Add new view' button (centered horizontally)
                            {:fx/type :h-box
                             :children [{:fx/type :h-box :h-box/hgrow :always}
                                        {:fx/type :button
                                         :text "Add new view"
                                         :padding 5
                                         :on-mouse-pressed (fn [_] (add-view!))}
                                        {:fx/type :h-box :h-box/hgrow :always}]}]}
                ;; Form to edit, save and delete the view.
                {:fx/type right-hand-side
                 :temp-view temp-view
                 :name value
                 :temp-view-changed? temp-view-changed?
                 :selected-view selected-view
                 :views views}])})


