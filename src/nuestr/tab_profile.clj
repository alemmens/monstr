(ns nuestr.tab-profile
  (:require
   [cljfx.api :as fx]
   [clojure.tools.logging :as log]
   [clojure.string :as str]
   [nuestr.domain :as domain]
   ; [nuestr.event :as event]
   [nuestr.file-sys :as file-sys]
   [nuestr.media :as media]
   [nuestr.nip19 :as nip19]
   [nuestr.status-bar :as status-bar]
   [nuestr.store :as store]
   [nuestr.timeline :as timeline]
   [nuestr.util :as util])
  (:import (javafx.geometry Insets)
           (javafx.scene.control DialogEvent Dialog Button TextArea)
           (javafx.scene.layout VBox HBox Priority)))


(defn keycard
  [{:keys [active? profile? public-key metadata identity_]}]
  (let [avatar-dim 75.0
        avatar-color (media/color public-key)
        picture-url (:picture-url metadata)]
    (log/debugf "Keycard for %s with metadata %s" public-key metadata)
    {:fx/type :v-box
     :pref-width 600
     :padding 0
     :spacing 15
     :style-class ["ndesk-keycard"]                   
     :children (remove nil?
                       [{:fx/type :h-box
                         :children
                         [(if (str/blank? picture-url)
                            {:fx/type :label
                             :min-width avatar-dim
                             :min-height avatar-dim
                             :max-width avatar-dim
                             :max-height avatar-dim
                             :style {:-fx-background-color avatar-color}
                             :style-class "ndesk-keycard-photo"
                             :text (subs public-key 0 3)}
                            {:fx/type media/avatar
                             :picture-url picture-url
                             :width avatar-dim})
                          {:fx/type :v-box
                           :children [{:fx/type :label
                                       :alignment :top-left
                                       :style-class ["label" "ndesk-keycard-name"]
                                       :text (or (:name metadata) "")}
                                      {:fx/type :label
                                       :alignment :top-left
                                       :style-class ["label" "ndesk-keycard-pubkey"]
                                       :text (let [npub (nip19/encode "npub" public-key)]
                                               (if profile?
                                                 ;; In the profile tab we have enough room to
                                                 ;; show the whole public key.
                                                 npub
                                                 (util/format-string-short npub)))}]}]}
                        {:fx/type :h-box
                         :children [{:fx/type :text
                                     :style-class "ndesk-keycard-about"
                                     :wrapping-width 500
                                     :text (or (:about metadata) "")}]}
                        (when identity_
                          {:fx/type :button
                           :text "Delete account"
                           :on-action {:event/type :delete-account
                                       :identity identity_}})])}))

(defn followers-pane
  [{:keys [pubkey open-profile-states identities identity-metadata]}]
  (let [followers (:followers (get open-profile-states pubkey))
        switch (fn [id]
                 (swap! domain/*state assoc-in
                        [:open-profile-states pubkey :followers-changed?]
                        true)
                 (swap! domain/*state assoc-in
                        [:open-profile-states pubkey :followers]
                        (if (get followers pubkey)
                          (disj followers pubkey)
                          (conj followers pubkey))))]
    {:fx/type :scroll-pane
     :padding 20
     :hbar-policy :never
     :vbar-policy :as-needed
     :content {:fx/type :v-box
               ; :spacing 5
               :children [{:fx/type :label :text "Accounts that follow this user:"}
                          {:fx/type :v-box
                           :padding 10
                           :spacing 10
                           :children (for [id (sort-by domain/identity-name identities)]
                                       (let [pubkey (:public-key id)]
                                         {:fx/type :h-box
                                          :spacing 10
                                          :children [{:fx/type :check-box
                                                      :disable (not (:secret-key id))
                                                      :selected (boolean (get followers pubkey))
                                                      :on-selected-changed (fn [_] (switch id))}
                                                     {:fx/type :label
                                                      :disable (not (:secret-key id))
                                                      :text (domain/identity-name id)}]}))}
                          {:fx/type :h-box
                           :padding 10
                           :children [{:fx/type :button
                                       :disable (not (:followers-changed?
                                                      (get open-profile-states pubkey)))                                       
                                       :on-action identity
                                       :text "Save"}]}]}}))


(defn save-following-views [event views pubkey]
  ;; Disable the button now, because otherwise it will take too long.
  (let [^Button button (.getSource event)]
    (.setDisable button true))
  ;;
  (let [profile-state (get (:open-profile-states @domain/*state) pubkey)
        following-views (:following-views profile-state)]
    #_(log/debugf "Saving following views for %s, changed=%s"
                  pubkey (:following-views-changed profile-state))
    (doseq [v (keys views)]
      ;; Update view's follow set.          
      (let [follow-set (:follow-set (domain/find-view v))
            new-follow-set (if (following-views v)
                             (conj follow-set pubkey)
                             (disj follow-set pubkey))]
        (domain/update-view! v :follow-set new-follow-set)))
    ;; Update the columns that use the views.
    (let [new-columns (map #(assoc % :view (domain/find-view (:name (:view %))))
                           (:all-columns @domain/*state))]
      (swap! domain/*state assoc :all-columns new-columns))
    (fx/run-later
     ;; Use a trick to be able to refer to hydrate/refresh-column! without getting
     ;; cyclical reference problems.
     (let [hydrate-namespace (find-ns 'nuestr.hydrate)
           refresh-column (ns-resolve hydrate-namespace (symbol "refresh-column!"))]
       (doseq [v (:following-views-changed profile-state)]
         ;; Refresh column if its view has changed.
         (let [column (domain/find-column-with-view-name v)]
           (assert column)
           (refresh-column column))))))
  ;; Save views.
  (file-sys/save-views (:views @domain/*state))
  (swap! domain/*state assoc-in
         [:open-profile-states pubkey :following-views-changed]
         #{}))

(defn following-views-pane
  [{:keys [pubkey open-profile-states views]}]
  (let [following-views (:following-views (get open-profile-states pubkey))
        switch (fn [view-name]
                 (let [profile-state (get (:open-profile-states @domain/*state) pubkey)
                       following-views (:following-views profile-state)
                       following-views-changed (:following-views profile-state)]
                   (log/debugf "Switching with following views: %s"
                               (pr-str following-views))                   
                   (swap! domain/*state assoc-in
                          [:open-profile-states pubkey :following-views-changed]
                          (conj following-views-changed view-name))
                   (swap! domain/*state assoc-in
                          [:open-profile-states pubkey :following-views]
                          (if (get following-views view-name)
                            (disj following-views view-name)
                            (conj following-views view-name)))))]
    (log/debugf "Following views: %s" (pr-str following-views))
    {:fx/type :scroll-pane
     :padding 20
     :pref-width 600
     :hbar-policy :never
     :vbar-policy :as-needed
     :content {:fx/type :v-box
               :children [{:fx/type :label :text "Show in: "}
                          {:fx/type :v-box
                           :spacing 10
                           :padding 10
                           :children (for [name (sort (keys views))]
                                       {:fx/type :h-box
                                        :spacing 10
                                        :children [{:fx/type :check-box
                                                    :selected (boolean (get following-views name))
                                                    :on-selected-changed (fn [_] (switch name))}
                                                   {:fx/type :label :text name}]})}
                          {:fx/type :h-box
                           :padding 10
                           :children [(let [id (str "profile-button-" pubkey)]
                                        {:fx/type :button
                                         :id id
                                         :disable (empty? (:following-views-changed
                                                           (get (:open-profile-states @domain/*state) pubkey)))
                                         :on-action (fn [e] (save-following-views e views pubkey))
                                         :text "Save"})]}]}}))
  
(defn follows
  [{:keys [pubkey open-profile-states identities identity-metadata views]}]
  {:fx/type :h-box
   :padding 10
   :spacing 1;  10
   :children [#_ ; TODO: FINISH THIS
              {:fx/type followers-pane
               :pubkey pubkey
               :open-profile-states open-profile-states
               :identity-metadata identity-metadata
               :identities identities
               :views views}
              {:fx/type following-views-pane
               :pubkey pubkey
               :open-profile-states open-profile-states
               :identity-metadata identity-metadata
               :identities identities
               :views views}]})
              

(defn user-posts
  [{:keys [profile-state pubkey]}]
  (let [timeline-pair (:timeline-pair profile-state)
        thread? (:show-thread? profile-state)
        listview (if thread?
                   (:thread-listview timeline-pair)
                   (:flat-listview timeline-pair))]
    (log/debugf "%d user posts" (count (:item-ids (domain/timeline timeline-pair thread?))))
    {:fx/type :v-box
     ; :padding 10
     :children (if (nil? listview)
                 []
                 [{:fx/type :h-box
                   :padding (if thread? 10 0)
                   :style-class "relay-timeline-label"                   
                   :children (if thread?
                               [{:fx/type :h-box :h-box/hgrow :always}
                                (timeline/back-from-thread-button nil pubkey)
                                {:fx/type :label :text "thread" :padding 5}
                                {:fx/type :h-box :h-box/hgrow :always}]
                               [])}
                  {:fx/type fx/ext-instance-factory
                   :create #(doto listview
                              (VBox/setVgrow Priority/ALWAYS))}])}))
  
(defn profile
  [{:keys [pubkey identities views metadata identity-metadata open-profile-states]}]
  (let [identity (first (filter (fn [id] (= (:public-key id) pubkey))
                                identities))]
    (log/debugf "Profile for %s with states %s"
                pubkey (pr-str (keys open-profile-states)))
    (if pubkey
      {:fx/type :h-box
       :padding 10
       :spacing 20
       :children [{:fx/type :v-box
                   ; :padding 10
                   :spacing 1;  5
                   :children (remove nil?
                                     [{:fx/type :h-box
                                       :padding 10
                                       :children [{:fx/type keycard
                                                   :fx/key pubkey
                                                   :public-key pubkey
                                                   :active? false
                                                   :profile? true
                                                   :identity_ identity
                                                   :metadata metadata}
                                                  ]}
                                      {:fx/type follows
                                       :pubkey pubkey
                                       :open-profile-states open-profile-states
                                       :identity-metadata identity-metadata
                                       :identities identities
                           :views views}])}
                  ;; Posts by this user.
                  {:fx/type user-posts
                   :pubkey pubkey
                   :profile-state (get open-profile-states pubkey)}]}
      {:fx/type :label
       :text "No pubkey found for profile"})))


