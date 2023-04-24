(ns nuestr.tab-profile
  (:require
   [cljfx.api :as fx]
   [clojure.tools.logging :as log]
   [clojure.string :as str]
   [nuestr.domain :as domain]
   [nuestr.file-sys :as file-sys]
   [nuestr.media :as media]
   [nuestr.nip19 :as nip19]
   [nuestr.status-bar :as status-bar]
   [nuestr.store :as store]
   [nuestr.timeline :as timeline]
   [nuestr.util :as util])
  (:import (javafx.geometry Insets)
           (javafx.scene.layout VBox HBox Priority)))


(defn keycard
  [{:keys [active? profile? public-key metadata identity_]}]
  (let [avatar-dim 75.0
        avatar-color (media/color public-key)
        picture-url (:picture-url metadata)]
    (log/debugf "Keycard for %s with metadata %s" public-key metadata)
    {:fx/type :v-box
     :pref-width 600
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
                                        ; :padding 10
                         :children [{:fx/type :text
                                     :style-class "ndesk-keycard-about"
                                     :wrapping-width 400
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
                           :children [{:fx/type :button
                                       :disable (empty? (:following-views-changed
                                                         (get (:open-profile-states @domain/*state) pubkey)))
                                       :on-action {:event/type :save-following-views
                                                   :views views
                                                   :pubkey pubkey}
                                       :text "Save"}]}]}}))
  
(defn follows
  [{:keys [pubkey open-profile-states identities identity-metadata views]}]
  {:fx/type :h-box
   :padding 10
   :spacing 10
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
     :padding 10
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
       :spacing 10
       :children [{:fx/type :v-box
                   ; :padding 10
                   :spacing 5
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


