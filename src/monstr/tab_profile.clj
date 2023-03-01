(ns monstr.tab-profile
  (:require
   [cljfx.api :as fx]
   [clojure.tools.logging :as log]
   [clojure.string :as str]
   [monstr.avatar :as avatar]
   [monstr.util :as util])
  (:import (javafx.geometry Insets)))


(defn keycard
  [{:keys [active? profile? public-key metadata identity_]}]
  (let [avatar-dim 75.0
        avatar-color (avatar/color public-key)
        picture-url (:picture-url metadata)]
    (log/debugf "Keycard for %s with metadata %s" public-key metadata)
    {:fx/type :v-box
     :max-height 400

     :style-class ["ndesk-keycard"]                   
     :children [{:fx/type :h-box
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
                    {:fx/type avatar/avatar
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
                               :text (if profile?
                                       ;; In the profile tab we have enough room to
                                       ;; show the whole public key.
                                       public-key
                                       (util/format-pubkey-short public-key))}]}]}
                {:fx/type :h-box
                 :padding 20
                 :children [{:fx/type :text
                             :style-class "ndesk-keycard-about"
                             :wrapping-width 400
                             :text (or (:about metadata) "")}]}]}))

(defn follow-pubkey! [pubkey]
  )

(defn follows
  [{:keys [pubkey identities identity-metadata views]}]
  (let [identities-with-secret-keys (remove (comp not :secret-key) identities)]  
    {:fx/type :grid-pane
     :children [{:fx/type :label :text "Follow for: "
                 :grid-pane/column 1
                 :grid-pane/row 1}
                {:fx/type :v-box
                 :grid-pane/column 2
                 :grid-pane/row 1
                 :padding 10
                 :spacing 10
                 :children (for [id identities-with-secret-keys]
                             (let [pubkey (:public-key id)
                                   name (or (:name (get identity-metadata pubkey))
                                            pubkey)]
                               {:fx/type :h-box
                                :spacing 10
                                :children [{:fx/type :check-box
                                            :selected false
                                            :on-selected-changed identity}
                                           {:fx/type :label :text name}]}))}
                {:fx/type :v-box
                 :grid-pane/column 1
                 :grid-pane/row 2
                 :children [{:fx/type :label :text "Make visible in: "}
                            {:fx/type :v-box :v-box/vgrow :always}]}
                {:fx/type :v-box
                 :spacing 10
                 :padding (Insets. 0.0 0.0 0.0 10.0)
                 :grid-pane/column 2
                 :grid-pane/row 2
                 :children (for [name (sort (keys views))]
                             {:fx/type :h-box
                              :spacing 10
                              :children [{:fx/type :check-box
                                          :selected false
                                          :on-selected-changed identity}
                                         {:fx/type :label :text name}]})}
                {:fx/type :h-box
                 :grid-pane/column 2
                 :grid-pane/row 3
                 :padding 10
                 :children [{:fx/type :button
                             :on-action identity
                             :text "Save"}]}]}))
              

(defn profile
  [{:keys [pubkey identities views metadata identity-metadata]}]
  (let [identity (first (filter (fn [id] (= (:public-key id) pubkey))
                                identities))]
    (log/debugf "Profile for %s with identity %s and metadata %s" pubkey identity metadata)
    (if pubkey
      {:fx/type :v-box
       :padding 20
       :spacing 10
       :children [{:fx/type keycard
                   :fx/key pubkey
                   :public-key pubkey
                   :active? false
                   :profile? true
                   :identity_ identity
                   :metadata metadata}
                  {:fx/type :h-box
                   :padding 10
                   :children [(if identity
                                {:fx/type :button
                                 :text "Delete identity"
                                 :disable true ; TODO: fix this
                                 :on-action {:event/type :delete-keycard :identity_ identity}}
                                {:fx/type :v-box
                                 :spacing 20
                                 :children [{:fx/type follows
                                             :pubkey pubkey
                                             :identity-metadata identity-metadata
                                             :identities identities
                                             :views views}]})]}]}
      {:fx/type :label
       :text "No pubkey found for profile"})))


