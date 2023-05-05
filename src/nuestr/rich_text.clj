(ns nuestr.rich-text
  (:require
   [clojure.tools.logging :as log]
   [clojure.string :as str]
   [nuestr.byte-vector :as byte-vector]
   [nuestr.domain :as domain]
   [nuestr.modal :as modal]
   [nuestr.nip19 :as nip19]
   [nuestr.status-bar :as status-bar]
   [nuestr.store :as store]
   [nuestr.tab-relays :as tab-relays]
   [nuestr.timeline :as timeline]
   [nuestr.util-fx :as util-fx]
   [nuestr.util-java :as util-java]
   [nuestr.util :as util])
  (:import (org.fxmisc.richtext.model TextOps SegmentOps SegmentOpsBase StyledSegment ReadOnlyStyledDocument)
           (java.util Optional)
           (org.fxmisc.richtext GenericStyledArea TextExt)
           (javafx.scene Node)
           (javafx.geometry VPos)))

;; --

(defrecord HyperlinkSeg
  [^String text ^String url column-id pubkey])

(defonce hyperlink-ops
  (proxy [SegmentOpsBase] [(->HyperlinkSeg "" "" nil nil)]
    (^int length [o]
      (count (:text o)))
    (^char realCharAt [o idx]
      (.charAt (:text o) idx))
    (^String realGetText [o]
      (:text o))
    (realSubSequence
      ([o start]
       (update o :text subs start))
      ([o start end]
       (update o :text subs start end)))
    (joinSeg [curr-seg next-seg]
      (Optional/empty))))

;; --

(defn or*
  [^TextOps text-ops seg->seg-ops]
  (let [seg->seg-ops' #(or (seg->seg-ops %) text-ops)]
    (reify TextOps
      (createEmptySeg [_]
        (.createEmptySeg text-ops))
      (create [_ s]
        (.create text-ops s))
      (length [_ seg]
        (.length ^SegmentOps (seg->seg-ops' seg) seg))
      (charAt [_ seg idx]
        (.charAt ^SegmentOps (seg->seg-ops' seg) seg idx))
      (getText [_ seg]
        (.getText ^SegmentOps (seg->seg-ops' seg) seg))
      (subSequence [_ seg start]
        (.subSequence ^SegmentOps (seg->seg-ops' seg) seg start))
      (subSequence [_ seg start end]
        (.subSequence ^SegmentOps (seg->seg-ops' seg) seg start end))
      (joinSeg [_ curr-seg next-seg]
        (Optional/empty)))))

;; --

(defn text-ext* ^TextExt [^String text]
  (doto (TextExt.)
    (.setTextOrigin VPos/TOP)
    (.setText text)))

(defn hyperlink* ^TextExt [{:keys [text url column-id pubkey]}]
  (doto (text-ext* text)
    (util-fx/add-style-class! "hyperlink")
    (util-fx/on-mouse-clicked!
     (fn [e]
       (if (str/starts-with? url "nostr:")
         (let [url-rest (subs url (count "nostr:"))]
           (cond (str/starts-with? url-rest "npub")
             (let [[_ pubkey] (nip19/decode url-rest)]
               (status-bar/message! (format "Pubkey: %s" pubkey))
               (timeline/open-profile e pubkey))
             ;;
             (str/starts-with? url-rest "note")
             (let [[_ event-id] (nip19/decode url-rest)]
               (status-bar/message! (format "Note: %s" pubkey))
               (let [column (and column-id (domain/find-column-by-id column-id))
                     continuation (fn [event]
                                    (if event
                                      (timeline/show-column-thread! domain/*state column pubkey event)
                                      (modal/info-popup (format "Can't find %s" url-rest))))]
                 (timeline/find-event-with-id event-id [] continuation 3000)))
             ;;
             (str/starts-with? url-rest "nevent")
             (let [[_ {:keys [special relays author kind]}] (nip19/decode url-rest)]
               (status-bar/message! (format "nevent: %s" pubkey))
               (run! tab-relays/maybe-add-relay! relays)
               (let [column (and column-id (domain/find-column-by-id column-id))
                     continuation (fn [event]
                                    (if event
                                      (timeline/show-column-thread! domain/*state column pubkey event)
                                      (modal/info-popup (format "Can't find event %s on %s"
                                                                special
                                                                (pr-str relays)))))]
                 (timeline/find-event-with-id special relays continuation 5000)))
             ;;
             :else (util/open-url! url)))
         (util/open-url! url))))))

;; --

(defonce seg-ops*
  (or*
    (SegmentOps/styledTextOps)
    #(cond
       (instance? HyperlinkSeg %) hyperlink-ops)))

(defn node-factory* ^Node [^StyledSegment styled-seg column-id pubkey]
  (let [seg (.getSegment styled-seg)]
    (if (instance? HyperlinkSeg seg)
      (hyperlink* (assoc seg
                         :column-id column-id
                         :pubkey pubkey))
      (text-ext* seg))))

(defn create* ^GenericStyledArea [column-id pubkey]
  (let [initial-paragraph-style nil
        apply-paragraph-style (util-java/->BiConsumer (fn [& _]))
        initial-text-style nil]
    ;; can we avoid proxy for something like gen-class?
    (proxy [GenericStyledArea]
           [initial-paragraph-style
            apply-paragraph-style
            initial-text-style
            seg-ops*
            (util-java/->Function #(node-factory* %1 column-id pubkey))]
      #_(computePrefHeight [^double width]
          ;(log/info 'computePrefHeight (proxy-super getText))
          (proxy-super computePrefHeight width)))))

(defn append-text!
  [^GenericStyledArea n s]
  (when (not-empty s)
    (.appendText n s)))

(defn append-hyperlink!
  [^GenericStyledArea n text url column-id pubkey]
  (when (not-empty url)
    (.append n
             (ReadOnlyStyledDocument/fromSegment (->HyperlinkSeg text url column-id pubkey)
                                                 nil ;; paragraph style
                                                 nil ;; text style
                                                 seg-ops*))))
