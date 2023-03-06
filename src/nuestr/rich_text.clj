(ns monstr.rich-text
  (:require
   [monstr.util-fx :as util-fx]
   [monstr.util-java :as util-java]
   [monstr.util :as util]
   [clojure.tools.logging :as log]
   [clojure.string :as str])
  (:import (org.fxmisc.richtext.model TextOps SegmentOps SegmentOpsBase StyledSegment ReadOnlyStyledDocument)
           (java.util Optional)
           (org.fxmisc.richtext GenericStyledArea TextExt)
           (javafx.scene Node)
           (javafx.geometry VPos)))

;; --

(defrecord HyperlinkSeg
  [^String text ^String url])

(defonce hyperlink-ops
  (proxy [SegmentOpsBase] [(->HyperlinkSeg "" "")]
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

(defn text-ext*
  ^TextExt [^String text]
  (doto (TextExt.)
    (.setTextOrigin VPos/TOP)
    (.setText text)))

(defn hyperlink*
  ^TextExt [{:keys [text url]}]
  (doto (text-ext* text)
    (util-fx/add-style-class! "hyperlink")
    (util-fx/on-mouse-clicked!
      (fn [& _] (util/open-url! url)))))

;; --

(defonce seg-ops*
  (or*
    (SegmentOps/styledTextOps)
    #(cond
       (instance? HyperlinkSeg %) hyperlink-ops)))

(defn node-factory*
  ^Node [^StyledSegment styled-seg]
  (let [seg (.getSegment styled-seg)]
    (cond
      (instance? HyperlinkSeg seg) (hyperlink* seg)
      :else (text-ext* seg))))

(defn create*
  ^GenericStyledArea []
  (let [initial-paragraph-style nil
        apply-paragraph-style (util-java/->BiConsumer (fn [& _]))
        initial-text-style nil]
    ;; can we avoid proxy for something like gen-class?
    (proxy [GenericStyledArea]
           [initial-paragraph-style
            apply-paragraph-style
            initial-text-style
            seg-ops*
            (util-java/->Function node-factory*)]
      #_(computePrefHeight [^double width]
          ;(log/info 'computePrefHeight (proxy-super getText))
          (proxy-super computePrefHeight width)))))

(defn append-text!
  [^GenericStyledArea n s]
  (when (not-empty s)
    (.appendText n s)))

(defn append-hyperlink!
  [^GenericStyledArea n text url]
  (when (not-empty url)
    (.append n
      (ReadOnlyStyledDocument/fromSegment
        (->HyperlinkSeg text url)
        nil ;; paragraph style
        nil ;; text style
        seg-ops*))))
