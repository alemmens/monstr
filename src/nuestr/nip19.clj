(ns nuestr.nip19
  (:require
   [clojure.string :as str]
   [nuestr.byte-vector :as byte-vector])
  (:use clojure.test))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NIP 19: bech32-encoded entities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; References:
;;;
;;; - NIP  19: https://github.com/nostr-protocol/nips/blob/master/19.md
;;; - BIP 173: https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bech32 encoding/decoding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def alphabet "qpzry9x8gf2tvdw0s3jn54khce6mua7l")

(def char-map
  (into {} (map (fn [char int] [char int])
                alphabet
                (range))))

(defn- bech32-char-to-int [char]
  (get char-map char))

(defn- bech32-int-to-char [int]
  (nth alphabet int))

(defn- encode-bech32
  "Returns a string with the bech32 encoding of `bytes`."
  [bytes]
  (let [bits (byte-vector/to-bits bytes (* 8 (count bytes)))
        bytes-5 (byte-vector/from-bits bits 5)]
    (apply str (map bech32-int-to-char bytes-5))))

(defn- decode-bech32
  "Takes a bech32 encoded string and returns the corresponding byte vector."
  [string]
  (let [bytes-5 (map bech32-char-to-int string)
        bit-count (* 5 (count string))        
        bits (byte-vector/to-bits bytes-5 bit-count 5)
        nr-superfluous-bits (rem (* 5 bit-count) 8)]
    (byte-vector/from-bits (byte-vector/slice bits 0 (- bit-count nr-superfluous-bits))
                           8)))

;;;
;;; Checksums
;;;

(def GEN [0x3b6a57b2 0x26508e6d 0x1ea119fa 0x3d4233dd 0x2a1462b3])

(defn- polymod
  "Takes a byte vector and returns an integer checksum."
  [bytes]
  (loop [values bytes
         chk 1]
    (if (empty? values)
      chk
      (let [b (bit-shift-right chk 25)
            chk (bit-xor (bit-shift-left (bit-and chk 0x1ffffff)
                                         5)
                         (first values))
            chk (reduce (fn [chk i]
                          (bit-xor chk
                                   (if (odd? (bit-shift-right b i))
                                     (nth GEN i)
                                     0)))
                        chk
                        (range 5))]
        (recur (rest values) chk)))))

(defn- expand-prefix
  "Returns a byte vector."
  [prefix]
  (let [bytes (byte-vector/from-ascii prefix)]
    (byte-vector/append (map #(bit-shift-right % 5) bytes)
                        (byte-vector/make 0)
                        (map #(bit-and % 31) bytes))))


(defn- create-checksum
  "Returns a bech32 encoded string of 6 characters."
  [prefix bytes]
  (let [mod (bit-xor (polymod (byte-vector/append (expand-prefix prefix)
                                                  bytes
                                                  (byte-vector/make 0 0 0 0 0 0)))
                     1)]
    (apply str
           (map bech32-int-to-char
                (map #(bit-and (bit-shift-right mod (* 5 (- 5 %)))
                               31)
                     (range 6))))))

(defn verify-checksum?
  "Returns true iff the checksum is OK. `prefix` is an ascii string, `data` and
  checksum are bech32 encoded strings."
  [prefix data checksum]
  (let [mod (polymod (byte-vector/append (expand-prefix prefix)
                                         (map bech32-char-to-int data)
                                         (map bech32-char-to-int checksum)))]
    (= 1 mod)))

;;;
;;; Tests
;;;

(deftest bech32
  (is (= (expand-prefix "npub")
         '(3 3 3 3 0 14 16 21 2)))
  (is (= (expand-prefix "nprofile")
         '(3 3 3 3 3 3 3 3 0 14 16 18 15 6 9 12 5)))
  (is (= (create-checksum "a" '())
         "2uel5l"))
  (is (= (create-checksum "npub"
                          (map bech32-char-to-int
                               "80cvv07tjdrrgpa0j7j7tmnyl2yr6yr7l8j4s3evf6u64th6gkws"))
         "yjh6w6"))
  (is (verify-checksum? "npub"
                        "80cvv07tjdrrgpa0j7j7tmnyl2yr6yr7l8j4s3evf6u64th6gkws"
                        "yjh6w6"))
  (doseq [[bech32 decoded]
          [["5q" '(160)]
           ["0elfcs4fr0l0r8af98jlmgdh9c8tcxjvz9qkw038js35mp4dma8q"
           (byte-vector/from-hex "7e7e9c42a91bfef19fa929e5fda1b72e0ebc1a4c1141673e2794234d86addf4e")]]]
    (is (= (decode-bech32 bech32) decoded))
    (is (= (encode-bech32 decoded) bech32))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NIP-19 decoding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- parse-tlv
  "Returns a sequence with 0 or more TLV parts, where each part is a vector
  of [1] the Type byte, [2] the Length byte, [3] the Value bytes. Returns
  nil if the data is not in tlv format."
  [data]
  (let [[type length & rest] data]
    (when (and type
               length
               (>= (count rest) length))
      (let [part [type length (take length rest)]]      
        (if-let [rest-data (seq (drop length rest))]
          (when-let [tlv (parse-tlv rest-data)]
            (cons part tlv))
          (list part))))))

(deftest tlv
  (is (= (parse-tlv '(1 2 3 4 0 3 1 1 1))
         '([1 2 (3 4)]
           [0 3 (1 1 1)]))))

(defn- tlv-parts-with-type [tlv type]
  (keep #(let [[t _ value] %]
           (when (= type t)
             value))
          tlv))

(defn- decode-relay [byte-vector]
  (byte-vector/to-ascii byte-vector))

(defn- parse-content
  "`data` is a byte vector, i.e. a sequence of integers 0<=n<=255."
  [prefix data]
  (if (or (#{"npub" "nsec" "note"} prefix)
          (not (#{"nprofile" "nevent" "nrelay" "naddr"} prefix)))
    (byte-vector/to-hex data)
    (let [tlv (parse-tlv data)]
      (when-let [special (first (tlv-parts-with-type tlv 0))]
        (case prefix
          "nprofile" {:special (byte-vector/to-hex special)
                      :relays (map decode-relay (tlv-parts-with-type tlv 1))}
          "nevent"   {:special (byte-vector/to-hex special)
                      :relays (map decode-relay (tlv-parts-with-type tlv 1))
                      :author (when-let [author (first (tlv-parts-with-type tlv 2))]
                                (byte-vector/to-hex author))
                      :kind (when-let [kind (first (tlv-parts-with-type tlv 3))]
                              (byte-vector/to-integer kind))}
          "nrelay"   {:special (decode-relay special)}
          "naddr"    (when-let [author (first (tlv-parts-with-type tlv 2))]
                       (when-let [kind (first (tlv-parts-with-type tlv 3))]
                         {:special (byte-vector/to-hex special)
                          :relays (map decode-relay (tlv-parts-with-type tlv 1))
                          :author (byte-vector/to-hex author)
                          :kind (byte-vector/to-integer kind)})))))))


(defn decode
  "Returns a vector with [1] the prefix string (also called 'human readable part' or
  'hrp'), [2] the data. The format of the data depends on the prefix. The default (used
  for `npub`, `nsec`, `note` and non-recognized prefixes) is a string with the hex
  encoding of the bytes. For `nprofile`, `nevent`, `nrelay`, and `naddr` it is a map with
  the following possible keys: `:special`, `:relays`, `:author`, `:kind`. See NIP-19 for
  more details.
  Returns nil if decoding is not possible."
  [string]
  (let [[prefix rest] (str/split string #"1" 2)]
    (when rest
      (let [size (- (count rest) 6)
            checksum (subs rest size)
            data (subs rest 0 size)]
        (try (when (verify-checksum? prefix data checksum)
               (when-let [content (parse-content prefix (decode-bech32 data))]
                 [prefix content]))
             (catch Throwable e
               nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NIP-19 encoding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- hex-to-tlv-part [kind hex]
  (let [bytes (byte-vector/from-hex hex)]
    (concat [kind (count bytes)]
            bytes)))

(defn- relay-to-tlv-part [kind string]
  (let [bytes (byte-vector/from-ascii string)]
    (concat [kind (count bytes)]
            bytes)))

(defn- relays-to-tlv-parts [strings]
  (map #(relay-to-tlv-part 1 %) strings))

(defn- encode-tlv
  "Each part in `parts` is a vector of the form [kind length & bytes].
  Returns a byte vector."
  [parts]
  (apply concat parts))

(defn- encode-content [prefix data]
  "`data` is either [1] a hex encoding of a byte vector or [2] a map as described in the
  documentation of `decode`. Returns a byte vector."
  [prefix data]
  (if (#{"npub" "nsec" "note"} prefix)
    (byte-vector/from-hex data)
    (if-let [special (:special data)]
      (let [relays (relays-to-tlv-parts (:relays data))]
        (encode-tlv (case prefix
                      "nprofile" (cons (hex-to-tlv-part 0 special)
                                       relays)
                      "nevent" (concat [(hex-to-tlv-part 0 special)]
                                       relays
                                       (when-let [author (:author data)]
                                         [(hex-to-tlv-part 2 author)])
                                       (when-let [kind (:kind data)]
                                         [(byte-vector/from-integer kind)]))
                      "nrelay" [(relay-to-tlv-part 0 special)]
                      "naddr" (concat [(hex-to-tlv-part 0 special)]
                                      relays
                                      (when-let [author (:author data)]
                                        [(hex-to-tlv-part 2 author)])
                                      (when-let [kind (:kind data)]
                                        [(byte-vector/from-integer kind)])))))
      (byte-vector/from-hex data))))

(defn encode
  "See the documentation of `decode` for a description of the arguments."
  [prefix data]
  (try (let [content (encode-content prefix data)
             checksum (create-checksum prefix
                                       (map bech32-char-to-int (encode-bech32 content)))]
         (str prefix
              "1"
              (encode-bech32 content)
              checksum))
       (catch Throwable e
         nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftest nip19
  (doseq [[entity prefix data]
          [["a12uel5l"
            "a"
            ""]
           ;; From NIP-19
           ["npub180cvv07tjdrrgpa0j7j7tmnyl2yr6yr7l8j4s3evf6u64th6gkwsyjh6w6"
            "npub"
            "3bf0c63fcb93463407af97a5e5ee64fa883d107ef9e558472c4eb9aaaefa459d"]
           ["npub10elfcs4fr0l0r8af98jlmgdh9c8tcxjvz9qkw038js35mp4dma8qzvjptg"
            "npub"
            "7e7e9c42a91bfef19fa929e5fda1b72e0ebc1a4c1141673e2794234d86addf4e"]
           ["nsec1vl029mgpspedva04g90vltkh6fvh240zqtv9k0t9af8935ke9laqsnlfe5"
            "nsec"
            "67dea2ed018072d675f5415ecfaed7d2597555e202d85b3d65ea4e58d2d92ffa"]
           ["nprofile1qqsrhuxx8l9ex335q7he0f09aej04zpazpl0ne2cgukyawd24mayt8gpp4mhxue69uhhytnc9e3k7mgpz4mhxue69uhkg6nzv9ejuumpv34kytnrdaksjlyr9p"
            "nprofile"
            {:special "3bf0c63fcb93463407af97a5e5ee64fa883d107ef9e558472c4eb9aaaefa459d"
             :relays '("wss://r.x.com" "wss://djbas.sadkb.com")}]
           ;; Borrowed from more-speech.
           ["npub1sg6plzptd64u62a878hep2kev88swjh3tw00gjsfl8f237lmu63q0uf63m"
            "npub"
            "82341f882b6eabcd2ba7f1ef90aad961cf074af15b9ef44a09f9d2a8fbfbe6a2"]
           ["npub19mun7qwdyjf7qs3456u8kyxncjn5u2n7klpu4utgy68k4aenzj6synjnft"
            "npub"
            "2ef93f01cd2493e04235a6b87b10d3c4a74e2a7eb7c3caf168268f6af73314b5"]
           ]]
    (let [[prefix_ data_] (decode entity)]
      (is (= prefix prefix_))
      (is (= data data_))
      (is (= (encode prefix_ data_) entity)))))

(run-tests 'nuestr.nip19)

  

