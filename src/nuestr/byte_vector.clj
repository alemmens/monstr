(ns nuestr.byte-vector
  (:gen-class)
  (:require
   [clojure.math.numeric-tower :as math])
  (:use clojure.test))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Byte vectors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For the moment we just use the convention that a byte-vector is a list of 8-bit
;; bytes (i.e.  unsigned integers smaller than 256). Sometimes the bytes may have another
;; width; in those cases the byte width will be specified as an argument to the functions
;; that process them.

(defn make [& bytes]
  bytes)

(defn size
  "Returns the number of bytes in a byte vector."
  [byte-vector]
  (count byte-vector))

;;
;; Converting between our byte vectors and Clojure's/Java's byte arrays
;;

(defn to-byte-array
  [byte-vector]
  (byte-array byte-vector))

(defn from-byte-array
  [byte-array]
  (map #(if (< % 0) (+ 256 %) %)
       byte-array))

;;
;; To/from integer
;;

(defn to-integer
  "Returns the non-negative integer (probably a bigint) corresponding to a byte vector.
  `base` defaults to 256.  `endianness` can be either :big-endian (i.e. most significant
  byte first, the default) or :little-endian."
  ([bytes base endianness]
   (case endianness
     :little-endian (reduce +
                         (map #(* (math/expt base %2) %1)
                              bytes
                              (iterate inc 0)))
     :big-endian (to-integer (reverse bytes) base :little-endian)))
  ([bytes base] (to-integer bytes (bigint base) :big-endian))
  ([bytes] (to-integer bytes 256M)))


(deftest test-to-integer
  (is (= (to-integer '(1 0 0 0)) 16777216))
  (is (= (to-integer '(1 1)) 257))
  (is (= (to-integer '(255 1)) 65281)))


(defn from-integer
  "Converts a non-negative integer to a byte vector. Uses big-endian notation, i.e.
  most significant byte first."
  ([n nr-bytes base]
   (loop [n n
          nr-bytes nr-bytes
          result ()]
     (if (= 0 nr-bytes)
       result
       (let [quotient (quot n base)
             remainder (rem n base)]
         (recur quotient (dec nr-bytes)
                (cons (int remainder) result))))))
  ([n nr-bytes]
   (from-integer n nr-bytes 256))
  ([n]
   (from-integer n
                 (let [bit-count (math/integer-length n)
                       byte-count (if (= 0 (rem bit-count 8))
                                    (quot bit-count 8)
                                    (inc (quot bit-count 8)))]
                   byte-count))))

(deftest test-from-integer
  (is (= (from-integer 257) '(1 1)))
  (is (= (from-integer 258 3) '(0 1 2)))
  (is (= (from-integer 65537 4) '(0 1 0 1)))
  (is (= (from-integer 65535 2) '(255 255))))

;;
;; Group in bits
;;

(defn split-bits
  "Splits a normal byte vector (where each element has a width of 8 bits) into a byte vector
  of the specified new size where each element has the specified new byte width."
  ([bytes new-byte-width new-size existing-byte-width]
   (let [divisor (bit-shift-left 1 new-byte-width)]
     (loop [n (to-integer bytes (bit-shift-left 1 existing-byte-width))
            result ()
            bytes-remaining new-size]
       (if (= 0 bytes-remaining)
         result
         (recur (quot n divisor)
                (cons (rem n divisor) result)
                (dec bytes-remaining))))))
  ([bytes new-byte-width new-size]
   (split-bits bytes new-byte-width new-size 8)))

(defn to-bits
  "Takes a byte vector of the specified byte width and returns the corresponding
  bit vector."
  ([bytes new-size existing-byte-width]
   (split-bits bytes 1 new-size existing-byte-width))
  ([bytes new-size]
   (split-bits bytes 1 new-size 8))
  ([bytes]
   (split-bits bytes 1 (* (count bytes) 8) 8)))

(defn join-bits
  "Takes a byte vector where each byte has the specified byte width and returns
  the corresponding byte vector where each byte has a width of 8 bits."
  ([bytes byte-width new-size]
   (let [n (to-integer bytes (bit-shift-left 1 byte-width))]
     (from-integer n new-size)))
  ([bytes byte-width]
   (join-bits bytes byte-width (int (Math/ceil (* (/ byte-width 8) (count bytes)))))))
         
(defn from-bits
  "Takes a bit vector (i.e. a 1-bit byte vector) and returns a byte vector where
  each element has the specified new byte width (default: 8)."
  [bits new-byte-width]
  (let [pad (repeat new-byte-width 0)
        groups (partition new-byte-width new-byte-width pad bits)]
    (map #(int (to-integer % 2)) groups)))

      
(deftest bits
  (is (= '(0 1 2 3) (split-bits (make 1 2 3) 8 4)))
  ;; 257 = (1 1) in 8-bit bytes = (1) in 11-bit bites
  (is (= '(257) (split-bits (make 1 1) 11 1)))
  ;; 19 = (1 0 0 1 1) in binary
  (is (= '(1 0 0 1 1) (split-bits (make 19) 1 5)))
  ;; 2100 = (1 52) in 11-bit bytes
  (is (= '(1 52) (split-bits (from-integer 2100) 11 2)))
  ;; 2100 = (1 52) in 11-bit bytes = (8 52) in 8-bit bytes
  (is (= '(0 8 52) (join-bits (split-bits (from-integer 2100) 11 2)
                              11)))
  (is (= '(0 0 8 52) (join-bits (split-bits (from-integer 2100) 11 2)
                                11
                                4)))
  (is (= '(15 8) (from-bits '(1 1 1 1 1) 4))))

;;
;; Append/slice/last-byte
;;

(defn append [& args]
  "Returns the concatentation of its args."
  (apply concat args))

(deftest test-append
  (is (= (append '(0 1) '(2 3) '(4 5))
         '(0 1 2 3 4 5))))

(defn slice
  "Returns a subsequence of a byte vector."
  ([byte-vector start end]
   (let [length (- end start)]
     (take length (drop start byte-vector))))
  ([byte-vector start]
   (slice byte-vector start (inc (count byte-vector)))))
  

(deftest test-slice
  (is (= (slice '(1 2 3 4) 1 3) '(2 3)))
  (is (= (slice '(1 2 3 4) 3) '(4))))

(defn first-byte [byte-vector]
  (first byte-vector))

(defn split [byte-vector position]
  [(slice byte-vector 0 position)
   (slice byte-vector position)])

;;
;; To/from hex
;;

(defn- byte-to-string [byte]
  (format "%02x" byte))

(defn to-hex
  "Returns a string with the hexadecimal representation of a byte vector. Assumes
little-endian notation."
  [bytes]
  (apply str (map byte-to-string bytes)))

(defn from-hex
  "Returns the byte vector corresponding to the given string with a hexadecimal representation.
Assumes little-endian notation."
  [string]
  (assert (= (mod (count string) 2) 0)
          (str "String with hexadecimal byte vector notation should have a length that's divisible by 2.
String: '"
               string
               "'"))
  (map (fn [[a b]] (Integer/parseInt (str a b) 16))
       (partition 2 string)))

(deftest hex
  (is (= (to-hex (from-hex "DEADCAFE"))
         "deadcafe"))
  (is (= (to-hex (from-hex "AF0B07DE"))
         "af0b07de")))

;;
;; To/from ASCII.
;;

(defn to-ascii [bytes]
  (apply str (map char bytes)))

(defn from-ascii [string]
  (map int string))

(deftest ascii
  (is (= (to-ascii (from-hex "414243616263"))
         "ABCabc"))
  (is (= (from-ascii "ABCabc")
         '(0x41 0x42 0x43 0x61 0x62 0x63))))

;;
;; To/from binary
;;

(defn to-binary
  "Takes a bit vector and returns the corresponding string in binary notation."
  [bits]
  (assert (every? #(<= 0 % 1) bits))
  (clojure.string/join (map #(if (= 0 %) "0" "1") bits)))


(run-tests 'nuestr.byte-vector)
