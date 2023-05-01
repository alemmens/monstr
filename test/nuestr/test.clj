
(ns nuestr.test
  (:require
   [nuestr.byte-vector]
   [nuestr.nip19])
  (:use clojure.test))

(run-tests 'nuestr.byte-vector)
(run-tests 'nuestr.nip19)
(run-tests 'nuestr.domain)
