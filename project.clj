(defproject monstr "0.1.0-SNAPSHOT"
  :description "Monstr: a Clojure Desktop app for Nostr"
  :url "https://github.com/alemmens/monstr"
  
  :dependencies
  [[org.clojure/clojure "1.10.3"]
   [cljfx/cljfx "1.7.19"]
   [aleph/aleph "0.4.7-alpha10"]
   [aysylu/loom "1.0.2"]

   [fr.acinq.secp256k1/secp256k1-kmp "0.6.2"]
   [fr.acinq.secp256k1/secp256k1-kmp-jni-jvm "0.6.2"]

   [org.xerial/sqlite-jdbc "3.36.0.2"]
   [seancorfield/next.jdbc "1.2.659"]

   [metosin/jsonista "0.3.5"]

   [com.google.guava/guava "31.0.1-jre"]

   [org.clojure/tools.logging "1.2.4"]
   [ch.qos.logback/logback-classic "1.2.10"]

   [org.fxmisc.richtext/richtextfx "0.10.7"]

   [org.openjfx/javafx-controls "17.0.2"]
   [org.openjfx/javafx-base "17.0.2"]
   [org.openjfx/javafx-graphics "17.0.2"]
   [org.openjfx/javafx-media "17.0.2"]]

  :profiles {:dev {:dependencies [[speclj "3.4.1"]]}
             :uberjar {:aot :all}}
  :repositories {"local" {:url "file:lib" :username "" :password ""}}
  :plugins [[speclj "3.4.1"]]
  :main monstr.app
  :java-source-paths ["java"]
)

