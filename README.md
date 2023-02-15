# Monstr: Nostr on the desktop (written in Clojure)


* work-in-progress
  - i.e., don't file bugs yet (or do if you like)
  - enjoy the ugly
  - wanna help?
* can't follow peeps yet so you'll have to create your identity and
  build your graph on another client
* db schema is subject to change rapidly
  - so MAY NEED TO do this whenever you pull new commits:
    - `$ rm ~/.nostr-desk` 

Pre-requisites:

```
$ brew install clojure/tools/clojure
```

May need to install some java/jdk, say v. 17, if you don't have one
You may also need to install rlwrap.

How to run:

```
$ make run
```

Alternatively, if you have Leiningen (https://leiningen.org) installed,
you can do:

```
$ lein run
```

### MacOs Darwin arm64 build

To build the secp256k1 native lib, which is not currently available in central repos,
recursive clone https://github.com/ACINQ/secp256k1-kmp and run

```
$ TARGET=darwin ./native/build.sh
$ TARGET=darwin ./jni/jvm/build.sh
```

This will create a dylib file which you can patch into a jar in the local maven repo, for example:

```
$ mkdir -p ./fr/acinq/secp256k1/jni/native/darwin-aarch64
$ cp ./jni/jvm/build/darwin/libsecp256k1-jni.dylib fr/acinq/secp256k1/jni/native/darwin-aarch64
$ jar uf ~/.m2/repository/fr/acinq/secp256k1/secp256k1-kmp-jni-jvm-darwin/0.7.1/secp256k1-kmp-jni-jvm-darwin-0.7.1.jar fr
```


### TODO
* seen-on relays w/ popup
  * popup queries db (w/ cache) for seen-on?
* manage followers ux
* design scheme for efficient load of <missing:xx> messages
  * ie messages from contacts you don't follow
* do not re-query everything when re-connecting to a relay
  * bonus: limit requery when contact lists change
* limit timelines by cardinality
  * with "load more" support
* configure/prune logs
  * capture logs in ui admin console
* more nips
* more pretty
  * themes?
* win/mac/linux installables
* keyboard ux
  * ctrl-enter publish/reply
  * esc cancel publish/reply (lose focus)
  * j/k navigate posts
* normy features
  * traditional-like 'signup' ux
  * what else


* metadata-cache a bit flawed
  * design more reactive approach
