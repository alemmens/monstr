# Nuestr: Nostr on the desktop (written in Clojure)

## Some notes

* It's still early days for Nuestr. So expect bugs (and please report them if you bump
  into any).
  
* At the moment Nuestr doesn't have the option to follow other accounts in the standard
  Nostr way (but note the option in the Views tab to follow authors for a specific
  view). So it's probably best to create your identity (public key and/or secret key) and
  build your contact list (follow list) on another client for now.
  
* The database schema is subject to change. So, as long as Nuestr doesn't do database
  migrations, you may need to delete the database after upgrading to a new release.
  There are two ways to do this:
  
    - `$ rm ~/.nostr-desk` will delete the SQLite database and also user settings like the
    views, visible columns, active account, etcetera. This means you will start completely
    from scratch.
    
    - `$ rm ~/.nostr-desk/nd.db` will only delete the SQLite database. If you're lucky,
    this will keep your user settings intact. If not, you'll have to delete everything
    (as described above).
   
  I will add a release note whenever there's a change that breaks the database schema.
  Since I started working on Nuestr (February 2023) there have been no such changes yet.
   
* There's a Telegram group for Nuestr at https://t.me/nuestr_clj

* You can follow the main developer (aasaa) on Nostr: 
  npub1aasaanckwe7489s3g7x000yfz8lndzru2cgdszv583tevgznaaysm2tjjh


## Prerequisites

* You need to have Clojure installed. See https://ericnormand.me/guide/how-to-install-clojure
  or https://clojure.org/guides/install_clojure

* A common way of installing Clojure is:

```
$ brew install clojure/tools/clojure
```

* You will need to have some java/jdk, say v. 17, installed if you don't have one.

* You may also need to install rlwrap.

## How to run

```
$ make run
```

Alternatively, if you have Leiningen (https://leiningen.org) installed,
you can do:

```
$ lein run
```

## MacOs Darwin arm64 build

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


## Original TODO notes by Aaron

(I mostly use Github issues for this now.)

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
