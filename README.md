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

How to run:

```
$ make run
```

Alternatively, if you have Leiningen (https://leiningen.org) installed,
you can do:

```
$ lein run
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
