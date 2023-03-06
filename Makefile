pom.xml:
	clj -Spom

.PHONY: sources
sources: pom.xml
	mvn dependency:sources

.PHONY: clean
clean:
	rm -rf target/
	rm -rf classes/

.PHONY: test
test:
	clj -X:test

.PHONY: run
run:
	clj -M -m nuestr.app

.PHONY: aot
aot:
	mkdir -p classes
	clj -M -e "(try (compile 'nuestr.app) (finally (javafx.application.Platform/exit)))"

.PHONY: uberjar
uberjar: aot
	clj -M:uberdeps

.PHONY: run-uberjar
run-uberjar:
	java -jar target/nuestr.jar
