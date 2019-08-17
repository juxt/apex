.DEFAULT_GOAL := test

.PHONY: test

test:
	clojure -Atest
