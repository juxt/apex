# Copyright © 2019, JUXT LTD.

.DEFAULT_GOAL := test

.PHONY: test

test:
	clojure -Atest
