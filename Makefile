# Copyright © 2020, JUXT LTD.

.DEFAULT_GOAL := test

.PHONY: test

test:
	clojure -Atest
