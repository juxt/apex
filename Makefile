# Copyright © 2020, JUXT LTD.

.DEFAULT_GOAL := test

.PHONY: test watch

test:
	make -C modules/http test
	make -C examples/tutorial test
	make -C examples/cms test

watch:
	find . -name "*.clj" | entr make
