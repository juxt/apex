# Copyright © 2020, JUXT LTD.

.DEFAULT_GOAL := test

.PHONY: test watch

test:
	make -C modules/http test

watch:
	make -C modules/http watch
