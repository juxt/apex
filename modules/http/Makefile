.PHONY: test

test:
	clojure -Atest

watch:
	find . -name "*.clj" | entr make

lint:
	clj-kondo --lint src/juxt --lint test/juxt
