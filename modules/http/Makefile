.PHONY: test

test:
	clojure -Atest && echo "apex/http:PASS" > /tmp/apex-http-test-status || echo "<span foreground='red'>apex/http:FAIL</span>" > /tmp/apex-http-test-status

watch:
	find . -name "*.clj" | entr make

lint:
	clj-kondo --lint src/juxt --lint test/juxt
