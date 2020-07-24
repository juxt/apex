.PHONY: test

test:
	echo "apex/cms:<span foreground='#ff3'>TEST</span>" > /tmp/apex-cms-test-status
	clojure -Atest && echo "apex/cms:PASS" > /tmp/apex-cms-test-status || echo "<span foreground='red'>apex/cms:FAIL</span>" > /tmp/apex-cms-test-status

watch:
	find . -name "*.clj" | entr make

lint:
	clj-kondo --lint src/juxt --lint test/juxt
