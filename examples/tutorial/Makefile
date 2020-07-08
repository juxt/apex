.PHONY: test

test:
	echo "apex/tut:<span foreground='#ff3'>TEST</span>" > /tmp/apex-tut-test-status
	clojure -Atest && echo "apex/tut:PASS" > /tmp/apex-tut-test-status || echo "<span foreground='red'>apex/tut:FAIL</span>" > /tmp/apex-tut-test-status

watch:
	find . -name "*.clj" | entr make

lint:
	clj-kondo --lint src/juxt --lint test/juxt
