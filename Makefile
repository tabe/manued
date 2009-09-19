SCHEME = ypsilon --sitelib=.

.PHONY: check test

check: test

test:
	$(SCHEME) test.scm
