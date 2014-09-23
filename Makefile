GIT_VERSION=$(shell git describe --tags --dirty --always)
DIST_NAME=gabor-melis#-$(GIT_VERSION)
DIST_FILE=dist/$(DIST_NAME).zip
BIN=rumcajsz

.PHONY: $(BIN)
$(BIN):
	./higgsml-build

# Check out the versions of library dependencies with which the
# software was tested.
#
# This won't work if you have quicklisp loaded already. The default
# ./configure options include --no-userinit, --no-sysinit which is
# usually enough to prevent it from being loaded.
quicklisp:
	curl -O http://beta.quicklisp.org/quicklisp.lisp
	build/run-lisp.sh --load quicklisp.lisp \
		--eval '(quicklisp-quickstart:install :path "./quicklisp/")' \
		--eval '(exit)'
	rm -f quicklisp.lisp
	cd quicklisp/local-projects/ && ../../build/fetch-local-projects.sh
	build/run-lisp.sh --load quicklisp/setup.lisp \
		--load build/install-dependencies.lisp \
		--eval '(exit)'

.PHONY: clean
clean:
	find . -name '*~' -o -name '*.fasl' | xargs rm -f

.PHONY: distclean
distclean: clean
	rm -rf dist/

.PHONY: archclean
archclean: distclean
	rm -rf SETTINGS $(BIN)

.PHONY: dist
dist:	$(BIN)
	-mkdir dist
	git archive --format=zip --prefix "$(DIST_NAME)/" HEAD > "$(DIST_FILE)"
	# Can't find how to filter files or make zip add a prefix. Do
	# it all by hand.
	mkdir "$(DIST_NAME)/"
	cp -a quicklisp/ "$(BIN)" "$(DIST_NAME)/"
	# "Filter out" .git dirs by removing them from the copy.
	find "$(DIST_NAME)/quicklisp/" -name .git -type d | xargs rm -rf
	zip -r "$(DIST_FILE)" "$(DIST_NAME)/"
	rm -rf "$(DIST_NAME)"
