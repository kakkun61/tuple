GHC ?= ghc

.PHONY: build
build: run-template build-only-dependencies
	cabal v2-build all --with-compiler=$(GHC)

.PHONY: build-only-dependencies
build-only-dependencies:
	cabal v2-build all --with-compiler=$(GHC) --only-dependencies

.PHONY: run-template
run-template: homotuple.run-template list-tuple.run-template

.PHONY: homotuple.run-template
homotuple.run-template:
	${MAKE} -C homotuple run-template

.PHONY: list-tuple.run-template
list-tuple.run-template:
	${MAKE} -C list-tuple run-template

.PHONY: test
test: build
	cabal v2-test all --with-compiler=$(GHC)
