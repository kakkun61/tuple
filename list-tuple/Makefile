.PHONY: build
build: run-template
	cabal v2-build .

.PHONY: run-template
run-template: src/Data/Tuple/List.hs

src/Data/Tuple/List.hs: template/List.hs template/ListAt.hs template/ListItem.hs .bin/list-tuple-template
	.bin/list-tuple-template .

.bin/list-tuple-template: dev/list-tuple-dev.cabal dev/list-tuple-template.hs
	@mkdir -p $(@D)
	cabal v2-install --installdir .bin --overwrite-policy=always list-tuple-dev:exe:list-tuple-template

.PHONY: clean
clean:
	-cabal v2-clean
	-rm -rf .bin
	-rm -rf src/Data/Tuple/List.hs
