.PHONY: build
build: run-template
	cabal v2-build .

.PHONY: run-template
run-template: src/Data/Tuple/Homotuple.hs

src/Data/Tuple/Homotuple.hs: template/Homotuple.hs template/HomotupleItem.hs .bin/homotuple-template
	.bin/homotuple-template .

.bin/homotuple-template: dev/homotuple-dev.cabal dev/homotuple-template.hs
	@mkdir -p $(@D)
	cabal v2-install --installdir .bin --overwrite-policy=always homotuple-dev:exe:homotuple-template

.PHONY: clean
clean:
	-cabal v2-clean
	-rm -rf .bin
	-rm -rf src/Data/Tuple/Homotuple.hs
