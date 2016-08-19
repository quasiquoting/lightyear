IDRIS := idris
PKG   := lightyear

.PHONY: all clean clean-all install rebuild doc doc-clean test

all: build

build: src/Lightyear/*.idr
	@$(IDRIS) --build $(PKG).ipkg

clean:
	@$(IDRIS) --clean $(PKG).ipkg
	@find . -name '*.ibc' -delete

clean-all: clean doc-clean

install: build
	@$(IDRIS) --install $(PKG).ipkg

rebuild: clean build

docs: docs-clean build
	@$(IDRIS) --mkdoc $(PKG).ipkg \
	&& mv $(PKG)_doc docs

docs-clean:
	@rm -rf docs >/dev/null

test: install
	(cd tests; bash runtests.sh)
