.PHONY: build
build:
	npx js-yaml syntaxes/simplescript.tmLanguage.yaml > syntaxes/simplescript.tmLanguage.json
	vsce package

.PHONY: install
install: build
	code --install-extension simplescript-syntax-highlights-0.0.1.vsix
