.PHONY: all test clean

SERVICE_NAME = guitar-theory-training
VERSION = 0.0.1


release-frontend:
	shadow-cljs release frontend

compile-frontend:
	shadow-cljs compile frontend

clean:
	rm -rf ./node_modules ./.cpcache ./.shadow-cljs ./target ./.lsp ./classes

update:
	clojure -Moutdated --write
	npm update

install:
	npm install

format:
	clojure-lsp format
	clojure-lsp clean-ns

diagnostics:
	clojure-lsp diagnostics
