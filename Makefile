bin  := sj
proj := s-java
path := $(shell stack path --local-install-root)/bin

.PHONY: build
build:
	@echo $(path)
	stack build
	cp -f $(path)/$(bin) app
	/usr/bin/strip app/$(bin)

.PHONY: clean
clean:
	rm -f app/$(bin)

.PHONY: doctest
doctest:
	@stack test $(proj):doc

.PHONY: test
test:
	@stack test $(proj):it

.PHONY: repl
repl:
	@stack ghci $(proj):lib --flag $(proj):-optimize
