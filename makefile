default: build

build:
	stack build

grammar-debug:
	stack exec happy Parser.y -- -iparser.log -o /dev/null

clean:
	stack clean
	rm -f parser.log

.PHONY: default build grammar-debug clean
