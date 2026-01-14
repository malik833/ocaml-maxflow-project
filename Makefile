.PHONY: all build format edit demo clean

src?=0
dst?=2
graph?=graph3.txt

all: build

build:
	@echo "\n   🚨  COMPILING  🚨 \n"
	dune build src/ftest.exe
	ls src/*.exe > /dev/null && ln -fs src/*.exe .

format:
	ocp-indent --inplace src/*

edit:
	code . -n

demo: build
	@echo "\n   ⚡  EXECUTING  ⚡\n"
	./ftest.exe graphs/${graph} $(src) $(dst) graphs/graph.dot
	dot -Tsvg graphs/graph.dot > new.svg

clean:
	find -L . -name "*~" -delete
	rm -f *.exe
	dune clean

exportDot: 
	@echo "\n   📦  EXPORTING DOT  📦 \n"
	./ftest.exe graphs/${graph} $(src) $(dst) graph.dot
	dot -Tsvg exportDot graph > graph.svg
