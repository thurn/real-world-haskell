# Makefile for Read World Haskell literate PDF output

pdf:
	lhs2TeX RealWorldHaskell.lhs -o RealWorldHaskell.tex
	pdflatex RealWorldHaskell.tex
	rm RealWorldHaskell.aux
	rm RealWorldHaskell.log
	rm RealWorldHaskell.ptb
	rm RealWorldHaskell.tex
	open RealWorldHaskell.pdf

test: build
	cabal test

build:
	cabal build

