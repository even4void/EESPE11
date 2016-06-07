all:
	latex 01-intro.tex
	biber 01-intro
	latex 01-intro.tex
	latex 01-intro.tex
	dvips 01-intro.dvi
	ps2pdf 01-intro.ps
