all: zea_lip_base.pdf
zea_lip_base.pdf: zea_lip_base.md bibliography.bib
	pandoc -V geometry:margin=1in -H format.sty --latex-engine=xelatex --bibliography bibliography.bib --csl=style.csl zea_lip_base.md -o zea_lip_base.pdf
