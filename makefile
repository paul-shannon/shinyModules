default: roxy install

roxy:
	R -e "devtools::document()"

install:
	R CMD INSTALL .

