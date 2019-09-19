PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename `pwd`)

all: clean atts docs build install

test:
	R -e "devtools::test()"

docs:
	R -e "devtools::document()"

build:
	R -e "devtools::build()"

atts:
	R -e "Rcpp::compileAttributes()"

install:
	R -e "devtools::install()"

clean:
	cd ..;\
	$(RM) -r $(PKGNAME).Rcheck/
