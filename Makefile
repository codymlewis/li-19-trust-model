PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename `pwd`)

all: clean atts docs build install

new-test:
	R -e "usethis::use_test('$(name)')"

test:
	R -e "devtools::test()"

deps:
	R -e "if (!require(devtools)) { \
	r = getOption('repos'); \
	r['CRAN'] = 'https://cran.csiro.au/'; \
	options(repos = r); \
	rm(r); \
	install.packages('devtools'); \
	}"

ci-test: deps all test

docs:
	R -e "devtools::document()"

build:
	R -e "devtools::build()"

atts:
	R -e "Rcpp::compileAttributes()"

ref:
	R CMD Rd2pdf .

install:
	R -e "devtools::install()"

clean:
	$(RM) -r images;\
	cd ..;\
	$(RM) -r $(PKGNAME).Rcheck/
