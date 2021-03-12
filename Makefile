# vi: fdm=marker

# Global variables {{{1
################################################################

# Mute R 3.6 "Registered S3 method overwritten" warning messages.
# Messages that were output:
#     Registered S3 method overwritten by 'R.oo':
#       method        from
#       throw.default R.methodsS3
#     Registered S3 method overwritten by 'openssl':
#       method      from
#       print.bytes Rcpp
export _R_S3_METHOD_REGISTRATION_NOTE_OVERWRITES_=no

# Set cache folder
ifndef BIODB_CACHE_DIRECTORY
export BIODB_CACHE_DIRECTORY=$(CURDIR)/cache
endif

# Set testthat reporter
ifndef TESTTHAT_REPORTER
ifdef VIM
TESTTHAT_REPORTER=summary
else
TESTTHAT_REPORTER=progress
endif
endif

PKG_VERSION=$(shell grep '^Version:' DESCRIPTION | sed 's/^Version: //')
GIT_VERSION=$(shell git describe --tags | sed 's/^v\([0-9.]*\)[a-z]*.*$$/\1/')
ZIPPED_PKG=biodbChebi_$(PKG_VERSION).tar.gz

# Display values of main variables
$(info "BIODB_CACHE_DIRECTORY=$(BIODB_CACHE_DIRECTORY)")
$(info "BIODB_CACHE_READ_ONLY=$(BIODB_CACHE_READ_ONLY)")
$(info "PKG_VERSION=$(PKG_VERSION)")

RFLAGS=--slave --no-restore

# Set test file filter
ifndef TEST_FILE
TEST_FILE=NULL
else
TEST_FILE:='$(TEST_FILE)'
endif

# Default target {{{1
################################################################

all:

# Check and test {{{1
################################################################

check: clean.vignettes $(ZIPPED_PKG)
	R $(RFLAGS) -e 'BiocCheck::BiocCheck("$(ZIPPED_PKG)", `new-package`=TRUE, `quit-with-status`=TRUE, `no-check-formatting`=TRUE)'

test:
ifdef VIM
	R $(RFLAGS) -e "devtools::test('$(CURDIR)', filter=$(TEST_FILE), reporter=c('$(TESTTHAT_REPORTER)', 'fail'))" | sed 's!\([^/A-Za-z_-]\)\(test[^/A-Za-z][^/]\+\.R\)!\1tests/testthat/\2!'
else
	R $(RFLAGS) -e "devtools::test('$(CURDIR)', filter=$(TEST_FILE), reporter=c('$(TESTTHAT_REPORTER)', 'fail'))"
endif

win:
	R $(RFLAGS) -e "devtools::check_win_devel('$(CURDIR)')"

# Build {{{1
################################################################

build: $(ZIPPED_PKG)

$(ZIPPED_PKG): doc
	R CMD build .

# Documentation {{{1
################################################################

doc:
	R $(RFLAGS) -e "devtools::document('$(CURDIR)')"

vignettes: clean.vignettes
	@echo Build vignettes for already installed package, not from local soures.
	time R $(RFLAGS) -e "devtools::build_vignettes('$(CURDIR)')"


# Install {{{1
################################################################

install.deps:
	R $(RFLAGS) -e "devtools::install_dev_deps('$(CURDIR)')"

install: uninstall install.local list.classes

install.local:
	R $(RFLAGS) -e "devtools::install_local('$(CURDIR)', dependencies = TRUE)"

list.classes:
	R $(RFLAGS) -e 'library(biodbChebi) ; cat("Exported methods and classes:", paste(" ", ls("package:biodbChebi"), collapse = "\n", sep = ""), sep = "\n")'

uninstall:
	R $(RFLAGS) -e "try(devtools::uninstall('$(CURDIR)'), silent = TRUE)"

# Clean {{{1
################################################################

clean: clean.build clean.vignettes
	$(RM) src/*.o src/*.so src/*.dll
	$(RM) -r tests/test.log tests/output tests/test\ *.log
	$(RM) -r biodbChebi.Rcheck
	$(RM) -r Meta

clean.vignettes:
	$(RM) -r doc

clean.build:
	$(RM) biodbChebi_*.tar.gz

clean.cache:
	$(RM) -r $(BIODB_CACHE_DIRECTORY)

# Phony targets {{{1
################################################################

.PHONY: all clean win test build check vignettes install uninstall devtools.check devtools.build clean.build clean.cache doc check.version
