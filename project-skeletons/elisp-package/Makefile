CASK        ?= cask
EMACS       ?= emacs
DIST        ?= dist
EMACSFLAGS   = --batch -Q

VERSION     := $(shell EMACS=$(EMACS) $(CASK) version)
PKG_DIR     := $(shell EMACS=$(EMACS) $(CASK) package-directory)

EMACS_D      = ~/.emacs.d
USER_ELPA_D  = $(EMACS_D)/elpa

SRCS         = $(filter-out %-pkg.el, $(wildcard *.el))
TESTS        = $(filter-out %-pkg.el, $(wildcard test/*.el))
PACKAGE_TAR  = $(DIST)/__PROJECT-NAME__-$(VERSION).tar


.PHONY: all deps check install uninstall reinstall clean-all clean
all : deps $(PACKAGE_TAR)

deps :
	$(CASK) install

check : deps
	$(CASK) exec $(EMACS) $(EMACSFLAGS)  \
	$(patsubst %,-l % , $(SRCS))\
	$(patsubst %,-l % , $(TESTS))\
	-f ert-run-tests-batch-and-exit

install : $(PACKAGE_TAR)
	$(EMACS) $(EMACSFLAGS) -l package \
	-f package-initialize  --eval '(package-install-file "$(PACKAGE_TAR)")'

uninstall :
	rm -rf $(USER_ELPA_D)/__PROJECT-NAME__-*

reinstall : clean uninstall install

clean-all : clean
	rm -rf $(PKG_DIR)

clean :
	$(CASK) clean-elc
	rm -f *.elc
	rm -rf $(DIST)

$(PACKAGE_TAR) : $(SRCS)
	$(CASK) package $(DIST)
	tar -cvf $@ -C $(DIST) --exclude $(notdir $@) .
