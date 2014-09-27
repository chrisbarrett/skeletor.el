CASK       ?= cask
EMACS      ?= emacs
DIST       ?= dist
EMACSFLAGS  = --batch -Q
EMACSBATCH  = $(EMACS) $(EMACSFLAGS)

VERSION    := $(shell EMACS=$(EMACS) $(CASK) version)
PKG_DIR    := $(shell EMACS=$(EMACS) $(CASK) package-directory)
PROJ_ROOT  := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))

EMACS_D     = ~/.emacs.d
USER_ELPA_D = $(EMACS_D)/elpa

SRCS        = skeletor.el
TESTS       = $(wildcard test/*.el)
ORG_MANUAL  = doc/skeletor.org
TEXI_MANUAL = doc/skeletor.texi
TAR         = $(DIST)/skeletor-$(VERSION).tar


.PHONY: all check install uninstall reinstall clean-all clean
all : $(PKG_DIR) $(TAR)

check : $(PKG_DIR)
	$(CASK) exec $(EMACSBATCH) \
	$(patsubst %,-l % , $(SRCS) $(TESTS)) \
	-f ert-run-tests-batch-and-exit

install : $(TAR)
	$(EMACSBATCH) -l package -f package-initialize \
	--eval '(package-install-file "$(PROJ_ROOT)/$(TAR)")'

uninstall :
	rm -rf $(USER_ELPA_D)/skeletor-*

reinstall : clean uninstall install

clean-all : clean
	rm -rf $(PKG_DIR)

clean :
	rm -f *.elc
	rm -rf $(DIST)
	rm -f $(TEXI_MANUAL)
	rm -f *-pkg.el

$(PKG_DIR) : Cask
	$(CASK) install
	touch $(PKG_DIR)

$(TAR) : $(DIST) $(TEXI_MANUAL)
	$(CASK) package

$(DIST) :
	mkdir $(DIST)

$(TEXI_MANUAL) : $(PKG_DIR) $(ORG_MANUAL)
	$(CASK) exec $(EMACSBATCH) \
	-l org -l ox-texinfo \
	--file=$(ORG_MANUAL) -f org-texinfo-export-to-texinfo
