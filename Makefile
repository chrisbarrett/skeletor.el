CASK        ?= cask
EMACS       ?= emacs
DIST        ?= dist
EMACSFLAGS   = --batch -Q

VERSION     := $(shell EMACS=$(EMACS) $(CASK) version)
PKG_DEPS    := $(shell EMACS=$(EMACS) $(CASK) package-directory)

EMACS_D      = ~/.emacs.d
USER_ELPA_D  = $(EMACS_D)/elpa

TESTS        = $(filter-out %-pkg.el, $(wildcard test/*.el))
DOC          = doc
ORG_MANUAL   = $(DOC)/skeletor.org
INFO_MANUAL  = $(DOC)/skeletor.info

DIST_SRCS      = $(DIST)/skeletor.el
DIST_PKG       = $(DIST)/skeletor-pkg.el
DIST_README    = $(DIST)/skeletor-readme.txt
DIST_TAR       = $(DIST)/skeletor-$(VERSION).tar
DIST_SKELETONS = $(DIST)/project-skeletons
DIST_MANUAL    = $(DIST)/skeletor.info


.PHONY: all check install uninstall reinstall clean-all clean
all : $(PKG_DEPS) $(DIST_TAR)

$(PKG_DEPS) :
	$(CASK) install

check : $(PKG_DEPS)
	$(CASK) exec $(EMACS) $(EMACSFLAGS)  \
	$(patsubst %,-l % , $(filter-out %-pkg.el, $(DIST_SRCS)))\
	$(patsubst %,-l % , $(TESTS))\
	-f ert-run-tests-batch-and-exit

install : $(DIST_TAR)
	$(EMACS) $(EMACSFLAGS) -l package \
	-f package-initialize  --eval '(package-install-file "$(DIST_TAR)")'

uninstall :
	rm -rf $(USER_ELPA_D)/skeletor-*

reinstall : clean uninstall install

clean-all : clean
	rm -rf $(PKG_DEPS)

clean :
	$(CASK) clean-elc
	rm -f *.elc
	rm -rf $(DIST)
	rm -f $(INFO_MANUAL)
	rm -f *-pkg.el

$(DIST_TAR) : $(DIST_README) $(DIST_PKG) $(DIST_SRCS) $(DIST_MANUAL) $(DIST_SKELETONS)
	tar -cvf $@ -C $(DIST) --exclude $(@F) .

$(DIST_README) :
	$(CASK) package $(DIST)

$(DIST_SRCS) : $(DIST)
	cp -f $(@F) $@

$(DIST_PKG) : $(DIST)
	cask pkg-file
	cp -f $(@F) $@

$(DIST) :
	mkdir $(DIST)

$(DIST_SKELETONS) :
	cp -rf $(@F) $@

$(DIST_MANUAL) : $(INFO_MANUAL)
	cp -f $(DOC)/$(@F) $@

$(INFO_MANUAL) : $(PKG_DEPS) $(ORG_MANUAL)
	$(CASK) exec $(EMACS) $(EMACSFLAGS) \
	-l org -l ox-texinfo \
	--file=$(ORG_MANUAL) -f org-texinfo-export-to-info
