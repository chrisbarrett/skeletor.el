CWD          = $(shell pwd)
DIST         = $(CWD)/dist
DOC          = $(CWD)/doc
SKELETONS    = $(CWD)/project-skeletons
SCRIPT       = $(CWD)/script
CASK        ?= cask
EMACS       ?= emacs
EMACSFLAGS   = --batch -Q
VERSION     := $(shell EMACS=$(EMACS) $(CASK) version)
PKG_DIR     := $(shell EMACS=$(EMACS) $(CASK) package-directory)
USER_EMACS_D = ~/.emacs.d
USER_ELPA_D  = $(USER_EMACS_D)/elpa

SRCS         = $(filter-out %-pkg.el, $(wildcard *.el))
TESTS        = $(filter-out %-pkg.el, $(wildcard test/*.el))
DOC_ORG      = $(DOC)/__PROJECT-NAME__.org
DOC_TEXI     = $(DOC)/__PROJECT-NAME__.texi
INFO_MANUAL  = $(DOC)/__PROJECT-NAME__.info
PACKAGE_TAR  = $(DIST)/__PROJECT-NAME__-$(VERSION).tar

.PHONY: all
all : deps

.PHONY: deps
deps : $(PKG_DIR)
$(PKG_DIR) :
	$(CASK) install

.PHONY: check
check : deps
	$(CASK) exec $(EMACS) $(EMACSFLAGS)  \
	$(patsubst %,-l % , $(SRCS))\
	$(patsubst %,-l % , $(TESTS))\
	-f ert-run-tests-batch-and-exit

.PHONY: install
install : $(DIST) $(USER_ELPA_D)
	$(EMACS) $(EMACSFLAGS) -l package \
	-f package-initialize  --eval '(package-install-file "$(PACKAGE_TAR)")'

.PHONY: uninstall
uninstall :
	rm -rf $(USER_ELPA_D)/__PROJECT-NAME__-*

.PHONY: reinstall
reinstall : clean uninstall install

.PHONY: clean-all
clean-all : clean
	rm -rf $(PKG_DIR)

.PHONY: clean
clean :
	cask clean-elc
	rm -rf $(DIST)
	rm -f $(DOC_TEXI)
	rm -f $(INFO_MANUAL)

$(DIST) : $(INFO_MANUAL)
	$(CASK) package

$(INFO_MANUAL) : deps $(DOC_ORG)
	$(CASK) exec $(EMACS) $(EMACSFLAGS) \
	-l org -l ox-texinfo \
	--file=$(DOC_ORG) -f org-texinfo-export-to-info

$(USER_ELPA_D) :
	mkdir -p $(USER_ELPA_D)
