CWD          = $(shell pwd)
DOC          = $(CWD)/doc
SCRIPT       = $(CWD)/script
GIT_DIR      = $(CWD)/.git
EMACS       ?= emacs
EMACSFLAGS   = --batch -Q
CASK         = cask
VERSION     := $(shell EMACS=$(EMACS) $(CASK) version)
PKG_DIR     := $(shell EMACS=$(EMACS) $(CASK) package-directory)
USER_EMACS_D = ~/.emacs.d
USER_INIT_EL = $(USER_EMACS_D)/init.el
USER_ELPA_D  = $(USER_EMACS_D)/elpa

SRCS         = $(filter-out %-pkg.el, $(wildcard *.el))
TESTS        = $(filter-out %-pkg.el, $(wildcard test/*.el))
OBJECTS      = $(SRCS:.el=.elc)
DOC_ORG      = $(DOC)/__PROJECT-NAME__.org
DOC_TEXI     = $(DOC)/__PROJECT-NAME__.texi
INFO_MANUAL  = $(DOC)/__PROJECT-NAME__.info
PACKAGE_SRCS = $(SRCS) __PROJECT-NAME__-pkg.el $(INFO_MANUAL)
PACKAGE_TAR  = __PROJECT-NAME__-$(VERSION).tar

PRECOMMIT_SRC  = $(SCRIPT)/pre-commit.sh
PRECOMMIT_HOOK = $(GIT_DIR)/hooks/pre-commit

.PHONY: all
all : env compile info dist

# Configure tooling and environment.
.PHONY: env
env : packages $(PRECOMMIT_HOOK)

# Run tests before committing.
$(PRECOMMIT_HOOK) :
	ln -s $(PRECOMMIT_SRC) $(PRECOMMIT_HOOK)
	chmod +x $(PRECOMMIT_HOOK)

# Byte-compile elisp files.
.PHONY: compile
compile : $(OBJECTS)

# Run ert tests.
.PHONY: check
check : compile
	$(CASK) exec $(EMACS) $(EMACSFLAGS)  \
	$(patsubst %,-l % , $(SRCS))\
	$(patsubst %,-l % , $(TESTS))\
	-f ert-run-tests-batch-and-exit

# Export the org documentation to an info manual.
.PHONY: info
info : $(INFO_MANUAL)
$(INFO_MANUAL) : $(DOC_ORG)
	$(CASK) exec $(EMACS) $(EMACSFLAGS) \
	-l org -l ox-texinfo \
	--file=$(DOC_ORG) -f org-texinfo-export-to-info

# Install packages with Cask.
$(PKG_DIR) : Cask
	$(CASK)
	$(CASK) install
	touch $(PKG_DIR)

# Create a tar that can be installed by package.el
.PHONY: dist
dist : $(PACKAGE_TAR)
$(PACKAGE_TAR) : $(PACKAGE_SRCS)
	rm -rf __PROJECT-NAME__-$(VERSION)
	mkdir -p __PROJECT-NAME__-$(VERSION)
	cp -f $(PACKAGE_SRCS) __PROJECT-NAME__-$(VERSION)
	tar cf $(PACKAGE_TAR) __PROJECT-NAME__-$(VERSION)
	rm -rf __PROJECT-NAME__-$(VERSION)

# Install elisp packages with cask.
.PHONY: packages
packages : $(PKG_DIR)

# Install the package to the user's Emacs dir.
.PHONY: install
install : dist
	$(EMACS) $(EMACSFLAGS) -l package \
	-f package-initialize  --eval '(package-install-file "$(CWD)/$(PACKAGE_TAR)")'

# Uninstall the package.
.PHONY: uninstall
uninstall :
	rm -rf $(USER_ELPA_D)/__PROJECT-NAME__-*

# Reinstall the package.
.PHONY: reinstall
reinstall : clean uninstall reinstall

# Restore to pristine state.
.PHONY: clean-all
clean-all : clean clean-pkgdir

# Clean generated files.
.PHONY: clean
clean : $(clean-doc)
	rm -f $(OBJECTS)
	rm -rf __PROJECT-NAME__-*.tar __PROJECT-NAME__-pkg.el
	rm -f $(DOC_TEXI)
	rm -f $(INFO_MANUAL)

# Remove packages installed by Cask.
.PHONY: clean-pkgdir
clean-pkgdir :
	rm -rf $(PKG_DIR)

# Generate files.

__PROJECT-NAME__-pkg.el : Cask
	$(CASK) package

%.elc : %.el $(PKG_DIR)
	$(CASK) exec $(EMACS) $(EMACSFLAGS) \
	--eval '(setq package-user-dir "$(PKG_DIR)")' -f package-initialize -L .\
	-f batch-byte-compile $<
