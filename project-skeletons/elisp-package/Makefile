CWD          = $(shell pwd)
DOC          = $(CWD)/doc
EMACS       ?= emacs
EMACSFLAGS   = --batch -Q
PYTHON       = python
CASK         = cask
CASK_URL     = https://raw.github.com/cask/cask/master/go
VERSION     := $(shell EMACS=$(EMACS) $(CASK) version)
PKG_DIR     := $(shell EMACS=$(EMACS) $(CASK) package-directory)
USER_INIT_EL = ~/.emacs.d/init.el

SRCS         = $(filter-out %-pkg.el, $(wildcard *.el))
OBJECTS      = $(SRCS:.el=.elc)
DOC_ORG      = $(DOC)/__PROJECT-NAME__.org
DOC_TEXI     = $(DOC)/__PROJECT-NAME__.texi
INFO_MANUAL  = $(DOC)/__PROJECT-NAME__.info
PACKAGE_SRCS = $(SRCS) __PROJECT-NAME__-pkg.el $(INFO_MANUAL)
PACKAGE_TAR  = __PROJECT-NAME__-$(VERSION).tar

.PHONY: all
all : env compile info dist

# Configure tooling and environment.
.PHONY: env
env : packages features

features : Cask
	$(CASK) exec ecukes new

# Byte-compile elisp files.
.PHONY: compile
compile : $(OBJECTS)

# Run ecukes tests.
.PHONY: check
check : compile
	$(CASK) exec ecukes

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
	--eval '(setq package-user-dir "$(PKG_DIR)")' -f package-initialize \
	-f batch-byte-compile $<
