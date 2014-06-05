CWD          = $(shell pwd)
DIST         = $(CWD)/dist
DOC          = $(CWD)/doc
SKELETONS    = $(CWD)/project-skeletons
SCRIPT       = $(CWD)/script
GIT_DIR      = $(CWD)/.git
CASK        ?= cask
EMACS       ?= emacs
EMACSFLAGS   = --batch -Q
VERSION     := $(shell EMACS=$(EMACS) $(CASK) version)
PKG_DIR     := $(shell EMACS=$(EMACS) $(CASK) package-directory)
USER_EMACS_D = ~/.emacs.d
USER_ELPA_D  = $(USER_EMACS_D)/elpa

SRCS         = $(filter-out %-pkg.el, $(wildcard *.el))
TESTS        = $(filter-out %-pkg.el, $(wildcard test/*.el))
PACKAGE_TAR  = $(DIST)/__PROJECT-NAME__-$(VERSION).tar

PRECOMMIT_SRC  = $(SCRIPT)/pre-commit.sh
PRECOMMIT_HOOK = $(GIT_DIR)/hooks/pre-commit

.PHONY: all
all : deps

.PHONY: deps
deps : $(PKG_DIR)
$(PKG_DIR) :
	$(CASK) install

# Add precommit hook to run tests before committing.
.PHONY: hook
hook : $(PRECOMMIT_HOOK)
$(PRECOMMIT_HOOK) :
	ln -s $(PRECOMMIT_SRC) $(PRECOMMIT_HOOK)
	chmod +x $(PRECOMMIT_HOOK)

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

$(DIST) :
	$(CASK) package

$(USER_ELPA_D) :
	mkdir -p $(USER_ELPA_D)
