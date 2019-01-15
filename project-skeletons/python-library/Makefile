PYTHON_BIN = __PYTHON-BIN__
VIRTUALENV = __VENV-DIR__/__PROJECT-NAME__

all : $(VIRTUALENV)

# Configure virtualenv for this project.
$(VIRTUALENV) :
	__VENV-BIN__ -p $(PYTHON_BIN) $(VIRTUALENV)
	source $(VIRTUALENV)/bin/activate

# Install editor tooling.
.PHONY: tooling
tooling : $(VIRTUALENV)
	__PIP-BIN__ install --user jedi
	__PIP-BIN__ install --user epc
	__PIP-BIN__ install --user flake8
	__PIP-BIN__ install --user black
	__PIP-BIN__ install --user autopep8
	__PIP-BIN__ install --user yapf
