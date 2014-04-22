PYTHON_BIN = __PYTHON-BIN__
PIP        = ./env/bin/pip

all : env

# Configure virtualenv for this project.
env :
	virtualenv -p $(PYTHON_BIN) env

# Install editor tooling.
.PHONY: tooling
tooling : env
	$(PIP) install jedi
	$(PIP) install epc
	$(PIP) install pylint
	$(PIP) install pymacs
