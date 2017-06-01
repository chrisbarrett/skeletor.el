PYTHON_BIN = __PYTHON-BIN__
VIRTUALENV = ~/.virtualenvs/__PROJECT-NAME__

all : $(VIRTUALENV)

# Configure virtualenv for this project.
$(VIRTUALENV) :
	virtualenv -p $(PYTHON_BIN) $(VIRTUALENV)
	source $(VIRTUALENV)/bin/activate

# Install editor tooling.
.PHONY: tooling
tooling : $(VIRTUALENV)
	pip install jedi
	pip install epc
	pip install pylint
	pip install pymacs
