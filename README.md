# skeletor.el

[![Build Status](https://travis-ci.org/chrisbarrett/skeletor.el.png?branch=master)](https://travis-ci.org/chrisbarrett/skeletor.el)

Skeletor provides project templates for Emacs. It also automates the mundane
parts of setting up a new project like version control, licenses and tooling.

Skeletor comes with a number of predefined predefined templates and allows you
to easily create your own.

![Skeletor Laughing](assets/skeletor.jpg)

## Table of Contents

<div id="text-table-of-contents">
<ul>
<li><a href="#supported-project-types">1. Supported Project Types</a></li>
<li><a href="#installation">2. Installation</a></li>
<li><a href="#creating-projects">3. Creating Projects</a></li>
<li><a href="#defining-project-templates">4. Defining Project Templates</a>
<ul>
<li><a href="#project-skeletons">4.1. Project Skeletons</a></li>
<li><a href="#external-tools">4.2. External Tools</a></li>
</ul>
</li>
<li><a href="#variables-and-expansions-in-templates">5. Variables and Expansions in Templates</a>
<ul>
<li><a href="#substitutions">5.1. Substitutions</a></li>
<li><a href="#embedded-elisp">5.2. Embedded Elisp</a></li>
</ul>
</li>
<li><a href="#contributing">6. Contributing</a></li>
<li><a href="#acknowledgements">7. Acknowledgements</a></li>
<li><a href="#license">7. License</a></li>
</ul>
</li>
</ul>
</div>

## Supported Project Types

Skeletor comes with predefined project types so it is useful out-of-the-box:

- Clojure
- Elisp
- Haskell
- Python
- Ruby

## Installation

You will need Emacs 24+, `make` and [Cask](https://github.com/cask/cask) to
build the project.

```sh
cd
git clone git@github.com:chrisbarrett/skeletor.el.git
cd skeletor
make && make install
```

## Creating Projects

Use `M-x create-project` to create a new project based on an existing template.
You will be guided through any configuration needed.

By default, new projects are created in `~/Projects`. Customise
`skel-project-directory` to change this.

## Defining Project Templates

### Project Skeletons

Project skeletons are prepared project templates that are used to construct new
projects. There are two parts to defining a new project skeleton:

1. Create a template directory in the `skel-user-directory`. It can contain any
   files and directories you'd like.

2. Use the `define-project-skeleton` macro to configure how the project template
   will be created.

For example, here's the directory structure of the
[elisp-package](https://github.com/chrisbarrett/skeletor.el/tree/master/project-skeletons/elisp-package)
template:

    elisp-package/
    |-- .gitignore
    |-- CONTRIBUTING.md
    |-- Cask
    |-- Makefile
    |-- README.md
    |-- __PROJECT-NAME__.el
    `-- doc
        `-- __PROJECT-NAME__.org

And here's a corresponding configuration:

```lisp
(define-project-skeleton "elisp-package")
```

The above definition generates all the bindings you need to start using the
skeleton. `define-project-skeleton` also allows you to perform more specialised
configuration if necessary:

```lisp
(define-project-skeleton "elisp-package"
  :default-license (rx bol "gpl")
  :substitutions '(("__DESCRIPTION__" . (lambda () (read-string "Description: "))))
  :after-creation
  (lambda (dir)
    (skel-async-shell-command dir "make env")))
```

### External Tools

It is possible to use external tools to handle the creation of a project, while
still using Skeletor to perform additional configuration. The
`define-skeleton-constructor` macro is similar to `define-project-skeleton`, but
it lets you use an arbitrary Elisp command to perform the project setup.

The example below uses `bundler` to create a Ruby project, then add RSpec tests.

```lisp
(define-skeleton-constructor "Ruby Gem"
  :requires-executables '(("bundle" . "http://bundler.io"))
  :initialise
  (lambda (name project-dir)
    (skel-shell-command
     project-dir (format "bundle gem %s" (shell-quote-argument name)))
    (when (and (executable-find "rspec")
               (y-or-n-p "Create RSpec test suite? "))
      (skel-shell-command (f-join project-dir name) "rspec --init"))))
```

## Variables and Expansions in Templates

### Substitutions

File and directory names may contain special tokens that will be expanded when a
project is created.

For example, the elisp template contains a file named `__PROJECT-NAME__.el`.
Given a project named *foo*, this would be expanded as `foo.el`.

The same principle applies to the contents of files in a template. A file with
the contents:

    Title: Secret Plans
    Project: __PROJECT-NAME__
    Author: __USER-NAME__

could be instantiated as the following:

    Title: Secret Plans
    Project: Capture Battle-Cat
    Author: Skeletor

The variable `skel-global-substitutions` defines the substitutions available in
all project templates, and each template may declare its own special
substitutions.

### Embedded Elisp

Template files may contain embedded Elisp expressions that will be evaluated and
replaced when the project is created. For example, a file with the contents:

    Current Time: __(format-time-string "%c")__
    Current OS:   __(shell-command-to-string "uname")__

could expand to:

    Current Time: Thu Dec 19 16:14:35 2013
    Current OS:   Darwin

## Contributing

Yes, please do! More project types are especially welcome. Read over
[CONTRIBUTING](https://github.com/chrisbarrett/skeletor.el/blob/master/CONTRIBUTING.md)
for guidelines.

## Acknowledgements

Muchas gracias to [@magnars](https://twitter.com/magnars) and
[@rejeep](https://twitter.com/rejeep) for their excellent libraries and tooling.
You guys are stars!

## License

See [COPYING](https://github.com/chrisbarrett/skeletor.el/blob/master/COPYING).
Copyright (c) 2013 Chris Barrett.
