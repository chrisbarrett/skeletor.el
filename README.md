# <img align="right" src="assets/skeletor.jpg"> skeletor.el [![Build Status](https://travis-ci.org/chrisbarrett/skeletor.el.png?branch=master)](https://travis-ci.org/chrisbarrett/skeletor.el)

Skeletor provides project templates for Emacs. It also automates the mundane
parts of setting up a new project like version control, licenses and tooling.

Skeletor comes with a number of predefined predefined templates and allows you
to easily create your own.

## Table of Contents ##

- [ skeletor.el ](#user-content--skeletorel-)
	- [Supported Project Types](#user-content-supported-project-types)
	- [Installation](#user-content-installation)
		- [MELPA Installation](#user-content-melpa-installation)
		- [Manual installation](#user-content-manual-installation)
	- [Usage](#user-content-usage)
	- [Extending](#user-content-extending)
		- [Project Skeletons](#user-content-project-skeletons)
			- [Creating a Skeleton Directory](#user-content-creating-a-skeleton-directory)
			- [Configuring the Skeleton](#user-content-configuring-the-skeleton)
		- [Substitutions](#user-content-substitutions)
			- [Introduction](#user-content-introduction)
			- [Specifying Substitutions](#user-content-specifying-substitutions)
			- [Embedded Elisp](#user-content-embedded-elisp)
		- [External Tools](#user-content-external-tools)
	- [Contributing](#user-content-contributing)
	- [Acknowledgements](#user-content-acknowledgements)
	- [License](#user-content-license)


## Supported Project Types ##

Skeletor comes with predefined project types so it is useful out-of-the-box:

-   Clojure
-   Elisp
-   Haskell
-   Python
-   Ruby

Skeletor is designed to be extensible so you can create your own templates.

## Installation ##

Skeletor is available on [MELPA][]. This is the easiest way to install.

### MELPA Installation ###

Add the following to your init.el:

```lisp
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)
```

Run the following in Emacs to install Skeletor:

    M-x package-install skeletor

### Manual installation ###

You will need Emacs 24+, _make_ and [Cask][] to build the project. You will also
need to configure MELPA to install dependencies.

Add the following to your init.el:

```lisp
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)
```

Then run the following in the shell to clone Skeletor and install it.

```sh
cd
git clone git@github.com:chrisbarrett/skeletor.el.git
cd skeletor
make && make install
```

## Usage ##

Use `M-x skeletor-create-project` to create a new project based on an existing template.
You will be guided through any configuration needed.

By default, new projects are created in `~/Projects`. Customise
`skeletor-project-directory` to change this.

An additional command, `skeletor-create-project-at`, allows you to choose the
directory where the project will be created.

## Extending ##

Skeletor allows you to define new types of projects. You can either create a
project template, called a *skeleton*, or you can use an external tool to create
the project and use Skeletor to perform additional configuration.

### Project Skeletons ###

Project skeletons are templates for constructing new projects. The following
discussion will use the terms *skeleton* and *template* interchangeably.

There are two parts to defining a new project skeleton, discussed in the
following sections.

#### Creating a Skeleton Directory ####

Skeletor uses physical files and directories to represent project templates.
When you create a project, Skeletor will copy the template directory and apply
certain transformations.

To create a template, make a new directory in `skeletor-user-directory`. It can
contain any files and directories you'd like.

It is a good idea to add any files needed for distributing the project source.
Most of the templates that ship with Skeletor include a Makefile, `.gitignore`,
README and contributing guidelines.

Below is an example project skeleton structure for Emacs Lisp:

    my-elisp-package/
    |-- .gitignore
    |-- CONTRIBUTING.md
    |-- Cask
    |-- Makefile
    |-- README.md
    |-- __PROJECT-NAME__.el
    `-- doc
        `-- __PROJECT-NAME__.org

Note that Skeletor will handle the creation of a license file itself, so you
should not add one to your skeleton.

#### Configuring the Skeleton ####

Once you have created a project skeleton, use the `skeletor-define-template`
macro to configure how the project template will be created.

1.  Basic Configuration

    In the simplest case, you just need to tell Skeletor the name of the template:

    ```lisp
    (skeletor-define-template "my-elisp-package")
    ```

    This will add `my-elisp-package` to the list of available projects. You can now
    create an instance by calling `M-x skeletor-create-project my-elisp-package`. Skeletor
    will manage the creation of the project, prompt you to choose a license, and
    initialise a git repository at the root of the project.

2.  Titles

    You can set a custom title for your project type using the `:title` keyword
    parameter.

    ```lisp
    (skeletor-define-template "my-elisp-package"
      :title "My Elisp Package")
    ```

    The title is the string that represents the project in the `skeletor-create-project`
    prompt.

3.  Default Licenses

    Skeletor will prompt you to select a license when you create a project. Some
     communities favour a particular license, so Skeletor allows you to pre-populate
     the license prompt.

    For example, Elisp projects are generally licensed under GPL:

    ```lisp
    (skeletor-define-template "my-elisp-package"
      ; ...
      :default-license (rx bol "gpl"))
    ```

    Note that the argument to `:default-license` is a regular expression so you
    don't have to specify the license name precisely.

4.  Custom Actions

    You can use the `:after-creation` keyword parameter to perform additional
    actions after a project has been created. It takes a single-parameter function
    taking the path to the newly-created project.

    For example, the Elisp project runs a Makefile task in the background to
    configure the development environment:

    ```lisp
    (skeletor-define-template "elisp-package"
      ; ...
      :after-creation
      (lambda (dir)
        (skeletor-async-shell-command dir "make env")))
    ```

    You can do anything you want in the `after-creation` command, but it is a good
    idea to automate as much of the environment setup as possible using a makefile
    or shell script. This will help other developers who want to contribute to your
    project.

5.  External Tools

    Sometimes you need to use an external tool to perform part of the project
    configuration. Skeletor provides the `skeletor-shell-command` and
    `skeletor-async-shell-command` functions for this purpose. These functions output to
    special buffers and assert that their shell commands were successful.


    ```lisp
    (skeletor-define-template "elisp-package"
      ; ...
      :after-creation
      (lambda (dir)
        (skeletor-async-shell-command dir "make env")))
    ```

    Because such external tools may not be installed on every system, Skeletor
    provides a way to declare these requirements up-front using the
    `:requires-executables` keyword parameter. It takes an alist of `(PROGRAM .
    URL)`, where `URL` is a link to a project page or download instructions.

    For example, the `elisp-package` template uses `make` and `Cask` to bootstrap
    the development environment and declares its dependency on these programs:


    ```lisp
    (skeletor-define-template "elisp-package"
      ; ...
      :requires-executables '(("make" . "http://www.gnu.org/software/make/")
                              ("cask" . "https://github.com/cask/cask")))
    ```

    Skeletor will search for these two programs when creating an instance of the
    template. It will display a help window with download links if either of them
    cannot be found.

### Substitutions ###

Skeletor can perform text substitutions when it creates new projects. This makes
it possible to refer to the name of the project, add time-stamps and customise
the contents of files according to user input when a project is created.

#### Introduction ####

The `__PROJECT-NAME__` substitution is a useful example. Given the following
skeleton,

    my-elisp-package/
    |-- __PROJECT-NAME__.el
    `-- doc
        `-- __PROJECT-NAME__.org

the project name entered by the user will be used to name the files. Given a
project named *foo*, Skeletor would instantiate this skeleton as:

    foo/
    |-- foo.el
    `-- doc
        `-- foo.org

Substitutions are also applied to the text inside files. A file with the
contents,

    Name: __USER-NAME__
    Project: __PROJECT-NAME__

might be expanded as:

    Name: Jane Coder
    Project: foo

#### Specifying Substitutions ####

The `skeletor-global-substitutions` variable defines the substitutions available to
all skeletons. It is an alist, where each element is a cons of `(STRING .
REPLACEMENT)`. `REPLACEMENT` should be a string literal, a variable name, a
function name, or a lambda expression.

You can add your own items to `skel-globl-substitutions`. For example:

```lisp
(add-to-list 'skeletor-global-substitutions
             '("__ORGANISATION__" . "Masters of the Universe"))

(add-to-list 'skeletor-global-substitutions
             (cons "__HOME__" (getenv "HOME")))

(add-to-list 'skeletor-global-substitutions
             (cons "__TIME__" (lambda () (format-time-string "%c"))))
```

You can also define substitutions available to individual skeletons:

```lisp
(skeletor-define-template "my-package"
  :substitutions
  '(("__DESCRIPTION__" . (lambda () (read-string "Description: ")))))
```

This will prompt you to enter a description when creating an instance of this
project.

#### Embedded Elisp ####

Template files may contain embedded Elisp expressions that will be evaluated
when the project is created. The expression will be replaced by its result. The
syntax is `__(expression)__`.

For example, a template file with the contents:

    Current Time: __(format-time-string "%c")__
    Current OS:   __(shell-command-to-string "uname")__

could be expanded to:

    Current Time: Thu Dec 19 16:14:35 2013
    Current OS:   Darwin

### External Tools ###

Some communities have well-established tools for creating projects from
templates. Skeletor may still be used to orchestrate these tools and perform
additional setup steps.

Skeletor provides the `skeletor-define-constructor` macro for this purpose. It
is similar to `skeletor-define-template`, but it requires you supply a function
that creates the project structure itself.

For example, [Bundler](http://bundler.io) is a popular tool in the Ruby community that can create new
Ruby projects. Skeletor provides the following binding:

```lisp
(skeletor-define-constructor "Ruby Gem"
  :requires-executables '(("bundle" . "http://bundler.io"))
  :no-license? t

  :initialise
  (lambda (name project-dir)
    (skeletor-shell-command
     project-dir (format "bundle gem %s" (shell-quote-argument name))))

  :after-creation
  (lambda (dir)
    (when (and (executable-find "rspec")
               (y-or-n-p "Create RSpec test suite? "))
      (skeletor-shell-command dir "rspec --init"))))
```

Skeletor will use `bundle` to create the project structure, offer to create an
RSpec test suite, then add everything to version control.

## Contributing ##

Yes, please do! More project types are especially welcome. Read over
[CONTRIBUTING][] for guidelines.

## Acknowledgements ##

Skeletor is based on [@magnars][]' Project Archetypes--one of many cool features
of his [.emacs.d](https://github.com/magnars/.emacs.d). This, and other goodies,
are covered in this [emacs chat session][] with Sacha Chua.

Muchas gracias to [@magnars][] and [@rejeep][] for their excellent libraries and
tooling. You guys are stars!

- [Yasuyuki Oka][] added support for customising the completing-read function
- [Damien Cassou][] contributed feedback and fixes to the Elisp template

## License ##

See [COPYING][]. Copyright (c) 2014 Chris Barrett.


[CASK]: https://github.com/cask/cask
[MELPA]: http://melpa.milkbox.net
[@magnars]: https://twitter.com/magnars
[@rejeep]: https://twitter.com/rejeep
[Yasuyuki Oka]: https://github.com/yasuyk
[Damien Cassou]: http://damiencassou.seasidehosting.st
[emacs chat session]: http://sachachua.com/blog/2013/11/emacs-chat-magnar-sveen-emacs-rocks/

[CONTRIBUTING]: ./CONTRIBUTING.md
[COPYING]: ./COPYING
