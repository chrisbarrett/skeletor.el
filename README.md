# skeletor.el

Skeletor allows you to easily create project templates in Emacs. It comes with a
number of predefined templates and allows you to easily create your own.

![Skeletor Laughing](assets/skeletor.jpg)

<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#sec-1">1. skeletor.el</a>
<ul>
<li><a href="#sec-1-1">1.1. Installation</a></li>
<li><a href="#sec-1-2">1.2. Creating Projects</a></li>
<li><a href="#sec-1-3">1.3. Defining Project Templates</a>
<ul>
<li><a href="#sec-1-3-1">1.3.1. Structure</a></li>
<li><a href="#sec-1-3-2">1.3.2. Token Expansion</a></li>
</ul>
</li>
<li><a href="#sec-1-4">1.4. Contributing</a></li>
<li><a href="#sec-1-5">1.5. License</a></li>
</ul>
</li>
</ul>
</div>
</div>

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

### Structure

There are two simple parts to defining a new project:

1. Create a template directory in the `skel-user-directory`, containing any
    files and directories you'd like

2. Use the `define-project-skeleton` macro to configure how the project template
    will be created.

For example, here's the directory structure of the elisp-package skeleton:

    elisp-package/
    |-- CONTRIBUTING.org
    |-- Cask
    |-- Makefile
    |-- README.org
    |-- __PROJECT-NAME__.el
    `-- doc
        `-- __PROJECT-NAME__.org

And here's the corresponding definition:

```lisp
(define-project-skeleton "elisp-package"
  :default-license (rx bol "gpl")
  :replacements '(("__DESCRIPTION__" . (lambda ()
                                         (read-string "Description: "))))
  :after-creation
  (lambda (dir)
    (async-shell-command
     (format "cd %s && make env" (shell-quote-argument dir))
     skel--shell-buffer-name)))
```

### Token Expansion

Filenames and file contents may contain special tokens that will be expanded
when the project is created. For example, a template file with the contents:

    Title: Secret Plans
    Project: __PROJECT-NAME__
    Author: __USER-NAME__

could be instantiated as the following:

    Title: Secret Plans
    Project: Capture Battle-Cat
    Author: Skeletor

The variable `skel-global-replacements` defines the replacements available in
all project templates, and each template may declare its own special
replacements.

## Contributing

Yes, please do! More project types are especially welcome. Read over
CONTRIBUTING.md for guidelines.

## License

See COPYING. Copyright (c) 2013 Chris Barrett.
