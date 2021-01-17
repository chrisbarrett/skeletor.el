;;; skeletor.el --- Provides project skeletons for Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Package-Requires: ((s "1.7.0") (f "0.14.0") (dash "2.2.0") (cl-lib "0.3") (let-alist "1.0.3")(emacs "24.1"))
;; Version: 1.5.2

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Skeletor provides project templates for Emacs.  It also automates the
;; mundane parts of setting up a new project like version control, licenses
;; and tooling.

;; Skeletor comes with a number of predefined templates and allows you to
;; easily create your own.
;;
;; To create a new project interactively, run 'M-x skeletor-create-project'.
;;
;; To define a new project, create a project template inside
;; `skeletor-user-directory', then configure the template with the
;; `skeletor-define-template' macro.
;;
;; See the info manual for all the details.

;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'cl-lib)
(require 'let-alist)
(require 'rx)
(require 'compile)
(autoload 'insert-button "button")
(autoload 'comint-mode "comint")

(defgroup skeletor nil
  "Provides customisable project skeletons for Emacs."
  :group 'tools
  :prefix "skeletor-"
  :link '(custom-manual "(skeletor)Top")
  :link '(info-link "(skeletor)Usage"))

(defcustom skeletor-user-directory (f-join user-emacs-directory "project-skeletons")
  "The directory containing project skeletons.
Each directory inside is available for instantiation as a project
skeleton."
  :group 'skeletor
  :type 'directory)

(defcustom skeletor-user-organisation (getenv "ORGANIZATION")
  "Used in template expansions to set the user organisation."
  :group 'skeletor
  :type '(choice (const :tag "None" nil)
                 (string :tag "Value")))

(defcustom skeletor-project-directory (f-join (getenv "HOME") "Projects")
  "The directory where new projects will be created."
  :group 'skeletor
  :type 'directory)

(defcustom skeletor-global-substitutions
  (list (cons "__YEAR__" (format-time-string "%Y"))
        (cons "__USER-NAME__" (lambda () user-full-name))
        (cons "__USER-MAIL-ADDRESS__" (lambda () user-mail-address))
        (cons "__ORGANISATION__" (lambda ()
                                   (or skeletor-user-organisation
                                       user-full-name))))
  "A list of substitutions available for expansion in all project skeletons.

Each alist element is comprised of (candidate . substitution),
where 'candidate' will be replaced with 'substitution'.
'substitution' may be a string literal, a variable that will be
evaluated or a function that will be called."
  :group 'skeletor
  :type '(alist :key-type 'string
                :value-type (choice string variable function)))

(defcustom skeletor-init-with-git (executable-find "git")
  "When non-nil, initialise newly created projects with a git repository."
  :group 'skeletor
  :type 'boolean)

(defcustom skeletor-show-project-command 'dired
  "The command to use to show newly-created projects.
Should be a function that accepts the path to the project as an
argument."
  :group 'skeletor
  :type 'function)

(defcustom skeletor-completing-read-function 'ido-completing-read
  "Function to be called when requesting input from the user."
  :group 'skeletor
  :type '(radio (function-item completing-read)
                (function :tag "Other")))

(defcustom skeletor-after-project-instantiated-hook nil
  "Hook run after a project is successfully instantiated.
Each function will be passed the path of the newly instantiated
project."
  :group 'skeletor
  :type 'hook)

(defcustom skeletor-shell-setup-finished-hook nil
  "Hook run after a project has been set up using `skeletor-with-shell-setup'.
Each function should accept a single argument that is the project path."
  :group 'skeletor
  :type 'hook)

(defgroup skeletor-python nil
  "Configuration for python projects in Skeletor."
  :group 'tools
  :prefix "skeletor-python-")

(defcustom skeletor-python-bin-search-path '("/usr/bin" "/usr/local/bin")
  "A list of paths to search for python binaries.

Python binaries found in these paths will be shown as canditates
when initialising virtualenv."
  :group 'skeletor-python
  :type '(repeat directory))

(defgroup skeletor-haskell nil
  "Configuration for haskell projects in Skeletor."
  :group 'tools
  :prefix "skeletor-haskell-")

(defcustom skeletor-hs-main-file-content
  "module Main where

main :: IO ()
main = undefined
"
  "The contents to insert when creating a Haskell main file."
  :group 'skeletor-haskell
  :type 'string)

(defcustom skeletor-hs-library-file-content-format
  "module %s where
"
  "Format string used to generate the contents of a new Haskell library file.
The format string should have one `%s' specfier, which is
replaced with the module name."
  :group 'skeletor-haskell
  :type 'string)

(defgroup skeletor-scala nil
  "Configuration for Scala projects in Skeletor."
  :group 'tools
  :prefix "skeletor-scala-")

(defcustom skeletor-scala-version nil
  "The version of Scala to use when generating SBT configuration files.

If this is nil, Skeletor will shell out to the Scala executable
to obtain its version."
  :group 'skeletor-scala
  :type 'string)

(defcustom skeletor-scala-use-ensime nil
  "When non-nil, add an ENSIME configuration file to scala projects."
  :group 'skeletor
  :type 'boolean)

;;; Vars for use in templates

(defvar skeletor-project-name nil
  "The name of the project currently being instantiated.")

(defvar skeletor-project-root nil
  "The base directory of the project currently being instantiated.")

(defvar skeletor-project-license nil
  "The license type of the project currently being instantiated.")

(defvar skeletor-project-spec nil
  "The full data structure representing the template being instantiated.

It is an alist, and can be conveniently inspected using
`let-alist' or `assoc'.

This is exposed under the caveat that it is used by Skeletor
internally and is subject to change.")

;;; -------------------------- Public Utilities --------------------------------

(defun skeletor-shell-command (command &optional dir)
  "Enqueue a shell command for this project, raising an error on a non-zero exit code.

* COMMAND is the shell command to execute.

* DIR is an unquoted path at which to run the command."
  (let ((code (skeletor--start-shell-process :command command :dir dir :async nil)))

    ;; Print output buffer error. Most useful for CI.
    (when noninteractive
      (unless (zerop code)
        (message "%s" (with-current-buffer (skeletor--current-project-shell-buffer)
                        (buffer-string)))))

    (cl-assert (zerop code) nil
               "Skeleton creation failed--see the output buffer for details")))

(defun skeletor-async-shell-command (command &optional dir)
  "Enqueue and an async shell command for this project.

* COMMAND is the shell command to execute.

* DIR is an unquoted path at which to run the command."
  (skeletor--start-shell-process :command command :dir dir :async t))

(defvar skeletor--interactive-process nil
  "The current interactive shell process.  See `skeletor-with-shell-setup'.")

(defvar skeletor--current-project-root nil)

(cl-defun skeletor-with-shell-setup
    (cmd callback &optional (dir skeletor--current-project-root))
  "Perform template setup using an interactive shell command.
Display the shell buffer for user input.

CMD is the shell command to call.

DIR will be used as the current directory.

If the command exits successfully,

- delete the shell buffer

- execute CALLBACK

- run `skeletor-shell-setup-finished-hook'.

This is intended to be used in the 'after-setup' stage of a
template declaration."
  (let ((bufname "*Skeletor Interactive Setup*"))
    (setq skeletor--interactive-process
          (start-process-shell-command
           "skeletorcmd" bufname
           (format "cd %s && %s" dir cmd)))
    (condition-case nil
        (let ((sentinel (lambda (proc state)
                          (setq skeletor--interactive-process nil)
                          (cond ((s-matches? "finished" state)
                                 (kill-buffer (process-buffer proc))
                                 (funcall callback)
                                 (skeletor--log-info "Interactive setup finished")
                                 (run-hook-with-args 'skeletor-shell-setup-finished-hook dir))
                                (t
                                 (skeletor--log-error state))))))
          (set-process-sentinel skeletor--interactive-process sentinel)
          (switch-to-buffer bufname)
          (comint-mode))
      (error
       (setq skeletor--interactive-process nil)))))

(defun skeletor-require-executables (alist)
  "Check that executables can be located in the `exec-path'.
Show a report with installation instructions if any cannot be
found.

ALIST is a list of `(PROGRAM-NAME . URL)', where URL points to
download instructions."
  (-when-let (not-found (--remove (executable-find (car it)) alist))
    (let ((buf (skeletor--current-project-shell-buffer)))
      (with-help-window buf
        (with-current-buffer buf
          (goto-char (point-max))
          (skeletor--log-error
           (concat
            "This template requires external tools which "
            "could not be found.\n\n"
            "See each item below for installation instructions.\n"))
          (-each not-found (-lambda ((program . url))
                             (let ((inhibit-read-only t))
                               (insert "\n - ")
                               (insert-button program
                                              'action (lambda (x) (browse-url (button-get x 'url)))
                                              'url url)))))))
    (user-error "Cannot find executable(s) needed to create project")))

;;; ----------------------------- Internal -------------------------------------

(defvar-local skeletor--command-queue nil)

(cl-defun skeletor--start-shell-process (&key command async dir)
  "Execute the given COMMAND-SPEC in the project's shell output buffer."
  (let ((dir (or dir skeletor--current-project-root)))
    (with-current-buffer (skeletor--current-project-shell-buffer)
      (add-to-list 'skeletor--command-queue (list :command command :async async :dir dir) t)
      (skeletor--execute-command-queue (current-buffer)))))

(defun skeletor--execute-command-queue (buf)
  "Execute commands enqueued for the given shell output buffer BUF."
  (with-current-buffer buf
    (when skeletor--command-queue
      (-let [(&plist :command cmd :async async :dir dir) (pop skeletor--command-queue)]
        (skeletor--insert-shell-command-arrow dir cmd)
        (let* ((cmd (format "cd %s && %s" dir cmd))
               (proc (start-process-shell-command "skeletor" buf cmd))
               (cont (lambda (_ state)
                       (with-current-buffer buf
                         (cond ((s-matches? "finished" state)
                                (skeletor--log-exitcode "Done")
                                (goto-char (point-max))
                                (skeletor--execute-command-queue buf))
                               (t
                                (skeletor--log-error state)))))))
          (set-process-sentinel proc cont)
          (unless async
            (while (process-live-p proc)
              (sit-for 0.1)))
          (process-exit-status proc))))))

(defun skeletor--current-project-shell-buffer ()
  "Return the shell buffer for the current project, creating it if needed."
  (let ((dir skeletor--current-project-root))
    (with-current-buffer (get-buffer-create (format "*Skeletor [%s]*" (f-filename dir)))
      (read-only-mode +1)
      (skeletor--maybe-insert-shell-buffer-banner dir)
      (current-buffer))))

(defun skeletor--maybe-insert-shell-buffer-banner (dir)
  "Insert a banner for the current shell output buffer.  DIR is the project root."
  (when (s-blank? (buffer-string))
    (let ((inhibit-read-only t))
      (insert (propertize (format "Skeletor setup commands for project at %s\n"
                                  (f-short dir))
                          'face 'compilation-info)))))

(defun skeletor--insert-shell-command-arrow (dir cmd)
  "Insert CMD into the current buffer with an arrow."
  (goto-char (point-max))
  (let ((inhibit-read-only t))
    (insert (propertize (format "\n--> [%s]: %s\n" (f-short dir) cmd)
                        'face 'compilation-warning))))

(defun skeletor--log-note (format-string &rest args)
  (apply 'skeletor--log (propertize format-string 'face 'compilation-info) args))

(defun skeletor--log-error (format-string &rest args)
  (apply 'skeletor--log (propertize format-string 'face 'compilation-error) args))

(defun skeletor--log-info  (format-string &rest args)
  (apply 'skeletor--log (propertize format-string 'face 'compilation-mode-line-run) args))

(defun skeletor--log (format-string &rest args)
  "Display a message and write it to the shell command buffer."
  (let ((msg (apply 'format format-string args)))
    (with-current-buffer (skeletor--current-project-shell-buffer)
      (goto-char (point-max))
      (let ((inhibit-read-only t)) (insert (format "\n%s\n" msg)))
      (message "%s" (substring-no-properties msg)))))

(defun skeletor--log-exitcode (code)
  (let ((msg (propertize (format "%s\n" code) 'face 'compilation-info)))
    (with-current-buffer (skeletor--current-project-shell-buffer)
      (let ((inhibit-read-only t)) (insert msg)))))

(defvar skeletor--pkg-root (f-dirname (or load-file-name (buffer-file-name)))
  "The base directory of the Skeletor package.")

(defvar skeletor--directory
  (f-join skeletor--pkg-root "project-skeletons")
  "The directory containing built-in project skeletons.
Each directory inside is available for instantiation as a project
skeleton.")

(defvar skeletor--project-types nil
  "A list of SkeletorProjectType that represents the available templates.")

(defvar skeletor--licenses-directory (f-join skeletor--pkg-root "licenses")
  "The directory containing license files for projects.")

(cl-defstruct (SkeletorTemplate
               (:constructor SkeletorTemplate (path files dirs)))
  "Represents a project template.

* PATH is the path to this project template.

* DIRS is a list of all directories in the filesystem tree beneath PATH.

* FILES is a list of all files in the filesystem tree beneath PATH."
  path files dirs)

(cl-defstruct (SkeletorExpansionSpec
               (:constructor SkeletorExpansionSpec (files dirs)))
  "Represents a project template with expanded filenames.

* DIRS is a list of conses, where the car is a path to a dir in
  the template and the cdr is that dirname with all substitutions performed.

* FILES is a list of conses, where the car is a path to a file in
  the template and the cdr is that filename with all substitutions
  performed."
  files dirs)

(cl-defstruct (SkeletorProjectType
               (:constructor SkeletorProjectType (title constructor)))
  "Represents a project type that can be created by the user.

* TITLE is the string representation of the template to be shown
  in the UI.

* CONSTRUCTOR is a command to call to construct an instance of the skeleton."
  title constructor)

;; FilePath -> IO SkeletorTemplate
(defun skeletor--dir->SkeletorTemplate (path)
  "Construct a SkeletorTemplate from the filesystem entries at PATH."
  (SkeletorTemplate path (f-files path nil t) (f-directories path nil t)))

;; [(String,String)], FilePath -> SkeletorExpansionSpec
(defun skeletor--expand-template-paths (substitutions dest eval-embedded-elisp? template)
  "Expand all file and directory names in a template.
Return a SkeletorExpansionSpec.

* SUBSTITUTIONS is an alist as accepted by `s-replace-all'.

* DEST is the destination path for the template.

* EVAL-EMBEDDED-ELISP? is flag that indicates whether we should
  perform embedded elisp evaluation.

* TEMPLATE is a SkeletorTemplate."
  (cl-assert (stringp dest))
  (cl-assert (listp substitutions))
  (cl-assert (SkeletorTemplate-p template))
  (cl-flet ((expand (it)
                    (->> (skeletor--replace-all (cons (cons "__DOT__" ".") substitutions)
                                                it
						eval-embedded-elisp?)
                         (s-chop-prefix (SkeletorTemplate-path template))
                         (s-prepend (s-chop-suffix (f-path-separator) dest)))))
    (SkeletorExpansionSpec
     (--map (cons it (expand it)) (SkeletorTemplate-files template))
     (--map (cons it (expand it)) (SkeletorTemplate-dirs template)))))

(defun skeletor--evaluate-elisp-exprs-in-string (str)
  "Evaluate any elisp expressions in string STR.
An expression has the form \"__(expr)__\"."
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (let ((sexp-prod (rx "__" (group "(" (+? anything) ")") "__")))
      (while (search-forward-regexp sexp-prod nil t)
        (with-demoted-errors "Error: %s"
          (replace-match
           (save-match-data
             (format "%s" (eval (read (match-string 1))))) t))))
    (buffer-string)))

;; [(String,String)], String -> String
(defun skeletor--replace-all (substitutions str eval-embedded-elisp?)
  "Expand SUBSTITUTIONS in STR with fixed case.
Like `s-replace-all' but preserves case of the case of the
substitution.  EVAL-EMBEDDED-ELISP? is flag that indicates whether
we should perform embedded elisp evaluation."
  (let ((expanded (if eval-embedded-elisp?
		      (skeletor--evaluate-elisp-exprs-in-string str)
		    str)))
    (if substitutions
        (replace-regexp-in-string (regexp-opt (-map 'car substitutions))
                                  (lambda (it) (cdr (assoc it substitutions)))
                                  expanded 'fixcase)
      expanded)))

(defun skeletor--validate-substitutions (alist)
  "Assert that ALIST will be accepted by `s-replace-all'."
  (cl-assert (listp alist))
  (cl-assert (--all? (stringp (car it)) alist))
  (cl-assert (--all? (stringp (cdr it)) alist)))

;; [(String,String)], SkeletorExpansionSpec -> IO ()
(defun skeletor--instantiate-spec (substitutions  eval-embedded-elisp? spec)
  "Create an instance of the given template specification.

* SUBSTITUTIONS is an alist as accepted by `s-replace-all'.

* EVAL-EMBEDDED-ELISP? is flag that indicates whether we should
  perform embedded elisp evaluation.

* SPEC is a SkeletorExpansionSpec."
  (skeletor--validate-substitutions substitutions)
  (cl-assert (SkeletorExpansionSpec-p spec))
  (--each (-map 'cdr (SkeletorExpansionSpec-dirs spec))
    (make-directory it t))
  (--each (SkeletorExpansionSpec-files spec)
    (cl-destructuring-bind (src . dest) it
      (f-touch dest)
      (f-write (skeletor--replace-all substitutions (f-read src) eval-embedded-elisp?)
               'utf-8 dest))))

;; [(String,String)], FilePath, FilePath -> IO ()
(defun skeletor--instantiate-skeleton-dir (substitutions src dest eval-embedded-elisp?)
  "Create an instance of a project skeleton.

* SUBSTITUTIONS is an alist as accepted by `s-replace-all'.

* SRC is the path to the template directory.

* DEST is the destination path for the template.

* EVAL-EMBEDDED-ELISP? is flag that indicates whether we should
  perform embedded elisp evaluation."
  (skeletor--validate-substitutions substitutions)
  (cl-assert (stringp src))
  (cl-assert (f-exists? src))
  (cl-assert (stringp dest))
  (make-directory dest t)
  (->> (skeletor--dir->SkeletorTemplate src)
       (skeletor--expand-template-paths substitutions dest eval-embedded-elisp?)
       (skeletor--instantiate-spec substitutions eval-embedded-elisp?)))

;; FilePath -> IO ()
(defun skeletor--initialize-git-repo  (dir)
  "Initialise a new git repository at DIR."
  (let ((default-directory dir))
    (skeletor--log-info "Initialising git...")
    ;; Some tools (e.g. bundler) initialise git but do not make an initial
    ;; commit.
    (unless (f-exists? (f-join dir ".git"))
      (skeletor-shell-command "git init"))
    (skeletor-shell-command "git commit --allow-empty -m 'Initial commit'")
    (skeletor-shell-command "git add -A && git commit -m 'Add initial files'")
    (message "Initialising git...done")))

;; FilePath, FilePath, [(String,String)] -> IO ()
(defun skeletor--instantiate-license-file (license-file dest substitutions eval-embedded-elisp?)
  "Populate the given license file template.

* LICENSE-FILE is the path to the template license file.

* DEST is the path it will be copied to.

* SUBSTITUTIONS is an alist passed to `skeletor--replace-all'.

* EVAL-EMBEDDED-ELISP? is flag that indicates whether we should
  perform embedded elisp evaluation."
  (f-write (skeletor--replace-all substitutions (f-read license-file) eval-embedded-elisp?) 'utf-8 dest))

;; FilePath -> IO ()
(defun skeletor--show-project (dest)
  "Reveal the new project at DEST by calling `skeletor-show-project-command'."
  (let ((default-directory dest))
    (when skeletor-show-project-command
      (if skeletor--interactive-process
          (add-hook 'skeletor-shell-setup-finished-hook
                    skeletor-show-project-command)
        (funcall skeletor-show-project-command dest)))))

;; FilePath -> IO ()
(defun skeletor--prepare-git (dest)
  "Configure a git repo at DEST at an appropriate stage in the setup.
If there is an interactive process, wait until that is finished.
Otherwise immediately initialise git."
  (let ((default-directory dest))
    (if skeletor--interactive-process
        (add-hook 'skeletor-shell-setup-finished-hook 'skeletor--initialize-git-repo)
      (skeletor--initialize-git-repo dest))))

;;; ---------------------- User Interface Commands -----------------------------

;; (String,String) -> IO (String,String)
(cl-defun skeletor--eval-substitution ((token . repl))
  "Convert a substitution item according to the following rules:

* If the item is a lambda-function or function-name it will be called

* If it is a symbol will be eval'ed

* Otherwise the item will be used unchanged."
  (cons token (cond ((functionp repl)
                     (if (commandp repl)
                         (call-interactively repl)
                       (funcall repl)))
                    ((symbolp repl)
                     (eval repl))
                    (t
                     repl))))

(defvar skeletor--read-license-fn nil)

;; String, Regex -> IO FilePath
(defun skeletor--read-license (prompt default)
  "Prompt the user to select a license.

* PROMPT is the prompt shown to the user.

* DEFAULT a regular expression used to find the default."
  (let* ((license-names (--map (cons (s-upcase (f-filename it)) it)
                               (f-files skeletor--licenses-directory)))
         (default (unless (s-blank? default)
                    (car (--first (s-matches? default (car it)) license-names))))
         (choice (funcall (or skeletor--read-license-fn skeletor-completing-read-function)
                          prompt (-map 'car license-names) nil t default)))
    (cdr (assoc choice license-names))))

(defvar skeletor--read-project-type-fn nil)

;; IO SkeletorProjectType
(defun skeletor--read-project-type ()
  "Prompt the user to select a project skeleton."
  (let* ((skeletons (->> skeletor--project-types
                         (-map 'SkeletorProjectType-title)
                         (-sort 'string<)))
         (title
          (funcall (or skeletor--read-project-type-fn skeletor-completing-read-function)
                   "Skeleton: " skeletons nil t)))

    (--first (equal title (SkeletorProjectType-title it))
             skeletor--project-types)))

(defvar skeletor--read-project-name-fn 'read-string)

;; {String} -> IO String
(cl-defun skeletor--read-project-name (&optional (prompt "Project Name: "))
  "Read a project name from the user."
  (let* ((name (funcall skeletor--read-project-name-fn prompt))
         (dest (f-join skeletor-project-directory name)))
    (cond
     ((s-blank? name)
      (skeletor--read-project-name))
     ((f-exists? dest)
      (skeletor--read-project-name
       (format "%s already exists. Choose a different name: " dest)))
     (t
      name))))


;;; --------------------- Public Commands and Macros ---------------------------

;;;###autoload
(defmacro skeletor-define-template (&rest args)
  "Declare a new project type.

* NAME is a string naming the project type. A corresponding
  skeleton should exist in `skeletor--directory' or
  `skeletor-user-directory'.

* TITLE is the name to use when referring to this project type in
  the UI.

* SUBSTITUTIONS is an alist of (string . substitution) specifying
  substitutions to be used, in addition to the global
  substitutions defined in `skeletor-global-substitutions'. These
  are evaluated when creating an instance of the template.

* When NO-LICENSE? is t, the project will not be initialised with
  a license file.

* DEFAULT-LICENSE is a regexp matching the name of a license to
  be used as the default. This default is used to pre-populate
  the license prompt when creating an insance of the template.

* LICENSE-FILE-NAME is the filename to use for the generated
  license file.

* AFTER-CREATION is a unary function to be run once the project
  is created. It should take a single argument--the path to the
  newly-created project.

* REQUIRES-EXECUTABLES is an alist of `(PROGRAM . URL)'
  expressing programs needed to expand this skeleton. See
  `skeletor-require-executables'.

* When NO-EVAL-EMBEDDED-ELISP? is t, embedded elisp won't be evaluated
  inside template files.

\(fn NAME &key TITLE SUBSTITUTIONS BEFORE-GIT AFTER-CREATION NO-LICENSE? DEFAULT-LICENSE LICENSE-FILE-NAME REQUIRES-EXECUTABLES NO-EVAL-EMBEDDED-ELISP?)"
  (declare (indent 1))
  (-let [(name . keys) args]
    `(skeletor-define-constructor ,name
       :initialise skeletor--ctor-skeleton-initialisation-fn
       ,@keys)))

;;;###autoload
(defmacro skeletor-define-constructor (&rest args)
  "Define a new project type with a custom way of constructing a skeleton.
This can be used to add bindings for command-line tools.

* TITLE is a string naming the project type in the UI.

* INITIALISE is a unary function that creates the project
  structure. It will be passed an alist containing a
  specification for the skeleton, including the following keys:

  - `project-name': The name of the project read from the user
  - `project-dir': The directory at which the project should be created
  - `dest': The project-directory joined with the project name.

  Consider using `let-alist' to conveniently bind these variables
  to `.project-name', `.project-dir' and `.dest' in the scope of
  your initialisation function.

  INITIALISE is expected to initialise the new project at `dest'.
  The command should signal an error if this fails for any
  reason.

  Make sure to switch to a shell buffer if INITIALISE is a shell
  command that requires user interaction.

* SUBSTITUTIONS is an alist of (string . substitution) specifying
  substitutions to be used, in addition to the global
  substitutions defined in `skeletor-global-substitutions'. These
  are evaluated when creating an instance of the template.

* BEFORE-GIT is a unary function to be run once the project is
  created, but before git is initialised. It should take a single
  argument--the path to the newly-created project.

* AFTER-CREATION is a unary function to be run once the project
  is created. It should take a single argument--the path to the
  newly-created project.

* When NO-GIT? is t, the project will not be initialised with a
  git repo, regardless of the value of `skeletor-init-with-git'.

* When NO-LICENSE? is t, the project will not be initialised with
  a license file.

* DEFAULT-LICENSE is a regexp matching the name of a license to
  be used as the default. This default is used to pre-populate
  the license prompt when creating an insance of the template.

* LICENSE-FILE-NAME is the filename to use for the generated
  license file.

* REQUIRES-EXECUTABLES is an alist of `(PROGRAM . URL)'
  expressing programs needed to expand this skeleton. See
  `skeletor-require-executables'.

* When NO-EVAL-EMBEDDED-ELISP? is t, embedded elisp won't be evaluated
  inside template files.

\(fn TITLE &key INITIALISE SUBSTITUTIONS BEFORE-GIT AFTER-CREATION NO-GIT? NO-LICENSE? DEFAULT-LICENSE LICENSE-FILE-NAME REQUIRES-EXECUTABLES NO-EVAL-EMBEDDED-ELISP?)"
  (declare (indent 1))
  (-let* ((spec-alist (skeletor--process-macro-args args))
          ((&alist 'default-license-var default-license-var
                   'default-license     default-license
                   'constructor-fname   ctor
                   'name                name
                   'title               title)
           spec-alist))
    `(progn
       (defvar ,default-license-var ,default-license
         ,(skeletor--gen-license-var-docstring name))
       (setq ,default-license-var ,default-license)

       (defun ,ctor()
         ,(skeletor--gen-ctor-docstring name)
         (skeletor--run-ctor (skeletor--ctor-runtime-spec ',spec-alist)))

       (add-to-list 'skeletor--project-types
                    (SkeletorProjectType ,(or title name) ',ctor)))))

;; Supporting functions for macros must be evaluated at compile-time to satisfy the byte-compiler.
(eval-and-compile

  (defun skeletor--process-macro-args (args)
    "Check ARGS are well-formed, then process them into an alist."
    (-let* (((name . keys) args)
            (arg-alist (skeletor--plist-to-alist keys)))
      (skeletor--validate-macro-arguments name arg-alist)
      (let-alist arg-alist
        (list (cons 'constructor-fname (intern (format "skeletor--create-%s" name)))
              (cons 'title (or .title (s-join " " (-map 's-capitalize (s-split-words name)))))
              (cons 'name name)
              (cons 'use-git? (not .no-git?))
              (cons 'initialise-fn .initialise)
              (cons 'before-git (or .before-git 'ignore))
              (cons 'after-creation (or .after-creation 'ignore))
              (cons 'create-license? (not .no-license?))
              (cons 'license-file-name (or .license-file-name "COPYING"))
              (cons 'default-license-var (intern (format "%s-default-license" name)))
              (cons 'substitutions (eval .substitutions))
              (cons 'required-executables (eval .requires-executables))
              (cons 'eval-embedded-elisp? (not .no-eval-embedded-elisp?))))))

  (defun skeletor--plist-to-alist (plist)
    "Convert PLIST to an alist, replacing keyword keys with symbols."
    (->> (-partition-in-steps 2 2 plist)
         (-map (-lambda ((k v))
                 (cons (intern (s-chop-prefix ":" (symbol-name k)))
                       v)))))

  (defun skeletor--validate-macro-arguments (name args)
    (cl-assert (skeletor--alist-keys-are-all-legal? args)  t)
    (let-alist args
      (cl-assert (stringp name) t)
      (cl-assert (functionp .initialise) t)
      (cl-assert (or (null .title) (stringp .title)) t)
      (cl-assert (or (null .license-file-name) (stringp .license-file-name)) t)
      (cl-assert (or (null .before-git) (functionp .before-git)) t)
      (cl-assert (or (null .after-creation) (functionp .after-creation)) t)
      (let ((execs (eval .requires-executables)))
        (cl-assert (listp execs) t)
        (cl-assert (skeletor--alist-all-keys-are-strings? execs) t)
        (cl-assert (skeletor--alist-all-values-are-strings? execs) t))
      (let ((subs (eval .substitutions)))
        (cl-assert (listp subs) t)
        (cl-assert (skeletor--alist-all-keys-are-strings? subs) t))))

  (defun skeletor--alist-keys (alist) (-map 'car alist))
  (defun skeletor--alist-all-keys-are-strings? (alist)   (-all? 'stringp (skeletor--alist-keys alist)))
  (defun skeletor--alist-all-values-are-strings? (alist) (-all? 'stringp (-map 'cdr alist)))

  (defvar skeletor--legal-keys
    '(title initialise before-git after-creation no-git? no-license?
            default-license license-file-name requires-executables substitutions
	    no-eval-embedded-elisp?))

  (defun skeletor--alist-keys-are-all-legal? (alist)
    (null (-difference (skeletor--alist-keys alist) skeletor--legal-keys)))

  (defun skeletor--ctor-skeleton-initialisation-fn (runtime-spec)
    (let-alist runtime-spec
      (if .skeleton
          (skeletor--ctor-instantiate-project-from-skeleton runtime-spec)
        (let ((err (format "Skeleton %s not found" .name)))
          (skeletor--log-error err)
          (error err)))))

  (defun skeletor--ctor-runtime-spec (spec)
    "Concatenate the given macro SPEC with values evaluated at runtime."
    (let ((project-name (skeletor--read-project-name)))
      (let-alist spec
        (-concat (list
                  (cons 'project-name project-name)
                  (cons 'project-dir skeletor-project-directory)
                  (cons 'dest (f-join skeletor-project-directory project-name))
                  (cons 'skeleton (skeletor--get-named-skeleton .name))
                  (cons 'license-file
                        (when .create-license?
                          (skeletor--read-license "License: " .license-file-name)))
                  (cons 'repls (-map 'skeletor--eval-substitution
                                     (-concat
                                      skeletor-global-substitutions
                                      (list (cons "__PROJECT-NAME__" project-name)
                                            (cons "__LICENSE-FILE-NAME__" .license-file-name))
                                      .substitutions))))
                 spec))))

  (defun skeletor--run-ctor (runtime-spec)
    (let-alist runtime-spec
      (setq skeletor--current-project-root .dest)
      (setq skeletor-project-root .dest)
      (setq skeletor-project-name .project-name)
      (setq skeletor-project-license .license-file)
      (setq skeletor-project-spec runtime-spec)
      (skeletor--log-spec runtime-spec)
      (skeletor-require-executables .required-executables)
      (switch-to-buffer (skeletor--current-project-shell-buffer))
      (skeletor--create-project-skeleton runtime-spec)
      (skeletor--ctor-run-setup-steps runtime-spec)
      (skeletor--show-project .dest)))

  (defun skeletor--log-spec (runtime-spec)
    (skeletor--log-info "Creating with specification:")
    (skeletor--log "%s" (pp-to-string runtime-spec)))

  (defun skeletor--create-project-skeleton (runtime-spec)
    (let-alist runtime-spec
      (if .initialise-fn
          (funcall .initialise-fn runtime-spec)
        (let ((err (format "No initialisation function supplied for %s" .name)))
          (skeletor--log-error (concat err ". Specification:"))
          (skeletor--log-error (pp-to-string runtime-spec))
          (error err)))

      (skeletor--log-info "Project created at %s" .dest)))

  (defun skeletor--ctor-instantiate-project-from-skeleton (spec)
    (let-alist spec
      (unless (f-exists? skeletor-project-directory)
        (make-directory skeletor-project-directory t))
      (skeletor--instantiate-skeleton-dir .repls .skeleton .dest .eval-embedded-elisp?)
      (when .license-file
        (skeletor--instantiate-license-file
         .license-file (f-join .dest .license-file-name) .repls .eval-embedded-elisp?))))

  (defun skeletor--get-named-skeleton (name)
    (-first 'f-exists?
            (list (f-expand name skeletor-user-directory)
                  (f-expand name skeletor--directory))))

  (defun skeletor--ctor-run-setup-steps (runtime-spec)
    (let-alist runtime-spec
      (funcall .before-git .dest)
      (when (and .use-git? skeletor-init-with-git)
        (skeletor--prepare-git .dest)
        (skeletor--log-info "Git initialised"))
      (funcall .after-creation .dest)
      (run-hook-with-args 'skeletor-after-project-instantiated-hook .dest)))

  (defun skeletor--gen-license-var-docstring (name)
    (concat "Auto-generated variable.\n\nThe default license type for " name " skeletons."))

  (defun skeletor--gen-ctor-docstring (name)
    (concat "Auto-generated function.\n\nCreates a new " name " skeleton."))

  )

;;;###autoload
(defun skeletor-create-project (skeleton)
  "Interactively create a new project with Skeletor.

SKELETON is a SkeletorProjectType."
  (interactive (list (skeletor--read-project-type)))
  (funcall (SkeletorProjectType-constructor skeleton)))

;;;###autoload
(defun skeletor-create-project-at (dir skeleton)
  "Interactively create a new project with Skeletor.

DIR is destination directory, which must exist.

SKELETON is a SkeletorProjectType."
  (interactive (list (read-directory-name "Create at: " nil nil t)
                     (skeletor--read-project-type)))
  ;; Dynamically rebind the project directory.
  (let ((skeletor-project-directory dir))
    (skeletor-create-project skeleton)))

;;; ------------------------ Built-in skeletons --------------------------------

(skeletor-define-template "generic"
  :title "Generic Project")

(skeletor-define-template "elisp-package"
  :title "Elisp Package"
  :requires-executables '(("make" . "http://www.gnu.org/software/make/")
                          ("cask" . "https://github.com/cask/cask"))
  :default-license (rx bol "gpl")
  :substitutions
  '(("__DESCRIPTION__"
     . (lambda ()
         (read-string "Description: ")))))

(skeletor-define-template "elisp-package-with-docs"
  :title "Elisp Package (with documentation)"
  :requires-executables '(("make" . "http://www.gnu.org/software/make/")
                          ("cask" . "https://github.com/cask/cask"))
  :default-license (rx bol "gpl")
  :substitutions
  '(("__DESCRIPTION__"
     . (lambda ()
         (read-string "Description: ")))))

(defun skeletor-py--read-python-bin ()
  "Read a python binary from the user."
  (->> skeletor-python-bin-search-path
       (--mapcat
        (f-files it (lambda (f)
                      (s-matches? (rx "python" (* (any digit "." "-")) eol)
                                  f))))
       (funcall skeletor-completing-read-function "Python binary: ")))

(defun skeletor-py--read-virtualenv-bin ()
  "Read a python virtualenv binary from the user."
  (->> skeletor-python-bin-search-path
       (--mapcat
        (f-files it (lambda (f)
                      (s-matches? (rx "virtualenv" (* (any digit "." "-")) eol)
                                  f))))
       (funcall skeletor-completing-read-function "VirtualEnv binary: ")))

(defun skeletor-py--read-pip-bin ()
  "Read a python pip binary from the user."
  (->> skeletor-python-bin-search-path
       (--mapcat
        (f-files it (lambda (f)
                      (s-matches? (rx "pip" (* (any digit "." "-")) eol)
                                  f))))

       (funcall skeletor-completing-read-function "PYPI binary: ")))

(skeletor-define-template "python-library"
  :title "Python Library"
  :requires-executables '(("make" . "http://www.gnu.org/software/make/"))
  :substitutions '(("__PYTHON-BIN__" . skeletor-py--read-python-bin)
                   ("__VENV-BIN__" . skeletor-py--read-virtualenv-bin)
                   ("__PIP-BIN__" . skeletor-py--read-pip-bin)
                   ("__VENV-DIR__" . (lambda () (read-string "Root directory for the Python Virtual Environments: "))))
  :after-creation
  (lambda (dir)
    (skeletor-async-shell-command "make tooling")))

(defun skeletor-hs--cabal-sandboxes-supported? ()
  "Non-nil if the installed cabal version supports sandboxes.
Sandboxes were introduced in cabal 1.18 ."
  (let ((vers (->> (shell-command-to-string "cabal --version")
                   (s-match (rx (+ (any num "."))))
                   car
                   (s-split (rx "."))
                   (-map 'string-to-int))))
    (cl-destructuring-bind (maj min &rest rest) vers
      (or (< 1 maj) (<= 18 min)))))

(defun skeletor-hs--post-process-cabal-file (file)
  "Adjust fields in the cabal file.  FILE is the cabal file path."
  (with-current-buffer (find-file-noselect file)
    (goto-char (point-min))
    ;; Set src dir.
    (save-excursion
      (when (search-forward-regexp (rx (* space)
                                       (group-n 1 "--" (* space))
                                       "hs-source-dirs:" (* space) eol)
                                   nil t)
        (replace-match "" nil nil nil 1)
        (goto-char (line-end-position))
        (indent-to 23)
        (insert "src")))

    ;; Set main file.
    (save-excursion
      (when (search-forward-regexp (rx (* space)
                                       (group-n 1 "--" (* space))
                                       "main-is:" (* space) eol)
                                   nil t)
        (replace-match "" nil nil nil 1)
        (goto-char (line-end-position))
        (indent-to 23)
        (insert "Main.hs")))

    (save-buffer)
    (kill-buffer)))

(defun skeletor-hs--init-src-file (cabal-file src-dir)
  "Create either a Main.hs file or a toplevel library file.

CABAL-FILE is the path to the project's cabal file.

SRC-DIR is the path to the project src directory."
  (let* ((executable? (s-contains? "main-is:" (f-read-text cabal-file)))
         (module-name
          (if executable?
              "Main"
            (->> (f-base (f-parent cabal-file))
                 s-split-words
                 (-map 's-capitalize)
                 (s-join ""))))
         (path
          (f-join src-dir
                  (if executable? "Main.hs" (concat module-name ".hs"))))
         (str (if executable?
                  skeletor-hs-main-file-content
                (format skeletor-hs-library-file-content-format
                        module-name)))
         )
    (f-write str 'utf-8 path)))

(skeletor-define-template "haskell-project"
  :title "Haskell Project (Cabal)"
  :requires-executables '(("cabal" . "http://www.haskell.org/cabal/"))
  :no-license? t
  :before-git
  (lambda (dir)
    (when (skeletor-hs--cabal-sandboxes-supported?)
      (skeletor--log-info "Initialising sandbox...")
      (skeletor-shell-command "cabal sandbox init"))

    (skeletor--log-info "Running cabal init...")
    (skeletor-with-shell-setup "cabal init"
                               `(lambda ()
                                  (let* ((proj-root ,dir)
                                         (cabal-file (car (f-entries proj-root (lambda (f) (equal "cabal" (f-ext f))))))
                                         (src-dir (f-join proj-root "src")))
                                    (skeletor-hs--post-process-cabal-file cabal-file)
                                    (f-mkdir src-dir)
                                    (skeletor-hs--init-src-file cabal-file src-dir))))))

(skeletor-define-constructor "Haskell Project (Stack)"
  :title "Haskell Project (Stack)"
  :no-license? t
  :requires-executables '(("stack" . "http://docs.haskellstack.org/en/stable/README.html"))
  :initialise
  (lambda (spec)
    (let-alist spec
      (skeletor-shell-command (format "stack new %s" (shell-quote-argument .project-name))
                              .project-dir))))

(skeletor-define-constructor "Ruby Gem"
  :requires-executables '(("bundle" . "http://bundler.io"))
  :no-license? t
  :initialise
  (lambda (spec)
    (let-alist spec
      (skeletor-shell-command (format "bundle gem %s" (shell-quote-argument .project-name))
                              .project-dir)))
  :before-git
  (lambda (dir)
    (when (and (executable-find "rspec")
               (y-or-n-p "Create RSpec test suite? "))
      (skeletor-shell-command "rspec --init"))))

(defvar skeletor-clj--project-types-cache nil
  "A list of strings representing the available Leiningen templates.")

(defun skeletor-clj--project-types ()
  "Parse the project templates known to Leiningen.
Return a list of strings representing the available templates.

This is a lengthy operation so the results are cached to
`skeletor-clj--project-types-cache'."
  (or skeletor-clj--project-types-cache
      (let ((types (->> (shell-command-to-string "lein help new")
                        (s-match
                         (rx bol "Subtasks available:\n" (group (+? anything)) "\n\n"))
                        cadr
                        (s-split "\n")
                        (--keep (cadr (s-match (rx bol (* space) (group (+ (not space))))
                                               it))))))
        (prog1 types
          (setq skeletor-clj--project-types-cache types)))))

(skeletor-define-constructor "Clojure Project"
  :requires-executables '(("lein" . "http://leiningen.org/"))
  :initialise
  (lambda (spec)
    (let-alist spec
      (message "Finding Leningen templates...")
      (let ((type (funcall skeletor-completing-read-function
                           "Template: " (skeletor-clj--project-types) nil t "default")))
        (skeletor-shell-command (format "lein new %s %s"
                                        (shell-quote-argument type)
                                        (shell-quote-argument .project-name))
                                .project-dir)))))

(defun skeletor-scala--version ()
  "Get the version of the installed scala executable."
  (or skeletor-scala-version
      (progn
        (message "Getting Scala version number...")
        (let* ((str (shell-command-to-string "scala -version"))
               (vers (cadr (s-match (rx (+ (not (any digit))) (group (+ (any digit ".")))) str))))
          (setq skeletor-scala-version vers)
          vers))))

(skeletor-define-template "scala-project"
  :title "Scala Project"
  :requires-executables '(("scala" . "http://www.scala-lang.org")
                          ("sbt" . "http://www.scala-sbt.org"))
  :substitutions '(("__SCALA-VERSION__" . skeletor-scala--version))
  :after-creation
  (lambda (dir)
    (when skeletor-scala-use-ensime
      (skeletor--log-info "Configuring SBT and ENSIME. This may take a while...")
      (skeletor-async-shell-command "sbt gen-ensime"))))

(provide 'skeletor)

;;; skeletor.el ends here
