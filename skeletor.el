;;; skeletor.el --- Provides project skeletons for Emacs

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Package-Requires: ((s "1.7.0") (f "0.14.0") (dash "2.2.0") (cl-lib "0.3") (emacs "24.1"))
;; Version: 0.2

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

;; Provides project skeletons for Emacs.
;;
;; To create a new project interactively, run 'M-x create-project'.
;;
;; To define a new project, create a project template inside
;; `skel-user-directory', then configure the template with the
;; `define-project-skeleton' macro.
;;
;; See the info manual for all the details.

;;; Code:

(eval-and-compile
  ;; Add cask packages to load path so flycheck checkers work.
  (when (boundp' flycheck-emacs-lisp-load-path)
    (dolist (it (file-expand-wildcards "./.cask/*/elpa/*"))
      (add-to-list 'flycheck-emacs-lisp-load-path it))))

(require 'dash)
(require 's)
(require 'f)
(require 'cl-lib)

(defgroup skeletor nil
  "Provides customisable project skeletons for Emacs."
  :group 'tools
  :prefix "skel-"
  :link '(custom-manual "(skeletor)Top")
  :link '(info-link "(skeletor)Usage"))

(defcustom skel-user-directory (f-join user-emacs-directory "project-skeletons")
  "The directory containing project skeletons.
Each directory inside is available for instantiation as a project
skeleton."
  :group 'skeletor
  :type 'directory)

(defcustom skel-project-directory (f-join (getenv "HOME") "Projects")
  "The directory where new projects will be created."
  :group 'skeletor
  :type 'directory)

(defcustom skel-global-replacements
  (list (cons "__YEAR__" (format-time-string "%Y"))
        (cons "__USER-NAME__" user-full-name)
        (cons "__USER-MAIL-ADDRESS__" user-mail-address)
        (cons "__ORGANISATION__" (if (boundp 'user-organisation)
                                     user-organisation
                                   user-full-name)))
  "A list of replacements available for expansion in all project skeletons.

Each alist element is comprised of (candidate . replacement),
where 'candidate' will be substituted for 'replacement'.
'replacement' may be a simple string, a variable that will be
evaluated or a function that will be called."
  :group 'skeletor
  :type '(alist :key-type 'string
                :value-type (choice string variable function)))

(defcustom skel-init-with-git t
  "When non-nil, initialise newly created projects with a git repository."
  :group 'skeletor
  :type 'boolean)

(defcustom skel-show-project-command 'dired
  "The command to use to show newly-created projects.
Should be a function that accepts the path to the project as an
argument."
  :group 'skeletor
  :type 'function)

(defcustom skel-after-project-instantiated-hook nil
  "Hook run after a project is successfully instantiated.
Each function will be passed the path of the newly instantiated
project."
  :group 'skeletor
  :type 'hook)

(defgroup skeletor-python nil
  "Configuration for python projects in Skeletor."
  :group 'tools
  :prefix "skel-python-")

(defcustom skel-python-bin-search-path '("/usr/bin" "/usr/local/bin")
  "A list of paths to search for python binaries.

Python binaries found in these paths will be shown as canditates
when initialising virtualenv."
  :group 'skeletor-python
  :type '(repeat directory))

;;; -------------------------- Public Utilities --------------------------------

(defun skel-shell-command (dir command)
  "Run a shell command.

* DIR is an unquoted path at which to run the command.

* COMMAND is the shell command to execute."
  (let ((buf (get-buffer-create
              (format "*Skeletor [%s]*" dir))))
    (with-current-buffer buf
      (erase-buffer))
    (shell-command
     (format "cd %s && %s" (shell-quote-argument dir) command)
     buf)))

(defun skel-async-shell-command (dir command)
  "Run an async shell command.

* DIR is an unquoted path at which to run the command.

* COMMAND is the shell command to execute."
  (let ((buf (get-buffer-create
              (generate-new-buffer-name
               (format "*Skeletor [%s]*" dir)))))
    (with-current-buffer buf
      (erase-buffer))
    (async-shell-command
     (format "cd %s && %s" (shell-quote-argument dir) command)
     buf
     (format "*Skeleton Errors [%s]*" dir))))

;;; ----------------------------- Internal -------------------------------------

(defvar skel--pkg-root (f-dirname (or load-file-name (buffer-file-name)))
  "The base directory of the Skeletor package.")

(defvar skel--directory
  (f-join skel--pkg-root "project-skeletons")
  "The directory containing built-in project skeletons.
Each directory inside is available for instantiation as a project
skeleton.")

(defvar skel--project-skeletons nil "The list of available project skeletons.")

(defvar skel--licenses-directory (f-join skel--pkg-root "licenses")
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
  the template and the cdr is that dirname with all replacements performed.

* FILES is a list of conses, where the car is a path to a file in
  the template and the cdr is that filename with all replacements
  performed."
  files dirs)

;; FilePath -> SkeletorTemplate
(defun skel--dir->SkeletorTemplate (path)
  "Construct a SkeletorTemplate from the filesystem entries at PATH."
  (SkeletorTemplate path (f-files path nil t) (f-directories path nil t)))

;; [(String,String)], FilePath -> SkeletorExpansionSpec
(defun skel--expand-template-paths (replacements dest template)
  "Expand all file and directory names in a template.
Return a SkeletorExpansionSpec.

* REPLACEMENTS is an alist as accepted by `s-replace-all'.

* DEST is the destination path for the template.

* TEMPLATE is a SkeletorTemplate."
  (cl-assert (stringp dest))
  (cl-assert (listp replacements))
  (cl-assert (SkeletorTemplate-p template))
  (cl-flet ((expand (it)
                    (->> (skel--replace-all replacements it)
                      (s-chop-prefix (SkeletorTemplate-path template))
                      (s-prepend (s-chop-suffix (f-path-separator) dest)))))
    (SkeletorExpansionSpec
     (--map (cons it (expand it)) (SkeletorTemplate-files template))
     (--map (cons it (expand it)) (SkeletorTemplate-dirs template)))))

;; [(String,String)], String -> String
(defun skel--replace-all (replacements str)
  "Expand REPLACEMENTS in STR with fixed case.
Like `s-replace-all' but preserves case of the case of the
replacement."
  (replace-regexp-in-string (regexp-opt (-map 'car replacements))
                            (lambda (it)
                              (cdr (assoc it replacements)))
                            str 'fixcase))

(defun skel--validate-replacements (alist)
  "Assert that ALIST will be accepted by `s-replace-all'."
  (cl-assert (listp alist))
  (cl-assert (--all? (stringp (car it)) alist))
  (cl-assert (--all? (stringp (cdr it)) alist)))

;; [(String,String)], SkeletorExpansionSpec -> IO ()
(defun skel--instantiate-spec (replacements spec)
  "Create an instance of the given template specification.

* REPLACEMENTS is an alist as accepted by `s-replace-all'.

* SPEC is a SkeletorExpansionSpec."
  (skel--validate-replacements replacements)
  (cl-assert (SkeletorExpansionSpec-p spec))
  (--each (-map 'cdr (SkeletorExpansionSpec-dirs spec))
    (make-directory it t))
  (--each (SkeletorExpansionSpec-files spec)
    (message "%s" it)
    (cl-destructuring-bind (src . dest) it
      (f-touch dest)
      (f-write (skel--replace-all replacements (f-read src))
               'utf-8 dest))))

;; [(String,String)], FilePath, FilePath -> IO ()
(defun skel--instantiate-skeleton-dir (replacements src dest)
  "Create an instance of a project skeleton.

* REPLACEMENTS is an alist as accepted by `s-replace-all'.

* SRC is the path to the template directory.

* DEST is the destination path for the template."
  (skel--validate-replacements replacements)
  (cl-assert (stringp src))
  (cl-assert (f-exists? src))
  (cl-assert (stringp dest))
  (make-directory dest t)
  (->> (skel--dir->SkeletorTemplate src)
    (skel--expand-template-paths replacements dest)
    (skel--instantiate-spec replacements)))

;; FilePath -> IO ()
(defun skel--initialize-git-repo  (dir)
  "Initialise a new git repository at DIR."
  (when skel-init-with-git
    (message "Initialising git...")
    (skel-shell-command
     dir "git init && git add -A && git commit -m 'Initial commit'")
    (message "Initialising git...done")))

;; FilePath, FilePath, [(String,String)] -> IO ()
(defun skel--instantiate-license-file (license-file dest replacements)
  "Populate the given license file template.
* LICENSE-FILE is the path to the template license file.

* DEST is the path it will be copied to.

* REPLACEMENTS is an alist passed to `skel--replace-all'."
  (f-write (skel--replace-all replacements (f-read license-file)) 'utf-8 dest))

;;; ---------------------- User Interface Commands -----------------------------

;; (String,String) -> IO (String,String)
(cl-defun skel--eval-replacement ((token . repl))
  "Convert a replacement item according to the following rules:

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

;; String, Regex -> IO FilePath
(defun skel--read-license (prompt default)
  "Prompt the user to select a license.

* PROMPT is the prompt shown to the user.

* DEFAULT a regular expression used to find the default."
  (let* ((xs (--map (cons (s-upcase (f-filename it)) it)
                    (f-files skel--licenses-directory)))
         (d (unless (s-blank? default)
              (car (--first (s-matches? default (car it)) xs))))
         (choice (ido-completing-read prompt (-map 'car xs) nil t d)))
    (cdr (assoc choice xs))))

;; {String} -> IO String
(cl-defun skel--read-project-name (&optional (prompt "Project Name: "))
  "Read a project name from the user."
  (let* ((name (read-string prompt))
         (dest (f-join skel-project-directory name)))
    (cond
     ((s-blank? name)
      (skel--read-project-name))
     ((f-exists? dest)
      (skel--read-project-name
       (format "%s already exists. Choose a different name: " dest)))
     (t
      name))))

;;; Public user commands

;;;###autoload
(cl-defmacro define-project-skeleton (name
                                      &key
                                      replacements
                                      (after-creation 'ignore)
                                      default-license
                                      (license-file-name "COPYING"))
  "Declare a new project type.

* NAME is a string naming the project type. A corresponding
  skeleton should exist in `skel--directory' or
  `skel-user-directory'.

* REPLACEMENTS is an alist of (string . replacement) used specify
  substitutions when initialising the project from its skeleton.

* DEFAULT-LICENSE is a regexp matching the name of a license to
  be used as the default when reading from the user.

* AFTER-CREATION is a unary function to be run once the project
  is created. It should take a single argument--the path to the
  newly-created project."
  (declare (indent 1))
  (cl-assert (or (symbolp name) (stringp name)) t)
  (cl-assert (stringp license-file-name) t)
  (let ((constructor (intern (format "skel--create-%s" name)))
        (default-license-var (intern (format "%s-default-license" name)))
        (rs (eval replacements)))
    (cl-assert (listp rs) t)
    (cl-assert (-all? 'stringp (-map 'car rs)) t)

    `(progn
       (defvar ,default-license-var ,default-license
         ,(concat "Auto-generated variable.\n\n"
                  "The default license type for " name " skeletons.") )
       ;; Update the variable if the definition is re-evaluated.
       (setq ,default-license-var ,default-license)

       (defun ,constructor (project-name license-file)

         ,(concat
           "Auto-generated function.\n\n"
           "Interactively creates a new " name " skeleton.\n"
           "
* PROJECT-NAME is the name of this project instance.

* LICENSE-FILE is the path to a license file to be added to the project.")

         (interactive
          (list (skel--read-project-name)
                (skel--read-license "License: " (eval ,default-license-var))))

         (let* ((dest (f-join skel-project-directory project-name))
                (default-directory dest)
                (repls (-map 'skel--eval-replacement
                             (-concat
                              skel-global-replacements
                              (list (cons "__PROJECT-NAME__" project-name)
                                    (cons "__LICENSE-FILE-NAME__" ,license-file-name))
                              ',rs))))

           ;; Instantiate the project.

           (-if-let (skeleton (-first 'f-exists?
                                      (list (f-expand ,name skel-user-directory)
                                            (f-expand ,name skel--directory))))
               (progn
                 (unless (f-exists? skel-project-directory)
                   (make-directory skel-project-directory t))
                 (skel--instantiate-skeleton-dir repls skeleton dest)
                 (skel--instantiate-license-file
                  license-file (f-join dest ,license-file-name) repls))

             (error "Skeleton %s not found" ,name))

           (save-window-excursion
             (funcall #',after-creation dest)
             (skel--initialize-git-repo dest)
             (run-hook-with-args 'skel-after-project-instantiated-hook default-directory))

           (when skel-show-project-command
             (funcall skel-show-project-command dest))

           (message "Project created at %s" dest)))

       (add-to-list 'skel--project-skeletons (cons ,name ',constructor)))))

;;;###autoload
(defun create-project (type)
  "Interactively create a new project with Skeletor.
TYPE is the name of an existing project skeleton."
  (interactive
   (list (completing-read "Skeleton: "
                          (-sort 'string< (-map 'car skel--project-skeletons))
                          nil t)))
  (let ((constructor (cdr (assoc type skel--project-skeletons))))
    (call-interactively constructor)))

;;; ------------------------ Built-in skeletons --------------------------------

(define-project-skeleton "elisp-package"
  :default-license (rx bol "gpl")
  :replacements
  '(("__DESCRIPTION__"
     . (lambda ()
         (read-string "Description: "))))
  :after-creation
  (lambda (dir)
    (skel-async-shell-command dir "make env")))

(defun skel-py--read-python-bin ()
  "Read a python binary from the user."
  (->> skel-python-bin-search-path
    (--mapcat
     (f-files it (lambda (f)
                   (s-matches? (rx "python" (* (any digit "." "-")) eol)
                               f))))
    (ido-completing-read "Python binary: ")))

(defun skel-py--create-virtualenv-dirlocals (dir)
  "Create a .dir-locals file in DIR for virtualenv variables."
  (save-excursion
    (add-dir-local-variable nil 'virtualenv-default-directory dir)
    (add-dir-local-variable nil 'virtualenv-workon (f-filename dir))
    (save-buffer)
    (kill-buffer)))

(define-project-skeleton "python-library"
  :replacements '(("__PYTHON-BIN__" . skel-py--read-python-bin))
  :after-creation
  (lambda (dir)
    (message "Finding python binaries...")
    (skel-py--create-virtualenv-dirlocals dir)
    (skel-async-shell-command dir "make tooling")))

(defvar skel-hs--haskell-categories
  (list "Codec" "Concurrency" "Control" "Data" "Database" "Development"
        "Distribution" "Game" "Graphics" "Language" "Math" "Network"
        "Sound" "System" "Testing" "Text" "Web")
  "List of Haskell project categories.")

(defvar skel-hs--haskell-language-versions
  (list "Haskell2010" "Haskell98")
  "List of Haskell language versions.")

(defun skel-hs--cabal-sandboxes-supported? ()
  "Non-nil if the installed cabal version supports sandboxes.
Sandboxes were introduced in cabal 1.18 ."
  (let ((vers (->> (shell-command-to-string "cabal --version")
                (s-match (rx (+ (any num "."))))
                car
                (s-split (rx "."))
                (-map 'string-to-int))))
    (cl-destructuring-bind (maj min &rest rest) vers
      (or (< 1 maj) (<= 18 min)))))

(define-project-skeleton "haskell-executable"
  :license-file-name "LICENSE"
  :replacements
  '(("__SYNOPSIS__"
     . (lambda ()
         (read-string "Synopsis: ")))
    ("__HASKELL-LANGUAGE-VERSION__"
     . (lambda ()
         (ido-completing-read "Language: " skel-hs--haskell-language-versions)))
    ("__PROJECT-CATEGORY__"
     . (lambda ()
         (ido-completing-read "Category: " skel-hs--haskell-categories))))
  :after-creation
  (lambda (dir)
    (when (skel-hs--cabal-sandboxes-supported?)
      (message "Initialising sandbox...")
      (skel-async-shell-command dir "cabal sandbox init"))))

(provide 'skeletor)

;;; skeletor.el ends here

;;  LocalWords:  SkeletorTemplate SkeletorDirectory SkeletorExpansionSpec STR
;;  LocalWords:  DEST skeletor
