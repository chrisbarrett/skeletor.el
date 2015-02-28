;;; skeletor-tests.el --- Tests for skeletor.el

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 0.1

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

;; Tests for skeletor.el

;;; Code:

(eval-and-compile
  ;; Add project root to flycheck checker load path to prevent spurious warnings.
  (when (boundp 'flycheck-emacs-lisp-load-path)
    (dolist (it (file-expand-wildcards "../.cask/*/elpa/*"))
      (add-to-list 'flycheck-emacs-lisp-load-path it))
    (add-to-list 'flycheck-emacs-lisp-load-path (expand-file-name "../"))))

(require 'ert)
(require 'undercover)

(let ((undercover-force-coverage t))
  (undercover "skeletor.el"))

(require 'skeletor)

(defvar this-dir (f-dirname (or load-file-name (buffer-file-name))))

(defvar template-name   "example-project")

(defvar template-path   (f-join this-dir template-name))

(defvar license-name "MIT")

;; Predeclare variables to prevent warnings.

(defvar destination-path)
(defvar template-instance)
(defvar spec-instance)
(defvar test-substitutions)
(defvar test-token)
(defvar project-name)



;; Regenerate random vars each test run.

(setq destination-path  (make-temp-file "skeletor-test_" t)

      template-instance (skeletor--dir->SkeletorTemplate template-path)

      test-token (concat "MiXcAsE" (md5 (number-to-string (random))))

      project-name (md5 (number-to-string (random)))

      test-substitutions `(("__PROJECT-NAME__" . ,project-name)
                           ("__TOKEN__"        . ,test-token))

      spec-instance     (skeletor--expand-template-paths test-substitutions
                                                         destination-path
                                                         template-instance))

;;; Utility functions

(defun -sets-equal? (xs ys)
  "Return non-nil if XS and YS are identical sets.
Elements are compared using `equal'."
  (not (or (-difference xs ys) (-difference ys xs))))

(defun -alist? (form)
  (and (listp form) (--all? (consp it) form)))

;;; Template parsing and transformations

(ert-deftest skeletor-template-root-has-correct-dir ()
  (should (equal template-path (SkeletorTemplate-path template-instance))))

(ert-deftest skeletor-template-has-all-directories ()
  (should (-sets-equal? (f-directories template-path nil t)
                        (SkeletorTemplate-dirs template-instance))))

(ert-deftest skeletor-template-has-all-files ()
  (should (-sets-equal? (f-files template-path nil t)
                        (SkeletorTemplate-files template-instance))))

(ert-deftest original-dirnames-in-expanded-template ()
  (should (-sets-equal? (SkeletorTemplate-dirs template-instance)
                        (-map 'car (SkeletorExpansionSpec-dirs spec-instance)))))

(ert-deftest original-filenames-in-expanded-template ()
  (should (-sets-equal? (SkeletorTemplate-files template-instance)
                        (-map 'car (SkeletorExpansionSpec-files spec-instance)))))

(ert-deftest expands-tokens-in-dirnames-when-expanding-template ()
  (should (--only-some? (s-matches? project-name (f-filename (cdr it)))
                        (SkeletorExpansionSpec-dirs spec-instance))))

(ert-deftest expands-tokens-in-filenames-when-expanding-template ()
  (should (--only-some? (s-matches? project-name (f-filename (cdr it)))
                        (SkeletorExpansionSpec-files spec-instance))))

(ert-deftest instance-starts-with-project-name--not-template-name ()
  (should (--none? (s-ends-with? (f-slash project-name) (f-dirname (cdr it)))
                   (-concat (SkeletorExpansionSpec-files spec-instance)
                            (SkeletorExpansionSpec-dirs spec-instance)))))

(ert-deftest spec-output-files-are-relative-to-dest ()
  (should (--all? (s-starts-with? destination-path (cdr it))
                  (SkeletorExpansionSpec-files spec-instance))))

(ert-deftest spec-output-directories-are-relative-to-dest ()
  (should (--all? (s-starts-with? destination-path (cdr it))
                  (SkeletorExpansionSpec-dirs spec-instance))))

(ert-deftest expands-tokens-in-files-with-fixed-case ()
  (let* ((token "__REPL__")
         (expected "test TEST tEsT")
         (substitutions (list (cons token expected))))
    (should (equal expected (skeletor--replace-all substitutions token)))))

(ert-deftest substitutions-are-idempotent-when-no-tokens-in-alist ()
  (let ((input (symbol-name (cl-gensym))))
    (should (equal input (skeletor--replace-all nil input)))))

(ert-deftest evaluates-embedded-elisp-in-file-templates ()
  (let ((x (random 100))
        (y (random 100)))
    (should (equal (* x y) (read (skeletor--replace-all nil "__(* x y)__"))))))

;;; Integration tests

(skeletor-define-template "example-project"
  :substitutions `(("__TOKEN__" . ,test-token)))

(let ((skeletor--read-project-name-fn (lambda (&rest _) project-name))
      (skeletor--read-license-fn (lambda (&rest _) license-name))
      (skeletor--read-project-type-fn (lambda (&rest _) "example-project"))

      (skeletor-user-directory this-dir)
      )
  (skeletor-create-project-at destination-path (car skeletor--project-types))
  )

(ert-deftest instantiates-all-files-in-template ()
  (let ((generated-files '("COPYING")))
    (should (equal (length (-concat generated-files (f-files template-path nil t)))
                   (length (f-files destination-path
                                    (lambda (it) (not (s-matches? (rx ".git/") it)))
                                    t))))))

(ert-deftest instantiates-all-dirs-in-template ()
  (should
   (equal (length (f-directories template-path nil t))
          (length (--remove (equal project-name (f-filename it))
                            (f-directories destination-path
                                           (lambda (it) (not (s-matches? (rx ".git") it)))
                                           t))))))

(ert-deftest initialises-git-repo ()
  (should (-contains? (-map 'f-filename (f-directories destination-path nil t)) ".git")))

(ert-deftest expands-tokens-in-instantiated-files ()
  (should (--only-some? (s-contains? test-token (f-read it))
                        (f-files destination-path nil t))))

(ert-deftest expands-file-name-tokens-in-instantiated-project ()
  (should (--only-some? (s-starts-with? project-name (f-filename it))
                        (f-files destination-path nil t))))

(ert-deftest expands-directory-name-tokens-in-instantiated-project ()
  (should (--only-some? (s-starts-with? project-name (f-filename it))
                        (f-directories destination-path nil t))))

(ert-deftest transforms-gitignore-in-template-to-dotgitignore ()
  (should (-contains? (-map 'f-filename (f-files destination-path nil t))
                      ".gitignore")))

(ert-deftest sets-project-root-global-variable ()
  (should (f-equal? (f-join destination-path project-name) skeletor-project-root)))

(ert-deftest sets-project-name-global-variable ()
  (should (equal project-name skeletor-project-name)))

(ert-deftest sets-license-name-global-variable ()
  (should (s-matches? license-name skeletor-project-license)))

(ert-deftest sets-spec-global-variable ()
  (should skeletor-project-spec)
  (should (-alist? skeletor-project-spec)))

(provide 'skeletor-tests)

;;; skeletor-tests.el ends here
