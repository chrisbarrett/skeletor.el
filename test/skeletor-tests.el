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
  (when (boundp' flycheck-emacs-lisp-load-path)
    (dolist (it (file-expand-wildcards "../.cask/*/elpa/*"))
      (add-to-list 'flycheck-emacs-lisp-load-path it))
    (add-to-list 'flycheck-emacs-lisp-load-path (expand-file-name "../"))))

(require 'ert)
(require 'skeletor)

(defvar this-dir        (f-dirname (or load-file-name (buffer-file-name))))

(defvar template-name   "example-project")

(defvar template-path   (f-join this-dir template-name))

;; Predeclare variables to prevent warnings.

(defvar destination-path)
(defvar template-instance)
(defvar spec-instance)
(defvar test-substitutions)
(defvar test-token)

;; Regenerate random vars each test run.

(setq destination-path  (make-temp-file "skeletor-test_" t)

      template-instance (skeletor--dir->SkeletorTemplate template-path)

      test-token (concat "MiXcAsE" (md5 (number-to-string (random))))

      test-substitutions `(("__PROJECT-NAME__" . ,template-name)
                          ("__TOKEN__"        . ,test-token))

      spec-instance     (skeletor--expand-template-paths test-substitutions
                                                     destination-path
                                                     template-instance))

;;; Utility functions

(defun -sets-equal? (xs ys)
  "Return non-nil if XS and YS are identical sets.
Elements are compared using `equal'."
  (not (or (-difference xs ys) (-difference ys xs))))

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
  (should (--only-some? (s-matches? template-name (f-filename (cdr it)))
                        (SkeletorExpansionSpec-dirs spec-instance))))

(ert-deftest expands-tokens-in-filenames-when-expanding-template ()
  (should (--only-some? (s-matches? template-name (f-filename (cdr it)))
                        (SkeletorExpansionSpec-files spec-instance))))

(ert-deftest instance-starts-with-project-name--not-template-name ()
  (should (--none? (s-ends-with? (f-slash template-name) (f-dirname (cdr it)))
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

(skeletor--instantiate-skeleton-dir test-substitutions template-path destination-path)

(ert-deftest instantiates-all-files-in-template ()
  (should (equal (length (f-files template-path nil t))
                 (length (f-files destination-path nil t)))))

(ert-deftest instantiates-all-dirs-in-template ()
  (should (equal (length (f-directories template-path nil t))
                 (length (f-directories destination-path nil t)))))

(ert-deftest expands-tokens-in-instantiated-files ()
  (should (--only-some? (s-contains? test-token (f-read it))
                        (f-files destination-path nil t))))

(ert-deftest expands-file-name-tokens-in-instantiated-project ()
  (should (--only-some? (s-starts-with? template-name (f-filename it))
                        (f-files destination-path nil t))))

(ert-deftest expands-directory-name-tokens-in-instantiated-project ()
  (should (--only-some? (s-starts-with? template-name (f-filename it))
                        (f-directories destination-path nil t))))

(provide 'skeletor-tests)

;;; skeletor-tests.el ends here
