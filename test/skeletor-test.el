;;; skeletor-test.el --- Tests for skeletor.el  -*- lexical-binding: t; -*-

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

(defconst this-dir (f-dirname (or load-file-name (buffer-file-name))))

(defconst template-name   "example-project")

(defconst template-path   (f-join this-dir template-name))

(defconst license-name "MIT")

(defconst destination-path (make-temp-file "skeletor-test_" t))

(defconst template-instance (skeletor--dir->SkeletorTemplate template-path))

(defconst test-token (concat "MiXcAsE" (md5 (number-to-string (random)))))

(defconst project-name (md5 (number-to-string (random))))

(defconst test-substitutions `(("__PROJECT-NAME__" . ,project-name)
                               ("__TOKEN__"        . ,test-token)))

(defconst default-project-name (md5 (number-to-string (random))))

(defconst no-eval-embedded-elisp? nil)

(defconst eval-embedded-elisp? (not no-eval-embedded-elisp?))

(defconst spec-instance (skeletor--expand-template-paths test-substitutions
                                                         destination-path
                                                         eval-embedded-elisp?
                                                         template-instance))

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
    (should (equal expected (skeletor--replace-all substitutions token eval-embedded-elisp?)))))

(ert-deftest substitutions-are-idempotent-when-no-tokens-in-alist ()
  (let ((input (symbol-name (cl-gensym))))
    (should (equal input (skeletor--replace-all nil input eval-embedded-elisp?)))))

(ert-deftest evaluates-embedded-elisp-in-file-templates ()
  ;; Need test variables in dynamic scope here, as let-bound variables are out
  ;; of scope in `skeletor--replace-all'.
  (setq x (random 100))
  (setq y (random 100))
  (should (equal (* x y) (read (skeletor--replace-all nil "__(* x y)__" eval-embedded-elisp?)))))

;;; Integration tests

(skeletor-define-template "example-project"
  :substitutions `(("__TOKEN__" . ,test-token)))

(-with-stubbed-user-interaction
  (skeletor-create-project-at destination-path (car skeletor--project-types)))

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

(provide 'skeletor-test)

;;; skeletor-test.el ends here
