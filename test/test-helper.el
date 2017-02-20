;; test helper inspired from https://github.com/tonini/overseer.el/blob/master/test/test-helper.el

;;; Code:

(when (require 'undercover nil t)
  (undercover "skeletor.el"))

(require 'skeletor)
(require 'ert)
(require 's)
(require 'dash)
(require 'f)

;;; Test variables

(defvar skeletor-path
  (f-parent (f-this-file)))

(defvar skeletor-test-path
  (f-dirname (f-this-file)))

(defvar skeletor-root-path
  (f-parent skeletor-test-path))

(defvar skeletor-sandbox-path
  (f-expand "sandbox" skeletor-test-path))

(when (f-exists? skeletor-sandbox-path)
  (error "Something is already in %s. Check and destroy it yourself" skeletor-sandbox-path))

(defmacro within-sandbox (&rest body)
  "Evaluate BODY in an empty sandbox directory."
  `(let ((default-directory skeletor-sandbox-path))
     (when (f-exists? skeletor-sandbox-path)
       (f-delete default-directory :force))
     (f-mkdir skeletor-sandbox-path)
     ,@body
     (f-delete default-directory :force)))

(defvar commander-ignore t
  "With this prevent commander to run without asking")

(eval-and-compile
  ;; Add project root to flycheck checker load path to prevent spurious warnings.
  (when (boundp 'flycheck-emacs-lisp-load-path)
    (dolist (it (file-expand-wildcards "../.cask/*/elpa/*"))
      (add-to-list 'flycheck-emacs-lisp-load-path it))
    (add-to-list 'flycheck-emacs-lisp-load-path (expand-file-name "../"))))

;;; Utilities

(defun -sets-equal? (xs ys)
  "Return non-nil if XS and YS are identical sets.
Elements are compared using `equal'."
  (not (or (-difference xs ys) (-difference ys xs))))

(defun -alist? (form)
  (and (listp form) (--all? (consp it) form)))

(defmacro -with-stubbed-user-interaction (&rest body)
  "Execute BODY forms with user input stubbed out."
  (declare (indent 0))
  `(let* ((skeletor--read-project-name-fn (lambda (&rest _) project-name))
          (skeletor--read-license-fn (lambda (&rest _) license-name))
          (skeletor--read-project-type-fn (lambda (&rest _) "example-project"))

          (skeletor-user-directory this-dir))
     ,@body))

;;; test-helper.el ends here
