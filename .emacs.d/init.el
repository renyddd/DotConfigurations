;;; init.el --- initialize all emacs settings

;;; Commentary:
;; The startup file
;; Emacs config by Erupmi :P
;; Special thanks to seagle0128, purcell, and doom-emacs

(setq debug-on-error t)

;; initialize straight, https://github.com/raxod502/straight.el#getting-started
(defvar bootstrap-version)

(setq straight-vc-git-default-clone-depth 1
      straight-check-for-modifications '(find-when-checking)
      straight-use-package-by-default t
      straight-recipes-gnu-elpa-use-mirror t)

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(eval-and-compile
  (straight-use-package 'leaf)
  (straight-use-package 'leaf-keywords)
  (leaf leaf-keywords
    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))

;; hide a minor mode in modeline
(leaf diminish :straight t)

;; profiler, https://github.com/jschaf/esup
(leaf esup
  :straight t
  :commands (esup))

;;
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; initialize constants and helper functions
(require 'init-const)
(require 'init-boot)

;; edit
(require 'init-note)

;; UI
(require 'init-show)

;; integration
(require 'init-devs)
(require 'init-lang)



;;; init.el ends here

