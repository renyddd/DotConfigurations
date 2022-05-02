;;; init-devs.el --- development related tools
;;; Commentary:
;; currently includes git, docker, and project management

;;; Code:
(require 'init-const)

;; Use $PATH set up shell for OS X, to avoid command not found
;; https://github.com/purcell/exec-path-from-shell
(leaf exec-path-from-shell
  :straight t
  :config
  (when (memq window-system '(mac ns x))
    ;; initialize faster, set full list of vars you want
    ;; set to start a non-interactive login shell
    ;; https://github.com/purcell/exec-path-from-shell#setting-up-your-shell-startup-files-correctly
    (setq exec-path-from-shell-arguments nil)
    (exec-path-from-shell-copy-envs '(
				      "GOPATH" "GOROOT"
				      ))
    (exec-path-from-shell-initialize)))

;; use magit for git VC
(leaf magit
  :straight t
  :bind
  ("C-x g" . magit-status)
  ("C-x M-g" . magit-dispatch)
  ("C-c M-g" . magit-file-popup)
  :mode
  ("\\COMMIT_EDITMSG\\'" . text-mode)
  ("\\MERGE_MSG\\'" . text-mode)
  :config
  ;; access git forges from magit
  (when (executable-find "sqlite")
    (leaf forge :straight t)))

;; https://github.com/Fuco1/smartparens
;; M-x sp-cheat-sheet, will show you all commands available
(leaf smartparens
  :straight t
  :hook ((text-mode-hook prog-mode-hook) . smartparens-mode)
  :config
  (require 'smartparens-config))

(leaf flymake
  :require t
  :straight nil
  :hook
  ((text-mode-hook prog-mode-hook) . flymake-mode)
  :bind
  ("C-c f" . flymake-show-buffer-diagnostics)
  ("C-c [" . flymake-goto-prev-error)
  ("C-c ]" . flymake-goto-next-error))

;; company
(leaf company
  :straight t
  :hook
  ((text-mode-hook prog-mode-hook) . company-mode)
  )

;; a client for LSP, https://github.com/joaotavora/eglot
(leaf eglot
  :after flymake
  :straight t
  :require t
  :hook((rust-mode-hook tuareg-mode-hook
			haskell-literate-mode-hook haskell-mode-hook
			c-mode-hook c++-mode-hook go-mode-hook) . eglot-ensure)
  :bind
  ("C-c =" . eglot-format)
  ("C-c -" . eglot-rename)
  ("C-c 0" . eglot-code-actions)
  ("C-c h" . egdoc)
  :config
  (setq tab-width 4)
  (setq indent-tabs-mode 1)
  (defun eglot-format-buffer-on-save ()
    (add-hook 'before-save-hook #'eglot-format-buffer -10 t))
  (add-hook 'go-mode-hook #'eglot-format-buffer-on-save)
  (setq-default eglot-workspace-configuration
		'((:gopls .
			  ((staticcheck . t)
			   (matcher . "CaseSensitive"))))))


(leaf go-mode ;; TODO: try to move this into eglog config part
    :straight t
    :config
    (autoload 'go-mode "go-mode" nil t)
    (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode)))

;; minor mod, information for Lisp objects at point
(leaf eldoc
  :diminish t
  )

;; childframe doc for eldoc, https://github.com/casouri/eldoc-box
(leaf eldoc-box
  :straight t
  :diminish t
  :hook (eldoc-mode-hook . eldoc-box-hover-at-point-mode))

;; https://github.com/emacs-tree-sitter/elisp-tree-sitter
;; https://tree-sitter.github.io/tree-sitter/
(leaf tree-sitter
  :straight t
  :config
  (leaf tree-sitter-langs)
  :hook
  (after-init-hook . global-tree-sitter-mode)
  (tree-sitter-after-on-hook . tree-sitter-hl-mode))

;; project
(leaf project
  :config
  (defun project-find-go-module (dir)
    (when-let ((root (locate-dominating-file dir "go.mod")))
      (cons 'go-module root)))
  (cl-defmethod project-root ((project (head go-module)))
    (cdr project))
  (add-hook 'project-find-functions #'project-find-go-module))

;; projectile, https://github.com/bbatsov/projectile
(leaf projectile
  :straight t
  :bind
  (("C-c p" . projectile-command-map))
  :config
  (projectile-mode +1)
  )

(provide 'init-devs)
;;; init-devs.el ends here
