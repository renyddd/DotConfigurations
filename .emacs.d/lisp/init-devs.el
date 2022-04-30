;;; init-devs.el --- development related tools
;;; Commentary:
;; currently includes git, docker, and project management

;;; Code:
(require 'init-const)

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

;; a client for LSP, https://github.com/joaotavora/eglot
(leaf eglot
  :after flymake
  :straight t
  :hook
  ((rust-mode-hook tuareg-mode-hook
    haskell-literate-mode-hook haskell-mode-hook
    c-mode-hook c++-mode-hook) . eglot-ensure)
  :bind
  ("C-c =" . eglot-format)
  ("C-c -" . eglot-rename)
  ("C-c 0" . eglot-code-actions)
  ("C-c h" . egdoc))

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

(provide 'init-devs)
;;; init-devs.el ends here
