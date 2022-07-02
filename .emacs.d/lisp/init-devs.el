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

;; blamer, https://github.com/Artawower/blamer.el
(leaf blamer
  :straight t
  :config
  ;;  (global-blamer-mode 1)
  )

;; https://github.com/Fuco1/smartparens
;; M-x sp-cheat-sheet, will show you all commands available
(leaf smartparens
  :straight t
  :hook ((text-mode-hook prog-mode-hook) . smartparens-mode)
  :config
  (require 'smartparens-config))

;; flycheck instead
;; (leaf flymake
;;   :require t
;;   :straight nil
;;   :hook
;;   ((text-mode-hook prog-mode-hook) . flymake-mode)
;;   :bind
;;   ("C-c f" . flymake-show-buffer-diagnostics)
;;   ("C-c [" . flymake-goto-prev-error)
;;   ("C-c ]" . flymake-goto-next-error))

;; flycheck prefix with `C-c !`
;; https://www.flycheck.org/en/latest/user/quickstart.html
(leaf flycheck
  :straight t
  :config (global-flycheck-mode))

;; company
(leaf company
  :straight t
  :hook
  ((text-mode-hook prog-mode-hook) . company-mode)
  )

(setq-default tab-width 4
			  indent-tabs-mode 1)

;; a client for LSP, https://github.com/joaotavora/eglot
;; (leaf eglot
;;   :after flymake
;;   :straight t
;;   :require t
;;   :hook((rust-mode-hook tuareg-mode-hook
;; 			haskell-literate-mode-hook haskell-mode-hook
;; 			c-mode-hook c++-mode-hook go-mode-hook) . eglot-ensure)
;;   :bind
;;   ("C-c =" . eglot-format)
;;   ("C-c -" . eglot-rename)
;;   ("C-c 0" . eglot-code-actions)
;;   ("C-c h" . egdoc)
;;   :config
;;   (defun eglot-format-buffer-on-save ()
;;     (add-hook 'before-save-hook #'eglot-format-buffer -10 t))
;;   (add-hook 'go-mode-hook #'eglot-format-buffer-on-save)
;;   (setq-default eglot-workspace-configuration
;; 		'((:gopls .
;; 			  ((staticcheck . t)
;; 			   (matcher . "CaseSensitive"))))))

;;; Program
;; lsp-ui contains highlevel UI modules of lsp-mode works out
;; of box
(leaf lsp-ui
  :straight t)

;; lsp-mode, https://github.com/emacs-lsp/lsp-mode
(leaf lsp-mode
  :straight t
  :hook
  ((go-mode-hook c-mode-hook c++-mode-hook) . lsp)
  :config
  (setq lsp-keymap-prefix "C-c l")
  (lsp-register-client ; https://emacs-lsp.github.io/lsp-mode/page/remote/
   (make-lsp-client :new-connection (lsp-tramp-connection "clang-14")
                    :major-modes '(c-mode)
                    :remote? t
                    :server-id 'clang-14-remote))
  ;; clang: error: no input filesr

  ;; solve, too many files open
  (if (not (fboundp 'file-notify-rm-all-watches))
	  (defun file-notify-rm-all-watches ()
		"Remove all existing file notification watches from Emacs."
		(interactive)
		(maphash
		 (lambda (key _value)
		   (file-notify-rm-watch key))
		 file-notify-descriptors)))
  )

(leaf go-mode ;; TODO: try to move this into eglog config part
  :straight t
  :config
  (autoload 'go-mode "go-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
  (defun my-go-mode-hook ()
	;; use goimports instead of go-fmt
	(setq gofmt-command "goimports")
	;; call Gofmt before saving
	(add-hook 'before-save-hook 'gofmt-before-save)
	;; guru too slow, and failed to set its scope
	;; (go-guru-hl-identifier-mode) ;; C-c C-o go-guru-map
	)
  (add-hook 'go-mode-hook #'my-go-mode-hook))

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
  :hook
  (after-init-hook . global-tree-sitter-mode)
  (tree-sitter-after-on-hook . tree-sitter-hl-mode))

;; tree-sitter-langs populate tree-sitter-major-mode-language-alist 
(leaf tree-sitter-langs
  :straight t)

;; tree-site-cores
(leaf tsc
  :straight t)

;;; Project
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

;; treemacs, https://github.com/Alexander-Miller/treemacs#installation
(leaf treemacs
  :straight t
  :bind
  ("M-0" . treemacs-select-window)
  ("C-c t 1"   . treemacs-delete-other-windows)
  ("C-c t t"   . treemacs)
  ("C-c t d"   . treemacs-select-directory)
  ("C-c t B"   . treemacs-bookmark)
  ("C-c t C-t" . treemacs-find-file)
  ("C-c t M-t" . treemacs-find-tag)
  )

;; json major mode, https://github.com/joshwnj/json-mode
(leaf json-mode
  :straight t
  )

;; yaml major mode, https://github.com/yoshiki/yaml-mode
(leaf yaml-mode
  :straight t
  :config
  )

(provide 'init-devs)
;;; init-devs.el ends here
