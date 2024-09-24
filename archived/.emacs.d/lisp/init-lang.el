;;; init-lang.el --- languange related configurations
;;; Commentary:
;; Language configurations.

;;; Code:

(leaf geiser-mit
  :straight t
  :config
  ;; https://medium.com/@joshfeltonm/setting-up-emacs-for-sicp-from-scratch-daa6473885c5
  (setq geiser-mit-binary "/usr/local/bin/scheme") ;;; https://github.com/emacsmirror/geiser
  (setq geiser-active-implementations '(mit))

  ;; http://alexott.net/en/writings/emacs-devenv/EmacsScheme.html
  (autoload 'run-scheme "cmuscheme" "Run an inferior Scheme" t)
  (setq scheme-program-name "/usr/local/bin/mit-scheme") ;;; This is the binary name of my scheme implementation
  )

;; elisp
(leaf elisp
  :config
  ;; better *help* buffer, https://github.com/Wilfred/helpful
  ;; with source code, pretty doc, callers
  (leaf helpful
	:straight t
	:bind (("C-h f" . helpful-callable)
		   ("C-h v" . helpful-variable)
		   ("C-h k" . helpful-key)
		   ("C-h p" . helpful-at-point))
	:config
	(leaf elisp-demos
	  :straight t
	  :config
	  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)
	  )
	)

  ;; intelligent code search for elisp
  ;; https://github.com/Wilfred/elisp-refs
  (leaf elisp-refs
	:straight t
	:bind (("C-c e f" . elisp-refs-function)
		   ("C-c e m" . elisp-refs-macro)
		   ("C-c e v" . elisp-refs-variable)
		   ("C-c e s" . elsip-refs-symbol))
	)

;;; M-x shortdoc-display-group
  )

;; https://www.gnu.org/software/emacs/manual/html_mono/ccmode.html
(leaf c-mode
  :config
  ;; c-default-style, c-set-style
  )

;; https://github.com/ppareit/graphviz-dot-mode
(leaf graphviz-dot-mode
  :straight t
  :config
  (setq graphviz-dot-indent-width 4)
  ;; Dot Language: https://graphviz.org/doc/info/lang.html
  )

(provide 'init-lang)
;;; init-lang.el ends here
