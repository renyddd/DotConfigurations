;;; init-lang.el

(leaf geiser-mit
  :straight t
  :config
  ;; https://medium.com/@joshfeltonm/setting-up-emacs-for-sicp-from-scratch-daa6473885c5
  (setq geiser-mit-binary "/usr/local/bin/scheme") ;;; https://github.com/emacsmirror/geiser
  (setq geiser-active-implementations '(mit))
  )

;; http://alexott.net/en/writings/emacs-devenv/EmacsScheme.html
(autoload 'run-scheme "cmuscheme" "Run an inferior Scheme" t)
(setq scheme-program-name "/usr/local/bin/mit-scheme") ;;; This is the binary name of my scheme implementation

;; https://www.gnu.org/software/emacs/manual/html_mono/ccmode.html
(leaf c-mode
  :config
  ;; c-default-style, c-set-style
  )

(provide 'init-lang)
;;; init-lang.el ends here
