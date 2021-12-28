;; ref
;; http://alexott.net/en/writings/emacs-devenv/EmacsScheme.html
(autoload 'run-scheme "cmuscheme" "Run an inferior Scheme" t)
(setq scheme-program-name "/usr/local/bin/mit-scheme") ;;; This is the binary name of my scheme implementation

;; https://medium.com/@joshfeltonm/setting-up-emacs-for-sicp-from-scratch-daa6473885c5
(setq geiser-mit-binary "/usr/local/bin/scheme")  ;;; https://github.com/emacsmirror/geiser
(setq geiser-active-implementations '(mit))

;; ref
;; http://community.schemewiki.org/?emacs-tutorial
;;; Also highlight parens
(global-font-lock-mode 1)
;;; Also highlight parens
(setq show-paren-delay 0
      show-paren-style 'parenthesis)
(show-paren-mode 1)

;; 更改显示字体大小 16pt
;; http://stackoverflow.com/questions/294664/how-to-set-the-font-size-in-emacs
;; (set-face-attribute 'default nil :height 160)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq default-directory "~")

