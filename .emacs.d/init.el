;;; init.el --- initialize all emacs settings

;;; Commentary:
;; The startup file
;; Emacs config by Erupmi :P
;; Special thanks to seagle0128, purcell, and doom-emacs

(setq debug-on-error t)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; initialize constants and helper functions
(require 'init-const)
(require 'init-conf)
(require 'init-note)
(require 'init-show)

;;; init.el ends here
