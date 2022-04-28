(global-font-lock-mode 1)

;; disable the menu bar and the scroll bar
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Also highlight parens
(setq show-paren-delay 0
      show-paren-style 'parenthesis)
(show-paren-mode 1)

;; http://stackoverflow.com/questions/294664/how-to-set-the-font-size-in-emacs
(set-face-attribute 'default nil :height 180)

(add-to-list 'default-frame-alist '(fullscreen . maximized))


(provide 'init-show)
;;; init-show.el ends here
