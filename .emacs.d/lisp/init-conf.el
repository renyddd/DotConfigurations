;; https://mirrors.tuna.tsinghua.edu.cn/help/elpa/
(setq package-archives '(("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
;; Add melpa package source when using package list
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)


;; https://github.com/wakatime/wakatime-mode
;; (global-wakatime-mode t)

;; go-mode failed in kubernetes repo
(setq max-lisp-eval-depth 4000)


(provide 'init-conf)
;;; init-conf.el ends here
