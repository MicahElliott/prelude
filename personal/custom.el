(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(browse-url-browser-function (quote browse-url-firefox))
 '(custom-safe-themes
   (quote
    ("708df3cbb25425ccbf077a6e6f014dc3588faba968c90b74097d11177b711ad1" default)))
 '(fci-rule-color "#383838")
 '(global-yascroll-bar-mode t)
 '(inhibit-startup-screen nil)
 '(markdown-header-scaling t)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages
   (quote
    (sotclojure monroe docker dockerfile-mode docker-mode intero protobuf-mode pkgbuild-mode typo typopunct centered-cursor-mode speed-type sublimity-scroll yascroll smart-mode-line-powerline-theme dashboard paradox elfeed crosshairs hiwin monokai-theme flymake-puppet puppet-mode wrap-region focus zlc bash-completion ansible-doc ansible idle-highlight-mode f sx prelude-helm-everywhere dired+ langtool pretty-mode clojure-mode-extra-font-locking jump-char flycheck-tip flycheck-pos-tip kibit-helper slamhound helm-clojuredocs flycheck-clojure col-highlight origami hideshowvis ghc flymake-hlint haskell-mode corral flymake-coffee coffee-mode all-the-icons neotree zop-to-char zenburn-theme yari yaml-mode which-key web-mode volatile-highlights visible-mark utop unfill undo-tree tuareg toggle-quotes tangotango-theme smex smartscan smartrep smartparens smart-mode-line shrink-whitespace show-marks ruby-tools rubocop rinari relative-line-numbers rainbow-mode rainbow-delimiters ov org-journal operate-on-number multi-term multi-eshell move-text markdown-mode mark-tools magit livescript-mode linum-relative key-chord json-mode imenu-anywhere ido-ubiquitous helm-projectile helm-descbinds helm-cider helm-ag haml-mode guru-mode guide-key grizzl god-mode gitignore-mode github-issues gitconfig-mode git-timemachine gist geiser flycheck-ocaml flx-ido floobits expand-region exec-path-from-shell esh-help elisp-slime-nav easy-kill dot-mode discover-my-major discover-clj-refactor diminish diff-hl cycle-quotes csv-mode crux crontab-mode company-quickhelp company-ansible comment-dwim-2 clojure-snippets clojure-cheatsheet cljr-helm clippy chruby buffer-move browse-kill-ring bm beacon anzu adoc-mode ace-window ac-cider)))
 '(paradox-automatically-star t)
 '(paradox-github-token t)
 '(scroll-bar-mode nil)
 '(search-whitespace-regexp "\"[ \\t\\r\\n]+\"")
 '(standard-indent 2)
 '(text-scale-mode-step 1.1)
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(wrap-region-global-mode t nil (wrap-region))
 '(yascroll:delay-to-hide nil)
 '(yascroll:scroll-bar (quote (right-fringe left-fringe text-area))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#272822" :foreground "#F8F8F2" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 80 :width normal :foundry "PfEd" :family "DejaVu Sans Mono"))))
 '(ansible::task-label-face ((t (:foreground "gold" :weight bold :height 1.4 :family "DejaVu Sans"))))
 '(cider-debug-code-overlay-face ((t (:background "gray50"))))
 '(col-highlight ((t (:background "gray24"))))
 '(cursor ((t (:background "red" :foreground "#272822" :inverse-video t))))
 '(easy-kill-selection ((t (:background "medium slate blue"))))
 '(font-lock-comment-face ((t (:foreground "#75715E" :slant italic :weight bold :width condensed :family "dejavu sans"))))
 '(font-lock-doc-face ((t (:foreground "dodger blue" :height 1.1 :family "DejaVu Sans"))))
 '(font-lock-string-face ((t (:foreground "#E6DB74" :slant italic))))
 '(fringe ((t (:background "gray22" :foreground "#F8F8F2"))))
 '(hiwin-face ((t (:background "gray24"))))
 '(idle-highlight ((t (:inherit region :background "dark blue"))))
 '(isearch ((t (:inherit region :background "#A6E22E" :foreground "dark violet" :weight bold))))
 '(lazy-highlight ((t (:inherit highlight :background "dark magenta" :foreground "chartreuse" :weight bold))))
 '(markdown-header-face ((t (:foreground "#A6E22E" :weight extra-bold))))
 '(markdown-header-rule-face ((t (:inherit markdown-markup-face))))
 '(mode-line ((t (:box (:line-width 2 :color "blue")))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "dark orange" :weight bold))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "deep pink" :weight bold))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "chartreuse" :weight bold))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "deep sky blue" :weight bold))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "yellow" :weight bold))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "orchid" :weight bold))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "spring green" :weight bold))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "sienna1" :weight bold))))
 '(region ((t (:inherit highlight :background "dark violet"))))
 '(visible-mark-face1 ((t (:background "light salmon" :foreground "black"))))
 '(visible-mark-face2 ((t (:background "light goldenrod" :foreground "black"))))
 '(yascroll:thumb-fringe ((t (:background "#75715E" :foreground "dark magenta" :width extra-condensed))))
 '(yascroll:thumb-text-area ((t (:background "#75715E" :foreground "blue")))))
