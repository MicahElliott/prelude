(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(avy-case-fold-search nil)
 '(bookmark-default-file "/home/mde/.emacs.d/savefile/bookmarks")
 '(browse-url-browser-function (quote browse-url-firefox))
 '(cljr-auto-sort-ns t)
 '(cljr-warn-on-eval nil)
 '(custom-safe-themes
   (quote
    ("708df3cbb25425ccbf077a6e6f014dc3588faba968c90b74097d11177b711ad1" default)))
 '(fci-rule-color "#383838")
 '(global-superword-mode t)
 '(global-yascroll-bar-mode t)
 '(helm-ag-ignore-patterns (quote ("vendor/")))
 '(helm-ag-use-agignore t)
 '(helm-ff-search-library-in-sexp t)
 '(hl-paren-colors (quote ("red" "IndianRed1" "IndianRed3" "IndianRed4")))
 '(hl-paren-delay 0)
 '(ido-default-file-method (quote selected-window))
 '(inhibit-startup-screen nil)
 '(magit-log-arguments (quote ("--graph" "--color" "--decorate" "--stat" "-n256")))
 '(markdown-header-scaling t)
 '(markdown-wiki-link-search-subdirectories t)
 '(neo-theme (quote icons))
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(org-agenda-files (quote ("~/proj/woodpecker/preso/osb.org")))
 '(package-selected-packages
   (quote
    (emojify emoji-cheat-sheet-plus emoji-cheat-sheet company-emoji ac-emoji flycheck-status-emoji flymd company-flx string-inflection highlight-parentheses clj-refactor pos-tip multiple-cursors parinfer counsel cucumber-goto-step feature-mode ac-octave nginx-mode rjsx-mode inf-clojure flycheck-joker highlight-numbers company-jedi elpy jedi fontawesome org-bullets org-tree-slide epresent htmlize ox-reveal clj-refacort command-log-mode smart-comment hide-comnt fic-mode sotclojure monroe docker dockerfile-mode docker-mode intero protobuf-mode pkgbuild-mode typo typopunct centered-cursor-mode speed-type sublimity-scroll yascroll smart-mode-line-powerline-theme dashboard paradox elfeed crosshairs hiwin monokai-theme flymake-puppet puppet-mode wrap-region focus zlc bash-completion ansible-doc ansible idle-highlight-mode f sx prelude-helm-everywhere dired+ langtool pretty-mode clojure-mode-extra-font-locking jump-char flycheck-tip flycheck-pos-tip kibit-helper slamhound helm-clojuredocs flycheck-clojure col-highlight origami hideshowvis ghc flymake-hlint haskell-mode corral flymake-coffee coffee-mode all-the-icons neotree zop-to-char zenburn-theme yari yaml-mode which-key web-mode volatile-highlights visible-mark utop unfill undo-tree tuareg toggle-quotes tangotango-theme smex smartscan smartrep smartparens smart-mode-line shrink-whitespace show-marks ruby-tools rubocop rinari relative-line-numbers rainbow-mode rainbow-delimiters ov org-journal operate-on-number multi-term multi-eshell move-text markdown-mode mark-tools magit livescript-mode linum-relative key-chord json-mode imenu-anywhere ido-ubiquitous helm-projectile helm-descbinds helm-cider helm-ag haml-mode guru-mode guide-key grizzl god-mode gitignore-mode github-issues gitconfig-mode git-timemachine gist geiser flycheck-ocaml flx-ido floobits expand-region exec-path-from-shell esh-help elisp-slime-nav easy-kill dot-mode discover-my-major discover-clj-refactor diminish diff-hl cycle-quotes csv-mode crux crontab-mode company-quickhelp company-ansible comment-dwim-2 clojure-snippets clojure-cheatsheet cljr-helm clippy chruby buffer-move browse-kill-ring bm beacon anzu adoc-mode ace-window ac-cider)))
 '(paradox-automatically-star t)
 '(paradox-github-token t)
 '(prelude-global-mode t)
 '(prelude-theme (quote monokai))
 '(prelude-whitespace t)
 '(projectile-enable-caching t)
 '(projectile-file-exists-remote-cache-expire nil)
 '(projectile-globally-ignored-directories
   (quote
    (".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "vendor")))
 '(projectile-mode t nil (projectile))
 '(projectile-switch-project-action (quote helm-projectile-find-file))
 '(safe-local-variable-values
   (quote
    ((cider-cljs-lein-repl . "(do (user/go) (user/cljs-repl))")
     (cider-refresh-after-fn . "reloaded.repl/resume")
     (cider-refresh-before-fn . "reloaded.repl/suspend"))))
 '(scroll-bar-mode nil)
 '(search-whitespace-regexp "\"[ \\t\\r\\n]+\"")
 '(standard-indent 2)
 '(text-scale-mode-step 1.1)
 '(tramp-default-method "ssh")
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
 '(which-key-max-description-length 45)
 '(yascroll:delay-to-hide nil)
 '(yascroll:scroll-bar (quote (right-fringe left-fringe text-area))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#272822" :foreground "#F8F8F2" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 110 :width normal :foundry "PfEd" :family "Fantasque Sans Mono"))))
 '(cursor ((t (:background "red" :foreground "#272822" :inverse-video t))))
 '(fixed-pitch ((t (:family "Fantasque Sans Mono"))))
 '(font-lock-doc-face ((t (:foreground "dodger blue" :weight bold))))
 '(font-lock-type-face ((t (:foreground "#66D9EF" :slant italic :weight bold))))
 '(idle-highlight ((t (:background "dark blue"))))
 '(lazy-highlight ((t (:inherit highlight :background "dark orange" :foreground "black" :weight bold))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :weight bold :height 1.3))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :weight bold :height 1.2))))
 '(mode-line ((t (:box (:line-width 2 :color "blue")))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "dark orange" :weight bold))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "deep pink" :weight bold))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "chartreuse" :weight bold))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "deep sky blue" :weight bold))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "yellow" :weight bold))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "orchid" :weight bold))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "spring green" :weight bold))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "sienna1" :weight bold)))))
