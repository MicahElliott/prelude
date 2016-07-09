;;; personal-init --- Something someting

;;; Commentary:
;; Personal config instructions:
;; https://github.com/bbatsov/prelude/issues/596

;;; Code:

(prelude-require-package 'tangotango-theme)
(load-theme 'tangotango t t)
(enable-theme 'tangotango)

(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(show-paren-mode 1)

;; (define-key key-translation-map [?\C-h] [?\C-?])
;; (global-set-key (kbd "C-S-h") 'backward-kill-word)

(require 'prelude-helm-everywhere)

;; (prelude-require-package 'linum-relative)
;; (require 'linum-relative)
;; (linum-on)
;; (global-linum-mode t)
;; (setq linum-relative-current-symbol "")

(prelude-require-package 'relative-line-numbers)
(global-relative-line-numbers-mode)

;; Fastest window switching: http://emacs.stackexchange.com/a/3471/11025
;(global-set-key (kbd "C-.") 'other-window)
;(global-set-key (kbd "C-,") 'prev-window)
(defun prev-window () (interactive) (other-window -1))
;; other-window
;; Default
(global-set-key (kbd "C-x o") (lambda () (interactive) (other-window 1)))
;; C-x C-o is common an easer for switching back window
(global-set-key (kbd "C-x o") 'other-window)
(global-set-key (kbd "C-x C-o") (lambda () (interactive) (other-window -1)))
(global-set-key "\C-xO"    (lambda () (interactive) (delete-blank-lines)))

;; url view
;; (global-set-key (kbd "C-c u") (lambda () (interactive) (browse-url-firefox)))
(global-set-key (kbd "C-c u") 'browse-url-firefox)

;; smartparens overrides M-r, so changing default
(global-set-key "\M-R" 'move-to-window-line-top-bottom)

;;; Rainbow Parens (already part of prelude?)
(prelude-require-package 'rainbow-delimiters)
;; (require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Multiple cursors: https://github.com/magnars/multiple-cursors.el
(prelude-require-package 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


;;; Guide Key: completable help menu
;; https://github.com/kai2nenobu/guide-key
(prelude-require-package 'guide-key)
(require 'guide-key)
;;(setq guide-key/guide-key-sequence '("C-x r" "C-x 4"))
(setq guide-key/guide-key-sequence t)
(guide-key-mode 1)  ; Enable guide-key-mode



(setq multi-term-program "/usr/bin/zsh")
(setq shell-file-name "zsh")
(prelude-require-package 'multi-term)
(require 'multi-term)

(prelude-require-package 'adoc-mode)
(require 'adoc-mode)
(add-to-list 'auto-mode-alist '("\\.asc$" . adoc-mode))
;(add-to-list 'auto-mode-alist (cons "\\.txt\\'" 'adoc-mode))
(add-hook 'adoc-mode-hook (lambda() (buffer-face-mode t)))

;;; Commenter: still stuggling with this
(prelude-require-package 'comment-dwim-2)
(global-set-key (kbd "M-;") 'comment-dwim-2)

;; Typopunct: fancy quotes, etc: — ‘’ “”
; enable: M-x typopunct-mode
; https://www.emacswiki.org/emacs/TypographicalPunctuationMarks
;;(prelude-require-package 'typopunct)
;;(require 'typopunct)
;;(typopunct-change-language 'english t)

; https://github.com/k1LoW/emacs-ansible
(add-hook 'yaml-mode-hook #'ansible-doc-mode)
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(add-hook 'yaml-mode-hook '(lambda () (ansible 1)))
(add-hook 'yaml-mode-hook #'ansible-doc-mode)

;; https://www.emacswiki.org/emacs/OrgJournal
(prelude-require-package 'org-journal)
(setq org-journal-dir "~/doc/journal/")
(require 'org-journal)

;;; Switch to other/previous buffer quickly
;; http://emacsredux.com/blog/2013/04/28/switch-to-previous-buffer/
;; (global-set-key (kbd "C-c b") 'mode-line-other-buffer)
;; (global-set-key (kbd "C-c n") 'next-buffer)
;; (global-set-key (kbd "C-c p") 'previous-buffer)


;;; ElFeed: rss feed reader
(global-set-key (kbd "C-x w") 'elfeed)
(setq elfeed-feeds
      '("http://emacsredux.com/atom.xml"
	"http://planet.clojure.in/atom.xml"
	"http://www.micahelliott.com/feed.xml"
	"http://sachachua.com/blog/feed/"
	"http://carmenla.me/blog/rss.xml"
	"http://yogthos.net/feed.xml"
	"http://oremacs.com/atom.xml"
	"https://medium.com/feed/tech-talk"
	"http://planet.emacsen.org/atom.xml"
	"http://www.lispcast.com/feed"))

;; Prelude badly sets C-- to zoom out
(global-set-key (kbd "C--") 'negative-argument)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(global-set-key (kbd "C-c q") 'auto-fill-mode)

;; Not sure if works
(setq explicit-shell-file-name "/usr/bin/zsh")


;; Buffer Move
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

(custom-set-faces
 '(rainbow-delimiters-depth-1-face ( (t (:foreground "dark orange" :weight bold))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "deep pink" :weight bold))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "chartreuse" :weight bold))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "deep sky blue" :weight bold))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "yellow" :weight bold))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "orchid" :weight bold))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "spring green" :weight bold))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "sienna1" :weight bold))))
 ;; http://www.saltycrane.com/blog/2007/10/emacs-mode-line-color-custimization/
 '(mode-line ((t (:box (:line-width 2 :color "blue")))))
 ;; '(modeline-inactive "dark gray")
 )

;; (set-face-foreground 'modeline "white")
;; (set-face-background 'modeline "purple"
;; (set-face-background 'modeline-inactive "dark gray")

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; https://github.com/mickeynp/smart-scan
;; M-n,  M-p,  M-' (replace all), C-u M-' (scoped replace)
(prelude-require-package 'smartscan)
;; Why is this not getting enabled??
(message "enabling smartscan")
(smartscan-mode 1)
(message "did smartscan work??")

;; M-p and M-n for search word under point, OR:
;; First need to remove prelude's mapping
;; https://github.com/bbatsov/prelude/issues/106
(define-key prelude-mode-map "\M-o" nil)
(global-set-key (kbd "M-o") 'ace-window)

;; https://github.com/leoliu/easy-kill
(global-set-key [remap mark-sexp] 'easy-mark)
(global-set-key [remap kill-ring-save] 'easy-kill)
;; (global-set-key [remap kill-ring-save] 'easy-mark)


(require 'ansi-color)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)


;; Ruby
(prelude-require-package 'rubocop)
(add-hook 'ruby-mode-hook 'rubocop-mode)
(prelude-require-package 'rinari)
(add-hook 'ruby-mode-hook 'rinari-minor-mode)


(prelude-require-package 'crontab-mode)
(add-to-list 'auto-mode-alist '("\\.crontab\\'" . crontab-mode))

;; Disable prompt for kill
;; http://superuser.com/questions/354849/emacs-kill-buffer-without-prompt
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;;; EShell

;; (prelude-require-package 'multi-eshell)

;; https://www.masteringemacs.org/article/complete-guide-mastering-eshell
(require 'eshell)
(require 'em-smart)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)
(setq eshell-scroll-to-bottom-on-input t)
;; must then call:
;; M-: (eshell-smart-initialize)

;; https://github.com/howardabrams/dot-files/blob/master/emacs-eshell.org
(defalias 'e 'find-file)
(defalias 'ff 'find-file)
(defalias 'emacs 'find-file)

(add-hook 'eshell-mode-hook
          (lambda ()
            ;; (eshell-smart-initialize)
            (add-to-list 'eshell-visual-commands "ssh")
            (add-to-list 'eshell-visual-commands "tail")))

(defun eshell/x ()
  "Closes the EShell session and gets rid of the EShell window."
  (kill-buffer)
  (delete-window))

(defun eshell-here ()
  "Opens up a new shell in the directory associated.
With the current buffer's file.  The eshell is renamed to match
that directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))
    (insert (concat "ls"))
    (eshell-send-input)))

(global-set-key (kbd "C-!") 'eshell-here)

;; https://github.com/sritchie/emacs.d/blob/master/starter-kit-eshell.el

;; (eval-after-load 'esh-opt
;;   '(progn
;;      (eshell-smart-initialize)))





(prelude-require-package 'chruby)

;; LiveScript
(prelude-require-package 'livescript-mode)

(prelude-require-package 'dot-mode)
(require 'dot-mode)

;; (prelude-require-package 'esh-help)
;; (require 'esh-help)
;; (setup-esh-help-eldoc)  ;; To use eldoc in Eshell

(prelude-require-package 'buffer-move)
;; (define-key prelude-mode-map "\C\S\up" nil)
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)


;; Zsh
(add-hook 'shell-script-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)
            (setq tab-width 2)))

;; http://emacs.stackexchange.com/questions/13662/a-confirmation-after-c-x-c-c
;; (setq confirm-kill-emacs 'yes-or-no-p)
(global-unset-key (kbd "C-x C-c"))

;; http://stackoverflow.com/questions/6464738/how-can-i-switch-focus-after-buffer-split-in-emacs
(global-set-key "\C-x2"
  (lambda () (interactive) (split-window-vertically) (other-window 1)))
(global-set-key "\C-x3"
  (lambda () (interactive) (split-window-horizontally) (other-window 1)))

(prelude-require-package 'company-ansible)
(prelude-require-package 'company-quickhelp)
(prelude-require-package 'pos-tip)

;; Clojure
(prelude-require-package 'cider)
;; (prelude-require-package 'hydra)
;; (prelude-require-package 'clj-refactor)
;; (prelude-require-package 'clojure-snippets)

(defun my-clojure-mode-hook ()
  (clj-refactor-mode 1)
  (yas-minor-mode 1) ; for adding require/use/import statements
  ;; This choice of keybinding leaves cider-macroexpand-1 unbound
  (cljr-add-keybindings-with-prefix "C-c m"))
;; (add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

;; (setq ac-delay 0.1)
;; (setq ac-quick-help-delay 0.5)

;; Simply bind `cljr-helm` to a key (I'd suggest C-c r) in Clojure
;; mode, and you're ready to go.
;; (prelude-require-package 'cljr-helm)
;; (prelude-require-package 'discover-clj-refactor)

(prelude-require-package 'ac-cider)
(prelude-require-package 'helm-cider)
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-mode))

(provide 'mde)

;;; mde ends here
