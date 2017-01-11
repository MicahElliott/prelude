;;; personal-init --- Something someting

;;; Commentary:
;; Personal config instructions:
;; https://github.com/bbatsov/prelude/issues/596

;;; Code:

(setq inhibit-startup-message nil)

;; http://stackoverflow.com/questions/22898244/
;; (eval-when-compile (defvar prelude-mode-map))

(prelude-require-package 'tangotango-theme)
(load-theme 'tangotango t t)
(enable-theme 'tangotango)

(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(show-paren-mode 1)


(prelude-require-package 'exec-path-from-shell)
(prelude-require-package 'shrink-whitespace)

(prelude-require-package 'unfill)
;; (define-key prelude-mode-map "\M-\S-q" nil)
(define-key prelude-mode-map "\M-Q" nil)
(global-set-key (kbd "M-Q") 'unfill-paragraph)
;; (global-set-key (kbd "M-S-q") 'unfill-paragraph)

(prelude-require-package 'langtool)

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


;;; Mark mode improvements

(defface visible-mark-active ;; put this before (require 'visible-mark)
  '((((type tty) (class mono)))
    (t (:background "magenta"))) "")
(setq visible-mark-max 4)
(setq visible-mark-faces `(visible-mark-face1 visible-mark-face2))
(prelude-require-package 'visible-mark)

;; https://www.masteringemacs.org/article/fixing-mark-commands-transient-mark-mode
;; (setq transient-mark-mode nil)

(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
   Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))
(global-set-key (kbd "C-`") 'push-mark-no-activate)

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
  This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))
(global-set-key (kbd "M-`") 'jump-to-mark)

(defun exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))
(define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)

;; (prelude-require-package 'show-marks)
;; (prelude-require-package 'bm)
;; (prelude-require-package 'mark-tools)


(global-unset-key (kbd "C-z"))

(global-set-key (kbd "M-P") 'prev-window)
(global-set-key (kbd "M-N") 'other-window)


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
(add-hook 'prog-mode-hook #'superword-mode)

;; Multiple cursors: https://github.com/magnars/multiple-cursors.el
(prelude-require-package 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; thread says Helm Descbinds is better than guide-key:
;; https://github.com/bbatsov/prelude/issues/481
;; ;;; Guide Key: completable help menu
;; ;; https://github.com/kai2nenobu/guide-key
;; (prelude-require-package 'guide-key)
;; (require 'guide-key)
;; (setq guide-key/guide-key-sequence t)
;; (guide-key-mode 1)  ; Enable guide-key-mode

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
;; enable: M-x typopunct-mode
;; https://www.emacswiki.org/emacs/TypographicalPunctuationMarks
;;(prelude-require-package 'typopunct)
;;(require 'typopunct)
;;(typopunct-change-language 'english t)

;; https://github.com/k1LoW/emacs-ansible
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

;; Like tpope's vim-surround
(prelude-require-package 'corral)
(setq corral-preserve-point t)
(global-set-key (kbd "M-[") 'corral-brackets-backward)
(global-set-key (kbd "M-]") 'corral-brackets-forward)
(global-set-key (kbd "M-\"") 'corral-double-quotes-backward)
(define-key prelude-mode-map "\M-'" nil)
(global-set-key (kbd "M-'") 'corral-single-quotes-backward)
;; need the following to be implemented for other surroundings
;; (global-set-key (kbd "M-_") 'corral-underscore-forward)
;; (global-set-key (kbd "M-`") 'corral-backquote-forward)
;; (global-set-key (kbd "M-*") 'corral-wrap-backward)

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
;; Disabling since bad to have global, overrides in REPLs
;; and does a bad job with extra syntax boundaries
;; (prelude-require-package 'smartscan)
;; (global-smartscan-mode 1)

;; M-p and M-n for search word under point, OR:
;; First need to remove prelude's mapping
;; https://github.com/bbatsov/prelude/issues/106
(define-key prelude-mode-map "\M-o" nil)
(global-set-key (kbd "M-o") 'ace-window)


;; https://github.com/leoliu/easy-kill
(global-set-key [remap mark-sexp] 'easy-mark)
(global-set-key [remap kill-ring-save] 'easy-kill)
;; (global-set-key [remap kill-ring-save] 'easy-mark)

(prelude-require-package 'jump-char)
;; But what about `back-to-indentation' (bound to M-m by default)?
;; You should customize C-a to toggle between indentation and
;; beginning of line like a civilized human being.
(global-set-key [(meta m)] 'jump-char-forward)
(global-set-key [(shift meta m)] 'jump-char-backward)

;; (key-chord-define-global "jj" nil)
(key-chord-define-global "jj" 'avy-goto-char-2)

(require 'ansi-color)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)


(prelude-require-package 'dired+)




;; Need to figure out how to only load this for shell-mode
;; ;; Shell
;; (load-file "ruby-tools.el")
;; (defvar ruby-tools-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map (kbd "C-'") 'ruby-tools-to-single-quote-string)
;;     (define-key map (kbd "C-\"") 'ruby-tools-to-double-quote-string)
;;     (define-key map (kbd "C-:") 'ruby-tools-to-symbol)
;;     (define-key map (kbd "C-;") 'ruby-tools-clear-string)
;;     (define-key map (kbd "#") 'ruby-tools-interpolate)
;;     map)
;;   "Keymap for `ruby-tools-mode'.")


;; Ruby
(prelude-require-package 'chruby)
(prelude-require-package 'rubocop)
(add-hook 'ruby-mode-hook 'rubocop-mode)
;; Rails (C-c ' ...)
(prelude-require-package 'rinari)
(add-hook 'ruby-mode-hook 'rinari-minor-mode)

(add-hook 'ruby-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-x C-e") 'ruby-send-region)
             ))

;; CoffeeScript
(prelude-require-package 'flymake-coffee)


(prelude-require-package 'crontab-mode)
(add-to-list 'auto-mode-alist '("\\.crontab\\'" . crontab-mode))

;; Disable prompt for kill
;; http://superuser.com/questions/354849/emacs-kill-buffer-without-prompt
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;;; Haskell
(prelude-require-package 'ghc)
(prelude-require-package 'flymake-hlint)
;; (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-tag-find)

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



;; Slime-like for shell (C-u C-x M-m)
;; http://stackoverflow.com/questions/6286579/
(defun sh-send-line-or-region (&optional step)
  (interactive ())
  (let ((proc (get-process "shell"))
        pbuf min max command)
    (unless proc
      (let ((currbuff (current-buffer)))
        (shell)
        (switch-to-buffer currbuff)
        (setq proc (get-process "shell"))
        ))
    (setq pbuff (process-buffer proc))
    (if (use-region-p)
        (setq min (region-beginning)
              max (region-end))
      (setq min (point-at-bol)
            max (point-at-eol)))
    (setq command (concat (buffer-substring min max) "\n"))
    (with-current-buffer pbuff
      (goto-char (process-mark proc))
      (insert command)
      (move-marker (process-mark proc) (point))
      ) ;;pop-to-buffer does not work with save-current-buffer -- bug?
    (process-send-string  proc command)
    (display-buffer (process-buffer proc) t)
    (when step
      (goto-char max)
      (next-line))
    ))
(defun sh-send-line-or-region-and-step ()
  (interactive)
  (sh-send-line-or-region t))
(defun sh-switch-to-process-buffer ()
  (interactive)
  (pop-to-buffer (process-buffer (get-process "shell")) t))

(define-key sh-mode-map [(control ?j)] 'sh-send-line-or-region-and-step2)
(define-key sh-mode-map [(control ?c) (control ?z)] 'sh-switch-to-process-buffer)

;; http://stackoverflow.com/questions/25819034/
(set-face-attribute 'comint-highlight-prompt nil
                    :inherit nil)



;; LiveScript
(prelude-require-package 'livescript-mode)

(prelude-require-package 'dot-mode)
(require 'dot-mode)
;; https://www.emacswiki.org/emacs/dot-mode.el
;; C-.  C-M-.  C-c.
(autoload 'dot-mode "dot-mode" nil t)
(dot-mode t)

;; https://github.com/domtronn/all-the-icons.el
(prelude-require-package 'all-the-icons)

;; https://github.com/jaypei/emacs-neotree
(prelude-require-package 'neotree)
(global-set-key [f8] 'neotree-toggle)
;; (setq neo-theme (if window-system 'icons 'arrow))
(setq neo-theme 'icons)

;; (prelude-require-package 'esh-help)
;; (require 'esh-help)
;; (setup-esh-help-eldoc)  ;; To use eldoc in Eshell

;; Buffer Move
(define-key prelude-mode-map (kbd "S-<up>")  nil)
(define-key prelude-mode-map (kbd "S-<down>")  nil)
(define-key prelude-mode-map (kbd "S-<left>")  nil)
(define-key prelude-mode-map (kbd "S-<right>")  nil)
(global-set-key (kbd "S-<up>")     'shrink-window)
(global-set-key (kbd "S-<down>")   'enlarge-window)
(global-set-key (kbd "S-<left>")   'shrink-window-horizontally)
(global-set-key (kbd "S-<right>")  'enlarge-window-horizontally)

(prelude-require-package 'buffer-move)
;; https://github.com/bbatsov/prelude/issues/106
;; have to unbind first
(define-key prelude-mode-map (kbd "C-S-<up>")  nil)
(define-key prelude-mode-map (kbd "C-S-<down>")  nil)
(define-key prelude-mode-map (kbd "C-S-<left>")  nil)
(define-key prelude-mode-map (kbd "C-S-<right>")  nil)
(global-set-key (kbd "C-S-<up>")     'buf-move-up)
(global-set-key (kbd "C-S-<left>")   'buf-move-left)
(global-set-key (kbd "C-S-<down>")   'buf-move-down)
(global-set-key (kbd "C-S-<right>")  'buf-move-right)

(prelude-require-package 'smooth-scrolling)
(smooth-scrolling-mode t)
;; (defun gcm-scroll-down () (interactive) (scroll-up 1))
;; (defun gcm-scroll-up () (interactive) (scroll-down 1))
;; (global-set-key [(control down)] 'gcm-scroll-down)
;; (global-set-key [(control up)]   'gcm-scroll-up)

;; https://www.emacswiki.org/emacs/HighlightCurrentColumn
(prelude-require-package 'smooth-scrolling)


;; (prelude-require-package 'pretty-mode)
;; (global-pretty-mode t)
(global-prettify-symbols-mode t)


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

(setq company-tooltip-idle-delay 0.1)
(setq company-quickhelp-delay 0.1)
(prelude-require-package 'pos-tip)
(require 'pos-tip)
(prelude-require-package 'company-quickhelp)
(company-quickhelp-mode 1)
;; stupid thing overrids M-h

;; (prelude-require-package 'clippy)
;; (setq clippy-tip-show-function #'clippy-popup-tip-show)

(prelude-require-package 'github-issues)

;; Clojure
;; NOTE: also installed to ~/.lein/profiles.clj: kibit, eastwood
(prelude-require-package 'smartparens)  ; better paredit, sp-*
(prelude-require-package 'cider)
(prelude-require-package 'hydra)
(prelude-require-package 'clj-refactor)
(prelude-require-package 'clojure-snippets) ; yas for clojure
(prelude-require-package 'clojure-cheatsheet)
(prelude-require-package 'flycheck-clojure)
(prelude-require-package 'helm-clojuredocs)
(prelude-require-package 'slamhound)
(prelude-require-package 'kibit-helper)
(prelude-require-package 'sotclojure)
(prelude-require-package 'flycheck-tip)
(prelude-require-package 'clojure-mode-extra-font-locking)

(defun my-clojure-mode-hook () "Foo bar."
       (message "in my-clojure-mode-hook")
       (clj-refactor-mode 1)
       (yas-minor-mode 1) ; for adding require/use/import statements
       ;; This choice of keybinding leaves cider-macroexpand-1 unbound
       (global-set-key (kbd "M-h") 'mark-paragraph)
       (cljr-add-keybindings-with-prefix "C-c m"))
(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

(message "MDE: before eval-after-load")
(eval-after-load "clojure-mode"
  '(progn
     ;; (define-key clojure-mode-map (kbd "C-c C-h") #'clojure-cheatsheet)
     (message "MDE: in clojure eval-after-load")))

;; https://github.com/clojure-emacs/squiggly-clojure
(eval-after-load 'flycheck '(flycheck-clojure-setup))
(add-hook 'after-init-hook #'global-flycheck-mode)

;; http://stackoverflow.com/questions/23766483/emacs-cider-clojure-auto-complete-how-to-get-the-docstring
(setq ac-delay 0.1)
(setq ac-quick-help-delay 0.1)

;; Simply bind `cljr-helm` to a key (I'd suggest C-c r) in Clojure
;; mode, and you're ready to go.
(prelude-require-package 'cljr-helm)
(prelude-require-package 'discover-clj-refactor)

(prelude-require-package 'ac-cider)
(prelude-require-package 'helm-cider)
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-mode))

;; Minor mode for personal overrides
;; http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs
(defvar my-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key prelude-mode-map (kbd "M-h")  nil)
    ;; (global-set-key (kbd "M-h") 'mark-paragraph)
    (define-key map (kbd "M-h") 'mark-paragraph)
    ;; (define-key map (kbd "C-i") 'some-function)
    map)
  "Custom my-keys-minor-mode keymap.")
(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
[]  :init-value t
  :lighter " my-keys")
(my-keys-minor-mode 1)

(defun my-minibuffer-setup-hook () "Foo bar."
       (my-keys-minor-mode 0))
(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)

;; http://stackoverflow.com/questions/18812938/copy-full-file-path-into-copy-paste-clipboard
(defun clip-file ()
  "Put the current file name on the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      (file-name-directory default-directory)
                    (buffer-file-name))))
    (when filename
      (x-select-text filename))))


(provide 'mde)

;;; mde ends here
