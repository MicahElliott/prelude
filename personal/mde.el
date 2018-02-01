;;; personal-init --- Micah's customizations

;;; Commentary:
;; Personal config instructions:
;; https://github.com/bbatsov/prelude/issues/596

;; Process for updating:
;; https://help.github.com/articles/syncing-a-fork/
;; git fetch upstream
;; git pull upstream master

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Libraries

;; f, Modern API for working with files and directories in Emacs
;; (used in one of magnars' snippets)
;; https://github.com/rejeep/f.el
;; (prelude-require-package 'f)
;; (require 'f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Look-n-Feel

;; (set-language-environment "UTF-8")
;; (set-default-coding-systems 'utf-8)
;;(set-frame-font "Ubuntu Mono 10" nil t)

;; Stop Emacs from processing .Xresources/.Xdefaults
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Resources.html#Resources
(setq inhibit-x-resources t)

;; http://stackoverflow.com/questions/22898244/
;; (eval-when-compile (defvar prelude-mode-map))
(defvar prelude-mode-map)

;; No splash screen
;; (setq inhibit-startup-message nil)
(setq inhibit-startup-message nil)
(setq inhibit-startup-screen nil)
(setq inhibit-splash-screen nil)

;; Jump to help window when opened
;; http://stackoverflow.com/questions/36506141/emacs-dispatch-help-window-from-original-buffer
(setq help-window-select t)

(prelude-require-package 'smart-mode-line-powerline-theme)

;; I don't think this will work since running in multi-client mode via `e'.
;; (prelude-require-package 'dashboard)
;; (dashboard-setup-startup-hook)

;; (prelude-require-package 'tangotango-theme)
;; (load-theme 'tangotango t t)
;; (enable-theme 'tangotango)
(prelude-require-package 'monokai-theme)
(setq prelude-theme 'monokai-theme)
(load-theme 'monokai t t)
(enable-theme 'monokai)

;; special treatment of FIXME, etc
(prelude-require-package 'fic-mode)
(add-hook 'prog-mode-hook 'fic-mode)

;; Wow, hide comments!!
;; (prelude-require-package 'hide-comnt)
;; (global-set-key (kbd "C-c c") 'hide/show-comments-toggle)

;; Planck-friendly
(global-set-key (kbd "M-{") 'backward-paragraph)
(global-set-key (kbd "M-<") 'forward-paragraph)
(global-set-key (kbd "M->") 'end-of-buffer)
(global-set-key (kbd "M-}") 'beginning-of-buffer)

;; Try vim-style word movement (start of word forward)
(require 'misc)
;; Replaces forward-word
;; (global-set-key (kbd "M-f") 'forward-to-word)

;; Theme overrides
;; (set-face-attribute 'region nil :background "#999")

;; (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq scroll-bar-width 2)

;; https://www.emacswiki.org/emacs/HighlightCurrentColumn
;; (prelude-require-package 'smooth-scrolling)
;; (smooth-scrolling-mode t)
;; (defun gcm-scroll-down () (interactive) (scroll-up 1))
;; (defun gcm-scroll-up () (interactive) (scroll-down 1))
;; (global-set-key [(control down)] 'gcm-scroll-down)
;; (global-set-key [(control up)]   'gcm-scroll-up)



;; Highlight word matching point without doing anything
;; https://github.com/nonsequitur/idle-highlight-mode/blob/master/idle-highlight-mode.el
;; Disabling since might play badly with org-mode
;; Also screws with visible-mark.
(prelude-require-package 'idle-highlight-mode)
(add-hook 'prog-mode-hook 'idle-highlight-mode)
;; (add-hook 'text-mode-hook 'idle-highlight-mode)

;; Highlight the point column (see `col-highlight' in customize)
;; (not really using)
;; Not great since covers up mark markers, and funky with variable-width lines.
(prelude-require-package 'crosshairs)
;; (column-highlight-mode t)

;; Make the active window more visible than others
;; https://github.com/yoshida-mediba/hiwin-mode
;; Ugh, make cider repl go blank when switching out
;; (prelude-require-package 'hiwin)
;; (hiwin-activate)
;; Set face color with customize
;; (set-face-background 'hiwin-face "gray80")

;; Dim inactive buffers. Works way better than hiwin!
(prelude-require-package 'auto-dim-other-buffers)
(add-hook 'after-init-hook (lambda ()
                             (when (fboundp 'auto-dim-other-buffers-mode)
                               (auto-dim-other-buffers-mode t))))


;; Disable discoloration of long lines (and other stuff)
;; https://github.com/bbatsov/prelude#disabling-whitespace-mode
;; Toggle these two lines and repoen a file
;; (setq prelude-whitespace nil)
;; (setq prelude-whitespace t)

(prelude-require-package 'linum-relative)
(linum-relative-global-mode)
;; (require 'linum-relative)
;; (linum-on)
;; (global-linum-mode t)
;; (setq linum-relative-current-symbol "")

;; (prelude-require-package 'relative-line-numbers)
;; (global-relative-line-numbers-mode)

;; Make all the marks visible
;; (defface visible-mark-active ;; put this before (require 'visible-mark)
;;   '((((type tty) (class mono)))
;;     (t (:background "magenta"))) "")
(setq visible-mark-max 2)
(setq visible-mark-faces `(visible-mark-face1 visible-mark-face2))
(prelude-require-package 'visible-mark)
(global-visible-mark-mode)

;;; Rainbow Parens (already part of prelude?)
(prelude-require-package 'rainbow-delimiters)
;; (require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
;; FIXME: why isn't superword working? Ah, because silly prelude was
;; explicitly setting subword mode.
(add-hook 'prog-mode-hook 'superword-mode)
(superword-mode 1)

;; Emoji!! 🐱 🐶 🔘 ☢ 🎿 😄 😱 😸 👸 👽 🙋 🚸
;; Also run gitmoji in terminal
;; (prelude-require-package 'flycheck-status-emoji)
;; (prelude-require-package 'company-emoji)
;; (add-to-list 'company-backends 'company-emoji)
;; (prelude-require-package 'emojify)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Behavior

;; Register marking/jumping, closer to vim
(global-set-key (kbd "C-S-M") 'point-to-register)
;; Hmm, M-J is needed for sp-join-sexp
;; (global-set-key (kbd "M-J") 'jump-to-register)
(global-set-key (kbd "C-S-J") 'jump-to-register)

(global-set-key (kbd "C-M-_") 'text-scale-decrease)
(global-set-key (kbd "C-M-+") 'text-scale-increase)

(global-set-key (kbd "C-x C-f") 'ido-find-file)

;; Scroll only half-screen
;; http://stackoverflow.com/a/19690877/326516
;; (require 'view)
;; (global-set-key "\C-v"   'View-scroll-half-page-forward)
;; (global-set-key "\M-v"   'View-scroll-half-page-backward)
;; Actually, I like full-screen scrolling.

;; Simple highlighting scrollbar
;; https://github.com/m2ym/yascroll-el
(prelude-require-package 'yascroll)
(global-yascroll-bar-mode 1)

;; (prelude-require-package 'centered-cursor-mode)
;; (global-centered-cursor-mode +1)

;; Make Emacs use the $PATH set up by the user's shell
(prelude-require-package 'exec-path-from-shell)

;; Whitespace removal DWIM key for emacs.
;; maybe use in future, not bound to anything
;; https://github.com/jcpetkovich/shrink-whitespace.el
(prelude-require-package 'shrink-whitespace)

;; Inverse of Emacs' fill-paragraph and fill-region
;; https://github.com/purcell/unfill
;; http://stackoverflow.com/questions/6707758/inverse-of-m-q-an-unfill-paragraph-function
(prelude-require-package 'unfill)
;; (define-key prelude-mode-map "\M-\S-q" nil)
(define-key prelude-mode-map "\M-Q" nil)
(global-set-key (kbd "M-Q") 'unfill-paragraph)
;; (global-set-key (kbd "M-S-q") 'unfill-paragraph)

;; Spelling/grammar help
;; https://github.com/mhayashi1120/Emacs-langtool
(prelude-require-package 'langtool)

(setq tab-stop-list (number-sequence 2 200 2))

;; Zsh, hopefully
(setq indent-tabs-mode t)
(setq tab-width 2)

;; (prelude-require-package 'bash-completion)
;; (bash-completion-setup)
;; (prelude-require-package 'zlc)
;; (zlc-mode t)

;; (define-key key-translation-map [?\C-h] [?\C-?])
;; (global-set-key (kbd "C-S-h") 'backward-kill-word)

;; (prelude-require-package 'yasnippet)
;; (setq yas-snippet-dirs
;;       '("~/.emacs.d/snippets"                 ;; personal snippets
;;         ))
;; (yas-global-mode 1)

;;; Mark mode improvements

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
;; (global-set-key (kbd "M-`") 'jump-to-mark)

;; WTF did I ever do this??
(defun exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))
;; (define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)

;; (prelude-require-package 'show-marks)
;; (prelude-require-package 'bm)
;; (prelude-require-package 'mark-tools)


;; Don't want to suspend emacs!
;; http://superuser.com/questions/349943/how-to-awake-emacs-gui-after-pressing-ctrlz#349997
(global-unset-key (kbd "C-z"))

;; (prelude-require-package 'pretty-mode)
;; (global-pretty-mode t)
(global-prettify-symbols-mode t)


;; ;; Make number colorful.
;; (prelude-require-package 'highlight-numbers)
;; (add-hook 'prog-mode-hook 'highlight-numbers-mode)

;; Jump to open file in neotree
(setq neo-smart-open t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Windowing
;;; Most should use C-S-

;; Ace Window
;; https://github.com/abo-abo/ace-window
;; First need to remove prelude's mapping
;; https://github.com/bbatsov/prelude/issues/106
(define-key prelude-mode-map "\M-o" nil)
(global-set-key (kbd "M-o") 'ace-window)
(defvar aw-dispatch-alist
  '((?x aw-delete-window "Delete Window")
    (?m aw-swap-window "Swap Window")
    (?M aw-move-window "Move Window")
    (?j aw-switch-buffer-in-window "Select Buffer")
    (?n aw-flip-window)
    (?\t aw-flip-window)
    (?c aw-split-window-fair "Split Fair Window")
    (?v aw-split-window-vert "Split Vert Window")
    (?b aw-split-window-horz "Split Horz Window")
    (?i delete-other-windows "Delete Other Windows")
    (?o delete-other-windows)
    (?? aw-show-dispatch-help))
  "List of actions for `aw-dispatch-default'.")

(require 'key-chord)
(key-chord-define-global "KK" 'aw-flip-window)
(global-set-key (kbd "C-<tab>") 'aw-flip-window)
(global-set-key (kbd "M-<tab>") 'ace-window)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
;; Planck arrows
(global-set-key (kbd "C-S-f") 'windmove-right)
(global-set-key (kbd "C-S-s") 'windmove-left)
(global-set-key (kbd "C-S-e") 'windmove-up)
(global-set-key (kbd "C-S-d") 'windmove-down)
;; (define-key prelude-mode-map "<right>" nil)
;; Disable guru from touching these.
(setq prelude-guru nil)
(global-set-key (kbd "C-<right>") 'windmove-right)
(global-set-key (kbd "C-<left>") 'windmove-left)
(global-set-key (kbd "C-<up>") 'windmove-up)
(global-set-key (kbd "C-<down>") 'windmove-down)
;; (global-set-key (kbd "C-s") 'isearch-forward)

;; Fastest window switching: http://emacs.stackexchange.com/a/3471/11025
;; (global-set-key (kbd "C-.") 'other-window)
;; (global-set-key (kbd "C-,") 'prev-window)
;; Switch windows; lower case version is used in too many shells/menus
(global-set-key (kbd "M-P") 'prev-window)
(global-set-key (kbd "M-N") 'other-window)
(defun prev-window () (interactive) (other-window -1))
;; other-window
;; Default
(global-set-key (kbd "C-x o") (lambda () (interactive) (other-window 1)))
;; C-x C-o is common an easer for switching back window
(global-set-key (kbd "C-x o") 'other-window)
(global-set-key (kbd "C-x C-o") (lambda () (interactive) (other-window -1)))
(global-set-key "\C-xO"    (lambda () (interactive) (delete-blank-lines)))

;; Resize windows
(define-key prelude-mode-map (kbd "S-<up>")  nil)
(define-key prelude-mode-map (kbd "S-<down>")  nil)
(define-key prelude-mode-map (kbd "S-<left>")  nil)
(define-key prelude-mode-map (kbd "S-<right>")  nil)
(global-set-key (kbd "S-<up>")     'shrink-window)
(global-set-key (kbd "S-<down>")   'enlarge-window)
(global-set-key (kbd "S-<left>")   'shrink-window-horizontally)
(global-set-key (kbd "S-<right>")  'enlarge-window-horizontally)

;; Shuffle/swap windows around
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

;; move line up/down (already enabled) -- M-S-up
;; move-text-down

(defun delete-window-balancedly ()
  (interactive)
  (delete-window)
  (balance-windows))
(global-set-key (kbd "C-S-z") 'delete-window-balancedly)

(global-set-key (kbd "C-S-o") 'delete-other-windows) ; think "Only"
(global-set-key (kbd "C-S-g") 'winner-undo)
;; (global-set-key (kbd "C-S-+") 'balance-windows)

;; Should change focus to new window.
(defun split-window-balancedly ()
  (interactive)
  (split-window-horizontally)
  (balance-windows)
  (other-window 1)
  (helm-projectile-find-file))
;; (global-set-key (kbd "C-S-n") 'split-window-horizontally)
(global-set-key (kbd "C-S-n") 'split-window-balancedly)

;; Scroll without moving point; like Vim's C-y, C-e
;; http://stackoverflow.com/a/10541426/326516
(defun scroll-up-stay (arg)
  (interactive "p")
  (forward-line (* -1 arg))
  (scroll-up arg))
(defun scroll-down-stay (arg)
  (interactive "p")
  (scroll-down arg)
  (forward-line arg))
(global-set-key (kbd "C-S-T") 'scroll-up-stay)
(global-set-key (kbd "C-S-Y") 'scroll-down-stay)
;; (global-set-key (kbd "C-y") 'yank)
;; (global-set-key (kbd "C-t") 'transpose-chars)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Packages

(prelude-require-package 'lorem-ipsum)

;; Replacement for package-list-packages
(prelude-require-package 'paradox)

;; url view
;; (global-set-key (kbd "C-c u") (lambda () (interactive) (browse-url-firefox)))
(global-set-key (kbd "C-c u") 'browse-url-firefox)

;; Camel, Kebab cases
;; https://stackoverflow.com/a/27422814/326516
(prelude-require-package 'string-inflection)
(global-set-key (kbd "C-c C") 'string-inflection-camelcase)        ;; Force to CamelCase
(global-set-key (kbd "C-c L") 'string-inflection-lower-camelcase)  ;; Force to lowerCamelCase
(global-set-key (kbd "C-c J") 'string-inflection-java-style-cycle) ;; Cycle through Java styles
(global-set-key (kbd "C-c K") 'string-inflection-kebab-case) ;; Cycle through Java styles

;; smartparens overrides M-r, so changing default
(global-set-key "\M-R" 'move-to-window-line-top-bottom)
;; Since already holding M-S-R, enable recenter (usually C-l) to also be M-S
(global-set-key (kbd "M-L") 'recenter-top-bottom)

;; Multiple cursors: https://github.com/magnars/multiple-cursors.el
(prelude-require-package 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Tramp
(tramp-set-completion-function
 "ssh" '((tramp-parse-sconfig "~/.ssh/config")
         (tramp-parse-sconfig "~/proj/Membean/provn/ansible/ssh-inventory.config")))

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
;; (require 'multi-term)

(prelude-require-package 'adoc-mode)
;; (require 'adoc-mode)
(add-to-list 'auto-mode-alist '("\\.asc$" . adoc-mode))
                                        ;(add-to-list 'auto-mode-alist (cons "\\.txt\\'" 'adoc-mode))
(add-hook 'adoc-mode-hook (lambda () (buffer-face-mode t)))

;;; Commenter: still stuggling with this
(prelude-require-package 'comment-dwim-2)
(global-set-key (kbd "M-;") 'comment-dwim-2)

;; Typopunct: fancy/pretty quotes, etc: — ‘’ “”
;; enable: M-x typopunct-mode
;; https://www.emacswiki.org/emacs/TypographicalPunctuationMarks
;; (prelude-require-package 'typopunct)
;; (typopunct-change-language 'english t)
;; (typopunct-mode 1)
;; https://github.com/jorgenschaefer/typoel
(prelude-require-package 'typo)
(setq-default typo-language "English")
(global-set-key (kbd "C-c T") 'typo-mode)
;; ISSUE: Need to auto-enter typo-mode only while inside strings.
;; M-x typo-mode

;; Edit in new buffer
;; Better than poporg-edit?  Great for markdown.  Can even eval code.
;; Keys: C-c ' (start), C-c C-c (commit)
(prelude-require-package 'edit-indirect)

;; Simpler attempt at typography.
(global-set-key (kbd "C-c '") "’")
(global-set-key (kbd "C-c `'") "‘")
(global-set-key (kbd "C-c \"") "“")
(global-set-key (kbd "C-c /") "”")
(global-set-key (kbd "C-c -") "—")

(prelude-require-package 'cycle-quotes)
(prelude-require-package 'toggle-quotes)
(global-set-key (kbd "C-'") 'toggle-quotes)
;; TEST: Can't "do" this.

;; Not working with bitbucket to to URL in .git/config
;; (prelude-require-package 'browse-at-remote)
(prelude-require-package 'git-messenger)

;; Magit: came with Super-based shortcuts; use C-c g ... instead
(define-key prelude-mode-map (kbd "C-c g")  nil)
(global-set-key (kbd "C-c g g") 'magit-status)
(global-set-key (kbd "C-c g b") 'magit-blame)
(global-set-key (kbd "C-c g l") 'magit-log-buffer-file)
(global-set-key (kbd "C-c g t") 'git-timemachine-toggle)
(global-set-key (kbd "C-c g t") 'git-timemachine-toggle)
(global-set-key (kbd "C-c g B") 'browse-at-remote)
(global-set-key (kbd "C-c g a") 'vc-annotate)
(global-set-key (kbd "C-c g p") 'git-messenger:popup-message)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-mode
;; https://www.emacswiki.org/emacs/OrgJournal
(prelude-require-package 'org-journal)
(setq org-journal-dir "~/doc/journal/")
;; (require 'org-journal)
;; Seems to do too much; might have failed install
;; (prelude-require-package 'ox-reveal)
;; (prelude-require-package 'htmlize)
;; (prelude-require-package 'epresent)
;; (prelude-require-package 'org-tree-slide)
;; (prelude-require-package 'org-bullets)

;; Dim surrounding text.
;; https://github.com/larstvei/Focus (neat idea)
;; (prelude-require-package 'focus)
;; (focus-mode 0)

;; Like tpope's vim-surround
;; (prelude-require-package 'corral)       ; doesn't honor selection
;; (setq corral-preserve-point t)
;; (global-set-key (kbd "M-[") 'corral-brackets-backward)
;; (global-set-key (kbd "M-]") 'corral-brackets-forward)
;; (global-set-key (kbd "M-\"") 'corral-double-quotes-backward)
;; (define-key prelude-mode-map "\M-'" nil)
;; (global-set-key (kbd "M-'") 'corral-single-quotes-backward)
;; need the following to be implemented for other surroundings
;; (global-set-key (kbd "M-_") 'corral-underscore-forward)
;; (global-set-key (kbd "M-`") 'corral-backquote-forward)
;; (global-set-key (kbd "M-*") 'corral-wrap-backward)

;; https://github.com/rejeep/wrap-region.el (bbatsov likes this)
(prelude-require-package 'wrap-region)
(wrap-region-global-mode t)
(wrap-region-add-wrappers
 '(("$" "$")
   ("{-" "-}" "#")
   ("/" "/" nil ruby-mode)
   ("/* " " */" "#" (java-mode javascript-mode css-mode))
   ("`" "`" nil (markdown-mode ruby-mode))))

;;; Switch to other/previous buffer quickly
;; http://emacsredux.com/blog/2013/04/28/switch-to-previous-buffer/
;; (global-set-key (kbd "C-c b") 'mode-line-other-buffer)
;; (global-set-key (kbd "C-c n") 'next-buffer)
;; (global-set-key (kbd "C-c p") 'previous-buffer)


;; ;;; ElFeed: rss feed reader
;; (prelude-require-package 'elfeed)
;; (global-set-key (kbd "C-x w") 'elfeed)
;; (setq elfeed-feeds
;;       '("http://emacsredux.com/atom.xml"
;;         "http://planet.clojure.in/atom.xml"
;;         "http://www.micahelliott.com/feed.xml"
;;         "http://sachachua.com/blog/feed/"
;;         "http://carmenla.me/blog/rss.xml"
;;         "http://yogthos.net/feed.xml"
;;         "http://oremacs.com/atom.xml"
;;         "https://medium.com/feed/tech-talk"
;;         "http://planet.emacsen.org/atom.xml"
;;         "http://www.lispcast.com/feed"
;;         ;; "http://www.espn.com/espn/rss/news"
;;         ))

;; Prelude badly sets C-- to zoom out, so keep as negative argument
(global-set-key (kbd "C--") 'negative-argument)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(global-set-key (kbd "C-c q") 'auto-fill-mode)

;; Not sure if works
(setq explicit-shell-file-name "/usr/bin/zsh")

;; Seems prelude already does this??
;; https://www.emacswiki.org/emacs/ShowParenMode
;; (show-paren-mode 1)
(setq show-paren-delay 0)
(show-paren-mode 0)
(show-smartparens-mode 0)

;; Highlight all parens you're inside
;; Not sure I love this.
;; https://www.emacswiki.org/emacs/HighlightParentheses
;; (prelude-require-package 'highlight-parentheses)
;; (global-highlight-parentheses-mode 1)
;; Uncomment to test.
;; ((((((((()))))))))

(custom-set-faces

 '(rainbow-delimiters-depth-1-face ((t (:foreground "royal blue" :weight bold))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "light slate blue" :weight bold))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "deep sky blue" :weight bold))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "cyan" :weight bold))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "forest green" :weight bold))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "yellow green" :weight bold))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "spring green" :weight bold))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "yellow" :weight bold))))

 ;; '(rainbow-delimiters-depth-1-face ((t (:foreground "dark orange"))))
 ;; '(rainbow-delimiters-depth-2-face ((t (:foreground "deep pink"))))
 ;; '(rainbow-delimiters-depth-3-face ((t (:foreground "chartreuse"))))
 ;; '(rainbow-delimiters-depth-4-face ((t (:foreground "deep sky blue"))))
 ;; '(rainbow-delimiters-depth-5-face ((t (:foreground "yellow"))))
 ;; '(rainbow-delimiters-depth-6-face ((t (:foreground "orchid"))))
 ;; '(rainbow-delimiters-depth-7-face ((t (:foreground "spring green"))))
 ;; '(rainbow-delimiters-depth-8-face ((t (:foreground "sienna1"))))

 ;; http://www.saltycrane.com/blog/2007/10/emacs-mode-line-color-custimization/
 '(mode-line ((t (:box (:line-width 2 :color "blue")))))
 ;; '(modeline-inactive "dark gray")
 )

;; (set-face-foreground 'modeline "white")
;; (set-face-background 'modeline "purple"
;; (set-face-background 'modeline-inactive "dark gray")

;; https://github.com/mickeynp/smart-scan
;; M-n,  M-p,  M-' (replace all), C-u M-' (scoped replace)
;; Disabling for problems:
;; - bad to have global overrides in REPLs
;; - does a bad job with extra syntax boundaries
;; - jumps to end of words, wrecking idle-highlight-mode
;; (prelude-require-package 'smartscan)
;; (global-smartscan-mode 1)

;; Easy occur searching
;; (global-set-key (kbd "C-o") 'open-line)  ; replaced
(global-set-key (kbd "C-o") 'helm-occur)
(global-set-key (kbd "C-*") 'helm-occur)

;; Select/highlight with easy-kill
;; https://github.com/leoliu/easy-kill
;; http://stackoverflow.com/a/36631886/326516
(global-set-key [remap mark-sexp] 'easy-mark)
(global-set-key [remap kill-ring-save] 'easy-kill)
;; (global-set-key [remap kill-ring-save] 'easy-mark)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Search/jump

(prelude-require-package 'jump-char)
;; But what about `back-to-indentation' (bound to M-m by default)?
;; You should customize C-a to toggle between indentation and
;; beginning of line like a civilized human being.
(global-set-key [(meta m)] 'jump-char-forward)
;; (global-set-key [(shift meta m)] 'jump-char-backward)

;; AVY: https://github.com/abo-abo/avy/wiki/defcustom
;; confine avy's characters for ease of typing/finding
;; (setq avy-keys (number-sequence ?a ?f))
;; Use only easiest left and right keys
(setq avy-keys (string-to-list "asdfwerkluioghqtypvcxz,.'j"))
;; (setq avy-keys (string-to-list "asdfwerjklasdfwerjklasdfwerjkl"))
;; only search in current window
(setq avy-all-windows nil)
;; make case-sensitive
(setq avy-case-fold-search nil)
;; (key-chord-define-global "jj" nil)
;; (key-chord-define-global "jj" 'avy-goto-char-2)
(key-chord-define-global "jj" 'avy-goto-word-1)
(key-chord-define-global "kk" 'avy-goto-char-2)
;; (key-chord-define-global "kk" 'avy-goto-line-above)
;; (key-chord-define-global "jj" 'avy-goto-line-below)

;; Ace window
;; https://github.com/abo-abo/ace-window
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(setq aw-dispatch-always t)
(setq aw-scope 'frame)
(defvar aw-dispatch-alist
  '((?x aw-delete-window " Ace - Delete Window")
    (?m aw-swap-window " Ace - Swap Window")
    (?n aw-flip-window)
    (?v aw-split-window-vert " Ace - Split Vert Window")
    (?b aw-split-window-horz " Ace - Split Horz Window")
    (?i delete-other-windows " Ace - Maximize Window")
    (?o delete-other-windows))
  "List of actions for `aw-dispatch-default'.")

;; (require 'ansi-color)
;; (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Prelude already requires dired-x.
;; (prelude-require-package 'dired+)

;; ;; https://github.com/lewang/command-log-mode
;; (prelude-require-package 'command-log-mode)
;; (require 'command-log-mode)
;; (add-hook 'LaTeX-mode-hook 'command-log-mode)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Language Setups


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


;; ;; Python
;; (prelude-require-package 'elpy)
;; (elpy-enable)
;; ;; (prelude-require-package 'jedi)
;; (prelude-require-package 'company-jedi)
;; (defun my/python-mode-hook ()
;;   (add-to-list 'company-backends 'company-jedi))
;; (add-hook 'python-mode-hook 'my/python-mode-hook)

;; (setq-default py-shell-name "ipython")
;; (setq-default py-which-bufname "IPython")
;; ;; use the wx backend, for both mayavi and matplotlib
;; (setq py-python-command-args '("--gui=wx" "--pylab=wx" "-colors" "Linux"))
;; (setq py-force-py-shell-name-p t)

;; switch to the interpreter after executing code
(setq py-shell-switch-buffers-on-execute-p t)
(setq py-switch-buffers-on-execute-p t)
;; don't split windows
(setq py-split-windows-on-execute-p nil)
;; try to automagically figure out indentation
(setq py-smart-indentation t)

;; ;; Ruby
;; (prelude-require-package 'chruby)
(prelude-require-package 'rubocop)
(add-hook 'ruby-mode-hook 'rubocop-mode)
;; ;; Rails (C-c ' ...)
;; (prelude-require-package 'rinari)
;; (add-hook 'ruby-mode-hook 'rinari-minor-mode)

;; (add-hook 'ruby-mode-hook
;;           '(lambda ()
;;              (local-set-key (kbd "C-x C-e") 'ruby-send-region)
;;              ))

;; CoffeeScript
;; (prelude-require-package 'flymake-coffee)


;; (prelude-require-package 'crontab-mode)
;; (add-to-list 'auto-mode-alist '("\\.crontab\\'" . crontab-mode))

;; Disable prompt for kill
;; http://superuser.com/questions/354849/emacs-kill-buffer-without-prompt
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; ;;; Haskell
;; (prelude-require-package 'ghc)
;; (prelude-require-package 'flymake-hlint)
;; ;; (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-tag-find)

;; (prelude-require-package 'intero)

;; ;;; Docker
;; (prelude-require-package 'dockerfile-mode)
;; (prelude-require-package 'docker)

;;; EShell

;; (prelude-require-package 'multi-eshell)

;; ;; https://www.masteringemacs.org/article/complete-guide-mastering-eshell
;; (require 'eshell)
;; (require 'em-smart)
;; (setq eshell-where-to-jump 'begin)
;; (setq eshell-review-quick-commands nil)
;; (setq eshell-smart-space-goes-to-end t)
;; (setq eshell-scroll-to-bottom-on-input t)
;; ;; must then call:
;; ;; M-: (eshell-smart-initialize)

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

;; Slime-like for shell/zsh (C-u C-x M-m)
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
      (forward-line))
    ))
(defun sh-send-line-or-region-and-step ()
  (interactive)
  (sh-send-line-or-region t))
(defun sh-switch-to-process-buffer ()
  (interactive)
  (pop-to-buffer (process-buffer (get-process "shell")) t))

;; (define-key sh-mode-map [(control ?j)] 'sh-send-line-or-region-and-step2)
;; (define-key sh-mode-map [(control ?c) (control ?z)] 'sh-switch-to-process-buffer)

;; http://stackoverflow.com/questions/25819034/
(set-face-attribute 'comint-highlight-prompt nil
                    :inherit nil)


;; Markdown
(prelude-require-package 'flymd)

;; Enable syntax highlighting of code in blocks.
(setq markdown-fontify-code-blocks-natively t)

;; LiveScript
;; (prelude-require-package 'livescript-mode)

(prelude-require-package 'dot-mode)
;; (require 'dot-mode)
;; https://www.emacswiki.org/emacs/dot-mode.el
;; C-.  C-M-.  C-c.
(autoload 'dot-mode "dot-mode" nil t)
;; (dot-mode t)
(global-dot-mode t)

;; https://github.com/domtronn/all-the-icons.el
(prelude-require-package 'all-the-icons)

;; https://github.com/jaypei/emacs-neotree
(prelude-require-package 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq neo-theme (if window-system 'icons 'arrow))
(setq neo-theme 'icons)
;; open neotree with current file as root
(setq neo-smart-open t)

;; Try dir-tree too.  Manual install.
;; https://github.com/rtircher/dirtree
;; (require 'dir-tree)

;; (prelude-require-package 'esh-help)
;; (require 'esh-help)
;; (setup-esh-help-eldoc)  ;; To use eldoc in Eshell


;; Octave
;; (prelude-require-package 'ac-octave)

;; Gherkin/Cucumber
(prelude-require-package 'feature-mode)
;; Just for emacs testing
;; (prelude-require-package 'ecukes)
(prelude-require-package 'cucumber-goto-step)


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

;; https://github.com/expez/company-quickhelp
(setq company-tooltip-idle-delay 0.1)
(setq company-quickhelp-delay 0.1)
(prelude-require-package 'pos-tip)
(require 'pos-tip)
(prelude-require-package 'company-quickhelp)
(company-quickhelp-mode 1)
;; stupid thing overrids M-h
;; (eval-after-load 'company
;; '(define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin))

;; (prelude-require-package 'clippy)
;; (setq clippy-tip-show-function #'clippy-popup-tip-show)

;; (prelude-require-package 'github-issues)
(prelude-require-package 'flycheck-tip)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clojure

(eval-after-load 'clojure-mode
  '(progn
     (define-key clojure-mode-map (kbd "C-c C-h") #'clojure-cheatsheet)))

;; NOTE: also installed to ~/.lein/profiles.clj: kibit, eastwood
(prelude-require-package 'smartparens)  ; better paredit, sp-*
(prelude-require-package 'cider)
;; (prelude-require-package 'monroe)
;; (add-hook 'clojure-mode-hook 'clojure-enable-monroe)
;; (prelude-require-package 'hydra)
(prelude-require-package 'clj-refactor)
(prelude-require-package 'clojure-snippets) ; yas for clojure
(prelude-require-package 'clojure-cheatsheet)
(prelude-require-package 'flycheck-clojure)
(prelude-require-package 'company-flx)
;; Not useful; just opens in broswer
;; (prelude-require-package 'helm-clojuredocs)
;; Not needed since cljr-clean-ns
;; (prelude-require-package 'slamhound)
;; Not working
(prelude-require-package 'flycheck-joker)
(require 'flycheck-joker)
;; (prelude-require-package 'kibit-helper)
(prelude-require-package 'sotclojure)
(define-key prelude-mode-map (kbd "C-c C-n") 'flycheck-tip-cycle)
(setq error-tip-notify-keep-messages t)
;; (flycheck-tip-use-timer 'verbose)
;; (prelude-require-package 'flycheck-pos-tip)
;; (with-eval-after-load 'flycheck (flycheck-pos-tip-mode))
(prelude-require-package 'clojure-mode-extra-font-locking)

;; Log all commands for demo purposes.
;; (add-hook 'clojure-mode-hook 'command-log-mode)

;; hack to enable clj refactor
(define-key prelude-mode-map (kbd "C-c r") nil)
(global-unset-key (kbd "C-c r"))

;; Trying to get rid of the prompt to save before load.
(defun my-cider-load-buffer ()
  (save-buffer)
  (cider-load-buffer))

;; https://github.com/clojure-emacs/clj-refactor.el
(defun my-clojure-mode-hook () "Foo bar."
       (message "in my-clojure-mode-hook")
       (clj-refactor-mode 1)
       (yas-minor-mode 1) ; for adding require/use/import statements
       ;; This choice of keybinding leaves cider-macroexpand-1 unbound
       (global-set-key (kbd "M-h") 'mark-paragraph)
       (global-set-key (kbd "C-c C-k") 'my-cider-load-buffer)
       (setq cider-save-file-on-load t)
       (setq cider-prompt-for-symbol nil)
       ;; (cljr-add-keybindings-with-prefix "C-c r")
       ;; (define-key (kbd "C-c r"))
       (company-flx-mode +1)
       ;; (cljr-add-keybindings-with-prefix "C-c m")
       (cljr-add-keybindings-with-prefix "C-c r")
       (global-set-key (kbd "C-c R") 'cljr-helm)
       (global-set-key (kbd "M-J") 'sp-join-sexp) ; maybe already done by smartparens
       ;; Make similar to wrapping with M-(
       (global-set-key (kbd "M-[") (lambda () (interactive) (sp-wrap-with-pair "[")))
       ;; Overrides tmm-menubar
       (global-set-key (kbd "M-`") (lambda () (interactive) (sp-wrap-with-pair "`")))
       )
(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

(message "MDE: before eval-after-load")
(eval-after-load "clojure-mode"
  '(progn
     ;; (define-key clojure-mode-map (kbd "C-c C-h") #'clojure-cheatsheet)
     (message "MDE: in clojure eval-after-load")))

;; https://github.com/clojure-emacs/squiggly-clojure
;; (eval-after-load 'flycheck '(flycheck-clojure-setup))
(add-hook 'after-init-hook #'global-flycheck-mode)
;; (eval-after-load 'flycheck
;;   '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

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

;; (setq cider-cljs-lein-repl
;;       "(do (user/go)
;;            (user/cljs-repl))")


;; ;; Puppet
;; (prelude-require-package 'puppet-mode)
;; (prelude-require-package 'flymake-puppet)
;; (add-hook puppet-mode-hook 'flymake-puppet-load)

;; ;; Ansible
;; (prelude-require-package 'ansible)
;; (prelude-require-package 'ansible-doc)

;; ;; https://github.com/k1LoW/emacs-ansible
;; (prelude-require-package 'yaml-mode)
;; (add-hook 'yaml-mode-hook #'ansible-doc-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;; (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
;; (add-hook 'yaml-mode-hook '(lambda () (ansible 1)))
;; (add-hook 'yaml-mode-hook #'ansible-doc-mode)

;; (prelude-require-package 'company-ansible)
;; (add-to-list 'company-backends 'company-ansible)

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
  :init-value t
  :lighter " my-keys")
(my-keys-minor-mode 1)

(defun my-minibuffer-setup-hook () "Foo bar."
       (my-keys-minor-mode 0))
(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)


;; ;; JavaScript
;; (prelude-require-package 'rjsx-mode)
;; (add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))

(prelude-require-package 'json-reformat)

;; ;; Nginx
;; (prelude-require-package 'nginx-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other non-programming modes

;; http://emacs.stackexchange.com/questions/2410/how-can-i-use-emacs-to-edit-questions-answers-and-comments-here/
;; (prelude-require-package 'sx)

;; http://stackoverflow.com/questions/18812938/copy-full-file-path-into-copy-paste-clipboard
(defun clip-file ()
  "Put the current file name on the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      (file-name-directory default-directory)
                    (buffer-file-name))))
    (when filename
      (gui-select-text filename))))

(defun my-beginning-of-defun ()
  "Jump to start of name of function, since often want to search it."
  (interactive)
  (beginning-of-defun)
  (forward-word 2)
  (backward-word))
;; (global-set-key (kbd "C-M-a") 'beginning-of-defun) ; replaced
(global-set-key (kbd "C-M-a") 'my-beginning-of-defun)



(provide 'mde)

;;; mde ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
