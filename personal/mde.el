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
;;; Prelude original stuff

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; config changes made through the customize UI will be store here
(setq custom-file (expand-file-name "custom.el" prelude-personal-dir))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Libraries

;; f, Modern API for working with files and directories in Emacs
;; (used in one of magnars' snippets)
;; https://github.com/rejeep/f.el
;; (prelude-require-package 'f)
;; (require 'f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Look-n-Feel

;;; FONTS

;; Should set default font through OS controls to something like Avenir

;; Use monospaced font faces in current buffer
(defun my-buffer-face-mode-fixed ()
  "Sets a fixed width (monospace) font in current buffer"
  (interactive)
  (typo-mode 0)
  (setq buffer-face-mode-face '(:family "Fantasque Sans Mono" :height 100))
  (buffer-face-mode))

(defun my-buffer-face-mode-variable ()
  "Set font to a variable width (proportional) fonts in current buffer"
  (interactive)
  (setq buffer-face-mode-face '(:family "Alegreya Sans" :height 100 :width semi-condensed))
  (buffer-face-mode))
;; Font face overrides via hooks
(add-hook 'prog-mode-hook 'my-buffer-face-mode-fixed)
(add-hook 'dired-mode-hook 'my-buffer-face-mode-fixed)
(add-hook 'magit-mode-hook 'my-buffer-face-mode-fixed)
(add-hook 'eshell-mode-hook 'my-buffer-face-mode-fixed)
;; (add-hook 'markdown-mode-hook 'my-buffer-face-mode-variable)


;; Defvault to using typo mode for better/fancy typography
(typo-global-mode 1)
(add-hook 'text-mode-hook 'typo-mode)

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

;; show the cursor when moving after big movements in the window
(prelude-require-package 'beacon)
(beacon-mode +1)
(global-set-key (kbd "C-S-c") 'beacon-blink)

;; I don't think this will work since running in multi-client mode via `e'.
;; (prelude-require-package 'dashboard)
;; (dashboard-setup-startup-hook)


;;; THEMES
;; See the diff between `load-theme' and `enable-theme':
;; https://github.com/jordonbiondo/ample-theme#installation

;; https://draculatheme.com/emacs/
(prelude-require-package 'dracula-theme)
(prelude-require-package 'alect-themes)
(prelude-require-package 'busybee-theme)
(prelude-require-package 'colorless-themes)
(prelude-require-package 'tangotango-theme)
(prelude-require-package 'monokai-theme)
(prelude-require-package 'heroku-theme)
(prelude-require-package 'hemisu-theme)
(prelude-require-package 'spacemacs-theme)
(prelude-require-package 'moe-theme)
(prelude-require-package 'ample-theme)
(prelude-require-package 'doom-themes)

;; (load-theme 'busybee t)
(load-theme 'ample t t)
(load-theme 'ample-flat t t)
(load-theme 'ample-light t t)
(enable-theme 'ample)
;; (load-theme 'tangotango t)
;; (enable-theme 'tangotango)
;; (setq prelude-theme 'monokai-theme)
;; (load-theme 'monokai t)
;; (enable-theme 'monokai)
;; (load-theme 'dracula t)
;; (load-theme 'doom-one t)

;; Theme overrides
;; (set-face-attribute 'region nil :background "#999")


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
;; (prelude-require-package 'crosshairs)
;; (column-highlight-mode t)

;; Don't seem to do a lot without some real study/effort
;; (prelude-require-package 'highlight)
;; (prelude-require-package 'highlight-quoted)

;; Make the active window more visible than others
;; https://github.com/yoshida-mediba/hiwin-mode
;; Ugh, make cider repl go blank when switching out
;; (prelude-require-package 'hiwin)
;; (hiwin-activate)
;; Set face color with customize
;; (set-face-background 'hiwin-face "gray80")

;; Dim inactive (and highlight active) buffers. Works way better than hiwin!
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
;; Don't really need subword mode since getting good at `sp' M-C
;; (add-hook 'prog-mode-hook 'superword-mode)
;; (superword-mode 1)

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
(global-set-key (kbd "C-S-x") 'avy-goto-word-1)
;; (global-set-key (kbd "C-S-x") 'crux-switch-to-previous-buffer)

;; ISpell (I)
(global-set-key (kbd "C-S-i") 'flycheck-next-error)


;; http://lotabout.me/orgwiki/emacs-helm.html
;; (global-set-key (kbd "C-S-h") 'helm-command-prefix)
;; sHell (H)
(global-set-key (kbd "C-S-h") 'eshell)
;; Speed up helm? https://github.com/emacs-helm/helm/wiki/FAQ#slow-frame-and-window-popup-in-emacs-26
(setq x-wait-for-event-timeout nil)

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

(prelude-require-package 'yasnippet)
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
;; https://en.wikipedia.org/wiki/Relational_algebra
(defun configure-prettify-symbols-alist ()
  "Set prettify symbols alist."
  (setq prettify-symbols-alist '(
                                 ;; ("lambda" . ?λ)
                                 ;; ("map" . ?↦)
                                 ("map" . ?a)
                                 ;; ("defn" . ?f)
                                 ;; ("->" . ?→)
                                 ;; ("#{" . ?ε)
                                 ;; ("->>" . ?↠)
                                 ;; ("=>" . ?⇒)
                                 ;; ("/=" . ?≠)
                                 ;; ("!=" . ?≠)
                                 ;; ("<=" . ?≤)
                                 ;; (">=" . ?≥)
                                 ;; ("and" . ?∧)
                                 ;; ("or" . ?∨)
                                 ("not" . ?¬))))

;; Fancy lambda etc
(global-prettify-symbols-mode t)

;; (add-hook 'emacs-lisp-mode-hook 'configure-prettify-symbols-alist)
(add-hook 'clojure-mode-hook 'configure-prettify-symbols-alist)

;; (add-hook 'prog-mode-hook 'highlight-numbers-mode)
;; (add-hook 'prog-mode-hook 'configure-prettify-symbols-alist)

;; ;; Make number colorful.
;; (prelude-require-package 'highlight-numbers)
;; (add-hook 'prog-mode-hook 'highlight-numbers-mode)

;; Jump to open file in neotree
(setq neo-smart-open t)
;; dir Listing (L)
(global-set-key (kbd "C-S-l") 'neotree-toggle)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

;; (prelude-require-package 'ivy)


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

;; (require 'key-chord)
;; (key-chord-define-global "KK" 'aw-flip-window)

;; Not sure where this one is coming from but gets in the way every
;; time "clj" is typed
;; (key-chord-define-global "lj" nil)

(global-set-key (kbd "C-<tab>") 'aw-flip-window)
(global-set-key (kbd "M-<tab>") 'ace-window)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
;; Planck arrows
;; (global-set-key (kbd "C-S-f") 'windmove-right)
;; (global-set-key (kbd "C-S-s") 'windmove-left)
;; (global-set-key (kbd "C-S-e") 'windmove-up)
;; (global-set-key (kbd "C-S-d") 'windmove-down)
(global-set-key (kbd "M-<right>") 'windmove-right)
(global-set-key (kbd "M-<left>") 'windmove-left)
(global-set-key (kbd "M-<up>") 'windmove-up)
(global-set-key (kbd "M-<down>") 'windmove-down)
;; (define-key prelude-mode-map "<right>" nil)

(global-set-key (kbd "C-S-s") 'isearch-forward-symbol-at-point)

;; Disable guru from touching these.
(setq prelude-guru nil)
(global-set-key (kbd "C-<right>") 'windmove-right)
(global-set-key (kbd "C-<left>") 'windmove-left)
(global-set-key (kbd "C-<up>") 'windmove-up)
(global-set-key (kbd "C-<down>") 'windmove-down)

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
(defun kill-window-balancedly ()
  (interactive)
  (kill-current-buffer)
  (delete-window)
  (balance-windows))
;; (global-set-key (kbd "C-z")   'delete-window-balancedly)
;; Background window (Z: like shell's C-z)
(global-set-key (kbd "C-S-z") 'delete-window-balancedly)
;; Kill (K)
(global-set-key (kbd "C-S-k") 'kill-window-balancedly)

;; Buffers (B)
(global-set-key (kbd "C-S-b") 'helm-mini)


;; Window buffer switching (O: Only)
(global-set-key (kbd "C-S-o") 'delete-other-windows) ; think "Only"
;; Just use C-c left-arrow
;; (global-set-key (kbd "C-S-g") 'winner-undo)
;; (global-set-key (kbd "C-S-+") 'balance-windows)

;; Should change focus to new window.
(defun split-window-balancedly ()
  (interactive)
  (split-window-horizontally)
  (balance-windows)
  (other-window 1)
  ;; (helm-projectile-find-file)
  )
;; (global-set-key (kbd "C-S-n") 'split-window-horizontally)
;; New window (N)
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
(global-set-key (kbd "C-S-E") 'scroll-up-stay)
(global-set-key (kbd "C-S-Y") 'scroll-down-stay)
;; (global-set-key (kbd "C-y") 'yank)
;; (global-set-key (kbd "C-t") 'transpose-chars)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Packages

(prelude-require-package 'lorem-ipsum)

;; HTTP headers, media-types, methods, relations and status codes, all
;; summarized and linking to their specification.
;; https://github.com/for-GET/know-your-http-well
(prelude-require-package 'know-your-http-well)

;; Replacement for package-list-packages
;; (prelude-require-package 'paradox)
;; (paradox-enable)
;; (setq paradox-github-token (cadr (auth-source-user-and-password "api.github.com" "MicahElliott^paradox")))

;; url view
;; (global-set-key (kbd "C-c u") (lambda () (interactive) (browse-url-firefox)))
;; (global-set-key (kbd "C-c u") 'browse-url-firefox)
(global-set-key (kbd "C-c u") 'browse-url-chrome)

;; Camel, Kebab cases
;; https://stackoverflow.com/a/27422814/326516
(prelude-require-package 'string-inflection)
(global-set-key (kbd "C-c C") 'string-inflection-camelcase)        ;; Force to CamelCase
(global-set-key (kbd "C-c L") 'string-inflection-lower-camelcase)  ;; Force to lowerCamelCase
(global-set-key (kbd "C-c J") 'string-inflection-java-style-cycle) ;; Cycle through Java styles
(global-set-key (kbd "C-c K") 'string-inflection-kebab-case) ;; Cycle through Java styles


(defun jump-to-bottom ()
  (interactive)
  (move-to-window-line-top-bottom)
  (move-to-window-line-top-bottom)
  )

;; smartparens overrides M-r, so changing default
(global-set-key "\M-R" 'move-to-window-line-top-bottom)
(global-set-key "\M-\C-R" 'jump-to-bottom)
;; Since already holding M-S-R, enable recenter (usually C-l) to also be M-S
(global-set-key (kbd "M-L") 'recenter-top-bottom)

;; Multiple cursors: https://github.com/magnars/multiple-cursors.el
(prelude-require-package 'multiple-cursors)
;; Multiple Cursors (C)
;;(global-set-key (kbd "C-S-c") 'mc/edit-lines)
;; (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Tramp
(tramp-set-completion-function
 "ssh" '((tramp-parse-sconfig "~/.ssh/config")
         (tramp-parse-sconfig "~/proj/Membean/provn/ansible/ssh-inventory.config")))

(prelude-require-package 'helm-descbinds)
;; (require 'helm-descbinds)
(helm-descbinds-mode)
;; which-key is the active help completer!!
;; thread says Helm Descbinds is better than guide-key:
;; https://github.com/bbatsov/prelude/issues/481
;; ;;; Guide Key: completable help menu
;; ;; https://github.com/kai2nenobu/guide-key
;; (prelude-require-package 'guide-key)
;; (require 'guide-key)
;; (setq guide-key/guide-key-sequence t)
;; (guide-key-mode 1)  ; Enable guide-key-mode

(setq multi-term-program "/bin/zsh")
(setq shell-file-name "zsh")
(prelude-require-package 'multi-term)
;; (require 'multi-term)

(prelude-require-package 'adoc-mode)
;; (require 'adoc-mode)
(add-to-list 'auto-mode-alist '("\\.asc$" . adoc-mode))
                                        ;(add-to-list 'auto-mode-alist (cons "\\.txt\\'" 'adoc-mode))
(add-hook 'adoc-mode-hook (lambda () (buffer-face-mode t)))

(add-to-list 'auto-mode-alist '("\\.tsx$" . typescript-mode))

;;; Commenter: still stuggling with this
(prelude-require-package 'comment-dwim-2)
(global-set-key (kbd "M-;") 'comment-dwim-2)


(prelude-require-package 'uuidgen)

(prelude-require-package 'epoch-view)
(epoch-view-mode t)



;; (defun clj-find-ignore-macro ()
;;   (interactive)
;;   ;; (while (not (= (char-after) )))
;;   (save-excursion
;;     (while (and (sp-backward-up-sexp) (not (= (char-before) ?_)))
;;       (clj-find-ignore-macro)
;;       (print "going up")
;;       ;; (if (not (= (char-before) ?_))
;;       ;; (clj-find-ignore-macro)))
;;       )))
;; (global-set-key (kbd "C-c ;") 'clj-find-ignore-macro)

;; (defun clj-comment ()
;;   "Do a Clojure-style `#_' un/commenting of “ignore” macro for sexps.
;; Somewhat helpful for debugging.
;; Requires smartparens (for now)."
;;   (interactive)
;;   (save-excursion
;;     (when (not (= (char-after) ?\())
;;       ;; (backward-sexp) ; not far enough and weird error sometimes
;;       (sp-backward-up-sexp)) ; smartparens is easier
;;     (if (= (char-before) ?_)
;;         (delete-char -2)
;;       (insert "#_"))))
;; (global-set-key (kbd "C-c ;") 'clj-comment)

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
(define-key prelude-mode-map (kbd "C-c C-g")  nil)
;; maGit (G)
(global-set-key (kbd "C-S-g") 'magit-status)
(global-set-key (kbd "C-c C-g B") 'github-browse-file)
(global-set-key (kbd "C-c C-g a") 'vc-annotate)
(global-set-key (kbd "C-c C-g b") 'magit-blame)
(global-set-key (kbd "C-c C-g f") 'github-browse-file)
(global-set-key (kbd "C-c C-g g") 'magit-status)
(global-set-key (kbd "C-c C-g l") 'magit-log-buffer-file)
(global-set-key (kbd "C-c C-g m") 'diff-hl-mark-hunk)
(global-set-key (kbd "C-c C-g n") 'diff-hl-next-hunk)
(global-set-key (kbd "C-c C-g p") 'diff-hl-previous-hunk)
(global-set-key (kbd "C-c C-g r") 'diff-hl-revert-hunk)
(global-set-key (kbd "C-c C-g t") 'git-timemachine-toggle)
(global-set-key (kbd "C-c C-g t") 'git-timemachine-toggle)
;; (global-set-key (kbd "C-c C-g p") 'git-messenger:popup-message)

;;; Git stuff
(prelude-require-package 'git-link)
(prelude-require-package 'github-browse-file)
(prelude-require-package 'github-pullrequest)
(prelude-require-package 'forge)
(with-eval-after-load 'magit (require 'forge))
;; (prelude-require-package 'github-review)
;; (prelude-require-package 'magit-circleci) ; doesn't really work

;; Add a prefix message (intent or ticket number) to all commits.
;; https://github.com/kidd/git-msg-prefix.el
;; (prelude-require-package 'git-msg-prefix)
;; (add-hook 'git-commit-mode-hook 'git-msg-prefix)
;; (setq git-msg-prefix-input-method q'helm-comp-read)

;; Ancient
;; (prelude-require-package 'mo-git-blame)


(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
;; https://magit.vc/manual/magit/Performance.html#Performance
(setq magit-refresh-status-buffer nil)

;; Perf: https://magit.vc/manual/magit/Performance.html#Performance
;; Whoa, needed for diff-hl-mode and avoiding another weird startup error!!
;; (setq vc-handled-backends nil)

;; Projectile changed!
;; (global-set-key (kbd "C-c p p") 'git-messenger:popup-message)
;; (setq projectile-keymap-prefix (kbd "C-c C-p"))
;; (define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)
;; (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
;; (define-key projectile-mode-map (kbd "C-c C-f") 'projectile-command-map)
;; Projectile (P)
(define-key projectile-mode-map (kbd "C-S-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-S-p s") 'helm-projectile-ag)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-mode
;; https://www.emacswiki.org/emacs/OrgJournal
(prelude-require-package 'org-journal)
(setq org-journal-dir "~/doc/journal/")
;; (require 'org-journal)
;; Seems to do too much; might have failed install
;; (prelude-require-package 'ox-reveal)
;; (prelude-require-package 'htmlize)
(prelude-require-package 'epresent)
;; (prelude-require-package 'ob-clojurescript)
(prelude-require-package 'org-tree-slide)
;; (prelude-require-package 'org-bullets)


;; (define-key org-mode-map (kbd "M-}") nil)
;; (define-key org-mode-map (kbd "C-TAB") nil) ; not working

;; https://stackoverflow.com/questions/4333467/override-ctrl-tab-in-emacs-org-mode
(add-hook 'org-mode-hook
          '(lambda ()
             (define-key org-mode-map [(control tab)] nil)
             (define-key org-mode-map (kbd "C-M-u") 'org-up-element)
             (define-key org-mode-map (kbd "C-M-d") 'org-down-element)
             (define-key org-mode-map (kbd "C-M-f") 'org-forward-element)
             (define-key org-mode-map (kbd "C-M-b") 'org-backward-element)
             (define-key org-mode-map (kbd "M-}") 'beginning-of-buffer)))


(global-set-key (kbd "C-c C-x l") 'org-toggle-link-display)

(prelude-require-package 'org-preview-html)
(prelude-require-package 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

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
(setq explicit-shell-file-name "/bin/zsh")

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


;; Hide-Show (V: visible), like folding
(add-hook 'prog-mode-hook 'hs-minor-mode)
(global-set-key (kbd "C-S-v H") 'hs-hide-all)
(global-set-key (kbd "C-S-v S") 'hs-show-all)
(global-set-key (kbd "C-S-v h") 'hs-hide-block)
(global-set-key (kbd "C-S-v s") 'hs-show-block)
(global-set-key (kbd "C-S-v t") 'hs-toggle-hiding)
(global-set-key (kbd "C-S-v v") 'hs-toggle-hiding)



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
;; (key-chord-define-global "jj" 'avy-goto-word-1)
;; (key-chord-define-global "kk" 'avy-goto-char-2)
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


;; Highlight/navigate TODO, FIXME, etc
;; C-c # ...
;; (prelude-require-package 'comment-tags)
;; (add-hook 'prog-mode-hook 'comment-tags-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Language Setups

;;; HTML
;; (prelude-require-package 'zencoding-mode)
;; (add-hook 'sgml-mode-hook 'zencoding-mode) ;; Auto-start on any markup modes
(prelude-require-package 'emmet-mode)
(prelude-require-package 'company-web)
(prelude-require-package 'helm-emmet)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)
(setq emmet-move-cursor-between-quotes t)

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


;;; Python
;; (prelude-require-package 'conda)

;; Code completion, navigation, interactive shell, virtualenv, syntax
;; checking, docs, debugger, snippets
(prelude-require-package 'elpy)
(elpy-enable)
;; (setq elpy-rpc-backend "jedi")

;; (prelude-require-package 'jedi)
;; autocompletion
;; (prelude-require-package 'company-jedi)
;; (defun my/python-mode-hook ()
  ;; (add-to-list 'company-backends 'company-jedi))
;; (add-hook 'python-mode-hook 'my/python-mode-hook)

;; (setq-default py-shell-name "ipython")
(setq py-force-py-shell-name-p t)
;; (setq-default py-which-bufname "IPython")
;; use the wx backend, for both mayavi and matplotlib
(setq py-python-command-args '("--gui=wx" "--pylab=wx" "-colors" "Linux"))

;; switch to the interpreter after executing code
(setq py-shell-switch-buffers-on-execute-p t)
(setq py-switch-buffers-on-execute-p t)
;; don't split windows
(setq py-split-windows-on-execute-p nil)
;; try to automagically figure out indentation
(setq py-smart-indentation t)

;; (prelude-require-package 'anaconda-mode)
;; (add-hook 'python-mode-hook 'anaconda-mode)
;; (add-hook 'python-mode-hook 'anaconda-eldoc-mode)

;; (setq python-shell-interpreter "ipython")

;; (prelude-require-package 'pippel)
;; (prelude-require-package 'pipenv)
;; (setq pipenv-projectile-after-switch-function
      ;; #'pipenv-projectile-after-switch-extended)

(prelude-require-package 'pyenv-mode)
(prelude-require-package 'pyenv-mode-auto)


(prelude-require-package 'py-autopep8)

(prelude-require-package 'pylint)
(add-hook 'python-mode-hook 'pylint-add-menu-items)
(add-hook 'python-mode-hook 'pylint-add-key-bindings)

(prelude-require-package 'flycheck-pycheckers)
(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup))

;; (prelude-require-package 'importmagic) ; C-c C-l
;; (define-key importmagic-mode-map (kbd "C-c C-f") 'importmagic-fix-symbol-at-point)

;; (prelude-require-package 'ein)

(prelude-require-package 'python-pytest)
(prelude-require-package 'pytest)


;;; Dart
(prelude-require-package 'dart-mode)
;; (prelude-require-package 'lsp-dart)


;;; Ruby
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
;; (global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-x k") 'kill-current-buffer)
(global-set-key (kbd "C-c k") 'kill-current-buffer)

;; ;;; Haskell
;; (prelude-require-package 'ghc)
;; (prelude-require-package 'flymake-hlint)
;; ;; (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-tag-find)

;; (prelude-require-package 'intero)

;; ;;; Docker
(prelude-require-package 'docker)
(prelude-require-package 'dockerfile-mode)
(prelude-require-package 'docker-compose-mode)

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

;; https://emacs.stackexchange.com/questions/3038/using-a-different-font-for-each-major-mode

;; LiveScript
;; (prelude-require-package 'livescript-mode)

(prelude-require-package 'dot-mode)
;; (require 'dot-mode)
;; https://www.emacswiki.org/emacs/dot-mode.el
;; C-.  C-M-.  C-c.
(autoload 'dot-mode "dot-mode" nil t)
;; (dot-mode t)
(global-dot-mode t)

(prelude-require-package 'mode-icons)

;; https://github.com/domtronn/all-the-icons.el
(prelude-require-package 'all-the-icons)

;; https://github.com/jaypei/emacs-neotree
(prelude-require-package 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq neo-theme (if window-system 'icons 'arrow))
(setq neo-theme 'icons)
;; open neotree with current file as root
(setq neo-smart-open t)

(prelude-require-package 'all-the-icons-dired)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

;; Gherkin/Cucumber
(prelude-require-package 'feature-mode)
;; Just for emacs testing
;; (prelude-require-package 'ecukes)
(prelude-require-package 'cucumber-goto-step)


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

;; FIXME: need something like this to work to avoid opening new
;; windows all the time for docs
;; (pos-tip-show "foo bar")
(defun my-describe-function (function)
  "Display the full documentation of FUNCTION (a symbol) in tooltip."
  (interactive (list (function-called-at-point)))
  (if (null function)
      (pos-tip-show
       "** You didn't specify a function! **" '("red"))
    (pos-tip-show
     (with-temp-buffer
       (let ((standard-output (current-buffer))
             (help-xref-following t))
         (prin1 function)
         (princ " is ")
         (describe-function-1 function)
         (buffer-string)))
     nil nil nil 0)))
;; (define-key emacs-lisp-mode-map (kbd "C-;") 'my-describe-function)
;; (global-set-key (kbd "C-;") 'my-describe-function)



;; (prelude-require-package 'clippy)
;; (setq clippy-tip-show-function #'clippy-popup-tip-show)

;; (prelude-require-package 'github-issues)
(prelude-require-package 'flycheck-tip)

;; NOOOO! These kill helm and who knows what else!!!
;; https://github.com/cpitclaudel/compact-docstrings
;; (prelude-require-package 'compact-docstrings)
;; (add-hook 'after-init-hook #'global-compact-docstrings-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clojure

;; NOTE: also installed to ~/.lein/profiles.clj: kibit, eastwood
(prelude-require-package 'smartparens)  ; better paredit, sp-*
(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
(prelude-require-package 'cider)
(prelude-require-package 'cider-eval-sexp-fu)

(prelude-require-package 'clj-refactor)
(prelude-require-package 'clojure-snippets) ; yas for clojure
;; (prelude-require-package 'clojure-cheatsheet)
(prelude-require-package 'flycheck-clojure)
(prelude-require-package 'company-flx)

(prelude-require-package 'flycheck-joker)
(require 'flycheck-joker)
(prelude-require-package 'kibit-helper)
(prelude-require-package 'flycheck-clj-kondo)
(prelude-require-package 'sotclojure)
(define-key prelude-mode-map (kbd "C-c C-n") 'flycheck-tip-cycle)
(setq error-tip-notify-keep-messages t)
(prelude-require-package 'clojure-mode-extra-font-locking)

;; For kondo: https://github.com/borkdude/flycheck-clj-kondo#multiple-linters
(require 'flycheck-clj-kondo)
(dolist (checker '(clj-kondo-clj clj-kondo-cljs clj-kondo-cljc clj-kondo-edn))
  (setq flycheck-checkers (cons checker (delq checker flycheck-checkers))))
(dolist (checkers '((clj-kondo-clj . clojure-joker)
                    (clj-kondo-cljs . clojurescript-joker)
                    (clj-kondo-cljc . clojure-joker)
                    (clj-kondo-edn . edn-joker)))
  (flycheck-add-next-checker (car checkers) (cons 'error (cdr checkers))))

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
       (setq cider-repl-pop-to-buffer-on-connect 'display-only)
       (setq cider-repl-result-prefix ";; => ")
       (setq cider-save-file-on-load t)
       (setq cider-prompt-for-symbol nil)
       ;; (cljr-add-keybindings-with-prefix "C-c r")
       (cljr-add-keybindings-with-prefix "C-S-r")
       (cljr-add-keybindings-with-prefix "C-c m")
       (global-set-key (kbd "C-c R") 'cljr-helm)
       ;; (global-set-key (kbd "C-S-r") 'cljr-helm)
       (global-set-key (kbd "C-c r") 'cljr-helm)
       ;; (global-set-key (kbd "C-S-T") 'cider-test-commands-map)
       ;; Disable flycheck next error in favor of Cider
       (define-key prelude-mode-map (kbd "C-c C-n")  nil)
       (global-set-key (kbd "C-c C-n") 'cider-ns-map)
       (define-key prelude-mode-map (kbd "C-c C-p")  nil)
       (global-unset-key (kbd "C-c C-p"))
       (global-set-key (kbd "C-c C-p") 'cider-inspect)
       ;; (define-key (kbd "C-c r"))
       (company-flx-mode +1)
       (global-set-key (kbd "M-J") 'sp-join-sexp) ; maybe already done by smartparens
       ;; Make similar to wrapping with M-(
       (global-set-key (kbd "M-[") (lambda () (interactive) (sp-wrap-with-pair "[")))
       ;; Overrides tmm-menubar
       (global-set-key (kbd "M-`") (lambda () (interactive) (sp-wrap-with-pair "`")))
       )
(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)
(add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)

(eval-after-load "clojure-mode"
  '(progn
     ;; (define-key clojure-mode-map (kbd "C-c C-h") #'clojure-cheatsheet)
     (message "MDE: in clojure eval-after-load")))

;; https://github.com/clojure-emacs/squiggly-clojure
; (eval-after-load 'flycheck '(flycheck-clojure-setup))
(add-hook 'after-init-hook #'global-flycheck-mode)
(prelude-require-package 'flycheck-pos-tip)

;; http://stackoverflow.com/questions/23766483/emacs-cider-clojure-auto-complete-how-to-get-the-docstring
(setq ac-delay 0.1)
(setq ac-quick-help-delay 0.1)

;; Simply bind `cljr-helm` to a key (I'd suggest C-c r) in Clojure
;; mode, and you're ready to go.
(prelude-require-package 'cljr-helm)
(prelude-require-package 'discover-clj-refactor)

;; Use imenu to display list of functions.
(global-set-key (kbd "C-c i") 'helm-semantic-or-imenu)

(prelude-require-package 'ac-cider)
(prelude-require-package 'helm-cider)

(defun my-cider-find-var (arg)
  (interactive "p")
  (cider-find-var arg)
  (recenter-top-bottom))

(defun my-create-cider-repl-window ()
  "Create a new right-most window with cider repl jacked-in."
  ;; Manually go to right-most window
  (interactive)
  (split-window-balancedly)
  ;; open core.clj
  (cider-jack-in-clj nil)
  (delete-window-balancedly)
  (crux-switch-to-previous-buffer))
;; (global-set-key (kbd "C-S-x") 'my-create-cider-repl-window)

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


;;; JavaScript
(prelude-require-package 'json-reformat)

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
  (beginning-of-line)
  (beginning-of-defun)
  (forward-word 2)
  (backward-word))
;; (global-set-key (kbd "C-M-a") 'beginning-of-defun) ; replaced
(global-set-key (kbd "C-M-a") 'my-beginning-of-defun)

;; Copy filename to clipboard
;; https://emacsredux.com/blog/2013/03/27/copy-filename-to-the-clipboard/
(defun my-copy-filename ()
  "Copy current buffer file name to clipboard"
  (interactive)
  (let ((fname (buffer-file-name)))
    (kill-new fname)
    (message "Copied buffer file name '%s' to the clipboard." fname)))
(global-set-key (kbd "C-c c") 'my-copy-filename)

(defun my-git-filename ()
  "Copy filename path relative to repo."
  (let ((root  (replace-regexp-in-string "~/" "/" (vc-root-dir))) ; "/.emacs.d/" "/proj/fk12/"
        (fname (buffer-file-name))
        ;; (reporoot (vc-git-push))
        ) ; "/Users/micah.elliott/.emacs.d/personal/mde.el"
    (replace-regexp-in-string (concat ".*" root) "" fname)))

(prelude-require-package 'dumb-jump)

(defun cider-or-dumb-jump ()
  (interactive)
  (if (cider-connected-p)
      (cider-find-var)
    (dumb-jump-go))
  (recenter-top-bottom))

(add-hook 'clojure-mode-hook
          (lambda ()
            (local-set-key (kbd "M-.") 'cider-or-dumb-jump)))

(provide 'mde)

;;; mde ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
