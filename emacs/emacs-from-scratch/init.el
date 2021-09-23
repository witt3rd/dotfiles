;; -- Basic Settings --

(setq
 ;; No need to see GNU agitprop.
 inhibit-startup-screen t
 ;; No need to remind me what a scratch buffer is.
 initial-scratch-message nil
 ;; Double-spaces after periods is morally wrong.
 sentence-end-double-space nil
 ;; Never ding at me, ever.
 ring-bell-function 'ignore
 ;; Prompts should go in the minibuffer, not in a GUI.
 use-dialog-box nil
 ;; Fix undo in commands affecting the mark.
 mark-even-if-inactive nil
 ;; Let C-k delete the whole line.
 kill-whole-line t
 ;; search should be case-sensitive by default
 case-fold-search nil
 ;; File littering
 make-backup-files nil
 auto-save-default nil
 create-lockfiles nil
 )

 ;; Minimal UI chrome
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)

;; ESC quits (some) prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Typed text replaces the selection
(delete-selection-mode t)

;; Column numbers in modeline
(column-number-mode)

;; Line numbers in buffers
;; https://stackoverflow.com/questions/64730866/disable-line-numbers-but-allow-them-in-prog-mode-and-text-mode
;; modes to enable
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))
;; modes to disable
(dolist (mode '(org-mode-hook
                term-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Automatically reload files changed on disk
(global-auto-revert-mode 1)

;; Never mix tabs and spaces. Never use tabs, period.
;; We need the setq-default here because this becomes
;; a buffer-local variable when set.
(setq-default indent-tabs-mode nil)

(defalias 'yes-or-no-p 'y-or-n-p) ; Accept 'y' in lieu of 'yes'.

;; ---  Emacs Customize  ---

;; Confuse customize by randomizing its output file
(setq custom-file (make-temp-file ""))

;; Assume all themes are safe
(setq custom-safe-themes t)

;; -- Encodings --

(set-charset-priority 'unicode)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; -- Appearance --

; Font
(set-face-attribute 'default nil :font "FiraCode Nerd Font" :height 120)

; Basic theme
(load-theme 'wombat)

;; -- Package Management --

; Initialize package sources
(require 'package)

; Where to get packages from
(setq package-archives `(("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

; Make all of the packages available for use
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

; Ensure the use-package package is installed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

; Load the use-package package
(require 'use-package)
(setq use-package-always-ensure t)

;; -- Packages --

;; Enable hiding of some minor modes from mode line
(use-package diminish)

;; Show a window for input-casting
(use-package command-log-mode
  :diminish
  :config
  (setq command-log-mode-auto-show t)
  (global-command-log-mode))

;; Better mode line
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

;; Match delimiters in programming modes
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; -- Vertico --
;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)

  ;; Grow and shrink the Vertico minibuffer
  (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)
  )

;; Use the `orderless' completion style. Additionally enable
;; `partial-completion' for file path expansion. `partial-completion' is
;; important for wildcard support. Multiple files can be opened at once
;; with `find-file' if you enter a wildcard. You may also give the
;; `initials' completion style a try.
(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))


;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;; -- VCS --

(use-package magit)
