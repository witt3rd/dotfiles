;; -*- lexical-binding: t -*-

;;
;; Emacs performance
;;
(setq gc-cons-threshold (* 500 1024 1024))

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
	   (format "%.2f seconds"
		   (float-time
		    (time-subtract after-init-time before-init-time)))
	   gcs-done))
(add-hook 'emacs-startup-hook #'efs/display-startup-time)


;;
;; Package management
;;

;; straight.el: https://github.com/raxod502/straight.el
;;
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)


;;
;; Basic UI
;;
(setq inhibit-startup-message t)        ; don't show startup screen
(scroll-bar-mode -1)                    ; don't show scroll bars
(tool-bar-mode -1)                      ; don't show tool bar
(menu-bar-mode -1)                      ; don't show menu bar
(tooltip-mode -1)                       ; don't show tool tips
(set-fringe-mode 10)                    ; give some breathing room
(setq visible-bell t)                   ; don't ding
(setq initial-scratch-message nil)      ; don't show message in scratch buffer

;; line numbering
(column-number-mode)
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)
;;(dolist (mode '(term-mode-hook
;;                eshell-mode-hook))
;;  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;
;; Editor behavior
;;
(global-set-key (kbd "<escape>")
                'keyboard-escape-quit)  ; make ESC quit prompts
(global-auto-revert-mode 1)             ; automatically reload files changed on disk
(setq custom-file null-device)          ; ignore 'customize'
(setq-default indent-tabs-mode nil)     ;


;;
;; Fonts
;;
(set-face-attribute 'default nil :font "FiraCode Nerd Font" :height 120)


;;
;; Icons
;;
(use-package all-the-icons
  :if (display-graphic-p)
  :commands all-the-icons-install-fonts
  :init
  (unless (find-font (font-spec :name "all-the-icons"))
    (all-the-icons-install-fonts t)))

(use-package all-the-icons-dired
  :if (display-graphic-p)
  :hook (dired-mode . all-the-icons-dired-mode))


;;
;; Themes
;;

(setq custom-safe-themes t)
;; assume all themes are safe

;; doom-modeline: https://github.com/seagle0128/doom-modeline
;;
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

;; doom-themes: https://github.com/hlissner/emacs-doom-themes
;;
(use-package doom-themes
  :config
  (load-theme 'doom-outrun-electric t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (setq doom-themes-treemacs-theme "doom-atom")
  (doom-themes-treemacs-config)
  (doom-themes-org-config))


;;
;; Completions
;;

;; vertico: https://github.com/minad/vertico
;;
(use-package vertico
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode)
  ;; Grow and shrink the Vertico minibuffer
  (setq vertico-resize t))

;; savehist:
;;
;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (vertico-mode))

;; marginalia: https://github.com/minad/marginalia
;;
(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

;; orderless: https://github.com/oantolin/orderless
;;
;; Optionally use the `orderless' completion style. See
;; `+orderless-dispatch' in the Consult wiki for an advanced Orderless style
;; dispatcher. Additionally enable `partial-completion' for file path
;; expansion. `partial-completion' is important for wildcard support.
;; Multiple files can be opened at once with `find-file' if you enter a
;; wildcard. You may also give the `initials' completion style a try.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch))
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

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

;; consult: https://github.com/minad/consult
;;


;; embark: https://github.com/oantolin/embark/
;;


;; which-key: https://github.com/justbur/emacs-which-key
;;
(use-package which-key
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))

;; helpful: https://github.com/Wilfred/helpful
;;
(use-package helpful)
;; Note that the built-in `describe-function' includes both functions
;; and macros. `helpful-function' is functions only, so we provide
;; `helpful-callable' as a drop-in replacement.
(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
;; Lookup the current symbol at point. C-c C-d is a common keybinding
;; for this in lisp modes.
(global-set-key (kbd "C-c C-d") #'helpful-at-point)
;; Look up *F*unctions (excludes macros).
;;
;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
;; already links to the manual, if a function is referenced there.
(global-set-key (kbd "C-h F") #'helpful-function)
;; Look up *C*ommands.
;;
;; By default, C-h C is bound to describe `describe-coding-system'. I
;; don't find this very useful, but it's frequently useful to only
;; look at interactive functions.
(global-set-key (kbd "C-h C") #'helpful-command)


;; treemacs: https://github.com/Alexander-Miller/treemacs
;;


;;
;; Programming
;;

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; lsp-mode: https://github.com/emacs-lsp/lsp-mode
;;
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t)
  :commands
  (lsp lsp-deferred))

;; lsp-ui: https://emacs-lsp.github.io/lsp-ui/
;;
(use-package lsp-ui
  :commands
  lsp-ui-mode)

;; dap-mode: https://emacs-lsp.github.io/dap-mode/
;;
(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; lsp-treemacs: https://github.com/emacs-lsp/lsp-treemacs
;;


;;
;; C/C++
;; - https://emacs-lsp.github.io/lsp-mode/tutorials/CPP-guide/
;; - https://clangd.llvm.org/
;;
;;
(add-hook 'c++-mode-hook 'lsp-deffered)

;;
;; LaTeX
;; - https://tug.org/texlive/
;;
;; texlab: https://github.com/latex-lsp/texlab
;;
(use-package doc-view)

;;
;; Python
;; - https://github.com/Microsoft/python-language-server
;;
;; lsp-python-ms: https://emacs-lsp.github.io/lsp-python-ms/
;;
(use-package lsp-python-ms
  :ensure t
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp-deferred))))  ; or lsp
(add-hook 'python-mode-hook 'lsp-deffered)
