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
(column-number-mode)
(global-display-line-numbers-mode t)
(setq display-line-numbers 'relative)


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
  (vertico-mode))

;; savehist:
;;
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

;; consult: https://github.com/minad/consult
;;


;; embark: https://github.com/oantolin/embark/
;;


;; orderless: https://github.com/oantolin/orderless
;;
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
