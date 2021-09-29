;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(setq user-full-name "Donald Thompson"
      user-mail-address "donald@witt3rd.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq
 doom-font (font-spec :family "FiraCode Nerd Font Mono" :size 16 :weight 'semi-light)
 doom-variable-pitch-font (font-spec :family "FiraCode Nerd Font" :size 16)
 doom-big-font (font-spec :family "FiraCode Nerd Font" :size 36 :weight 'bold))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq custom-safe-themes t)
(if (window-system)
    (setq doom-theme 'doom-outrun-electric)
  (setq doom-theme 'doom-one))
(setq fancy-splash-image "~/.doom.d/doom.png")

; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(setq custom-file null-device)

(setq
 projectile-project-search-path '(("~/src/witt3rd/" . 5) "~/org" ("~/dotfiles" . 0))
 projectile-auto-discover 1
 )

(setq
 org-directory "~/org/"
 )

(use-package! org-super-agenda
  :after org-agenda
  :init
  (setq org-super-agenda-groups '((:name "Today"
                                   :time-grid t
                                   :scheduled today)
                                  (:name "Due Today"
                                   :deadline today)
                                  (:name "Important"
                                   :priority "A")
                                  (:name "Overdue"
                                   :deadline past)
                                  (:name "Due soon"
                                   :deadline future)
                                  (:name "Big Outcomes"
                                   :tag "bo")))
  :config
  (org-super-agenda-mode)
  )

(setq lsp-clients-clangd-args '("-j=3"
                                "--background-index"
                                "--clang-tidy"
                                "--completion-style=detailed"
                                "--header-insertion=iwyu"
                                "--header-insertion-decorators=0"))
(after! lsp-clangd (set-lsp-priority! 'clangd 2))

(after! projectile
  (defun my/cmake-ide-find-project ()
    "Finds the directory of the project for cmake-ide."
    (with-eval-after-load 'projectile
      (setq cmake-ide-project-dir (projectile-project-root))
      (setq cmake-ide-build-dir (concat cmake-ide-project-dir "build")))
    (setq cmake-ide-compile-command
          (concat "cd " cmake-ide-build-dir " && cmake .. && make"))
    (cmake-ide-load-db))
  (defun my/switch-to-compilation-window ()
    "Switches to the *compilation* buffer after compilation."
    (other-window 1))
  ;;  :bind ([remap comment-region] . cmake-ide-compile)
  (advice-add 'cmake-ide-compile :after #'my/switch-to-compilation-window)
  (add-hook 'c++-mode-hook #'my/cmake-ide-find-project)
  )

(require 'rtags)

(require 'dap-cpptools)
