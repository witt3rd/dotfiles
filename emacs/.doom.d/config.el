;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Donald Thompson"
      user-mail-address "donald@witt3rd.com")

(setq
 doom-font (font-spec :family "FiraCode Nerd Font Mono" :size 16 :weight 'semi-light)
 doom-variable-pitch-font (font-spec :family "FiraCode Nerd Font" :size 16)
 doom-big-font (font-spec :family "FiraCode Nerd Font" :size 36 :weight 'bold))

(setq custom-safe-themes t)
(if (window-system)
    (setq doom-theme 'doom-outrun-electric)
  (setq doom-theme 'doom-one))
(setq fancy-splash-image "~/.doom.d/doom.png")

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
