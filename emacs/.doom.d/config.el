;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Donald Thompson"
      user-mail-address "donald@witt3rd.com")

(setq doom-font (font-spec :family "JetBrains Mono" :size 16 :weight 'regular)
      doom-variable-pitch-font (font-spec :family "Cantarell" :size 18)
      doom-big-font (font-spec :family "Cantarell" :size 36 :weight 'bold))

(setq custom-safe-themes t)
(if (window-system)
    (setq doom-theme 'doom-dark+)
  (setq doom-theme 'doom-one))
(setq fancy-splash-image "~/.doom.d/doom.png")

(setq display-line-numbers-type 'relative)

(map! "C->" 'indent-rigidly-right-to-tab-stop)
(map! "C-<" 'indent-rigidly-left-to-tab-stop)

(setq custom-file null-device)

(setq projectile-project-search-path '(("~/src/witt3rd/" . 5) "~/org" ("~/dotfiles" . 0))
      projectile-auto-discover 1
 )

(after! org

  (setq org-directory "~/org/"
        org-ellipsis " ▼"
        org-hide-emphasis-markers nil
        org-agenda-files (list "~/org")
        org-log-done 'time
        org-log-into-drawer t
        )

; (defun my/org-mode-setup ()
;    (org-indent-mode 0)
;    (variable-pitch-mode 0)
;    (auto-fill-mode 0)
;    (visual-line-mode 1)
;    (dolist (face '((org-level-1 . 1.2)
;                    (org-level-2 . 1.1)
;                    (org-level-3 . 1.05)
;                    (org-level-4 . 1.0)
;                    (org-level-5 . 1.0)
;                    (org-level-6 . 1.0)
;                    (org-level-7 . 1.0)
;                    (org-level-8 . 1.0)))
;      (set-face-attribute (car face) nil :font "Cantarell" :weight 'bold :height (cdr face)))
;    (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
;    (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
;    (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
;    (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
;    (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
;    (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
;    (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
;    )
;
;  (add-hook 'org-mode-hook #'my/org-mode-setup)
  )

;(use-package! visual-fill-column
;  :after org
;  :config
;  (defun my/org-mode-visual-fill ()
;    (setq visual-fill-column-width 200
;          visual-fill-column-center-text t)
;    (visual-fill-column-mode 1)
;    )
;
;  (add-hook 'org-mode-hook #'my/org-mode-visual-fill)
;  )

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
(setq lsp-ui-mode nil)
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

  ;; TODO
  ;;:bind ([remap comment-region] . cmake-ide-compile)

  (advice-add 'cmake-ide-compile :after #'my/switch-to-compilation-window)

  (add-hook 'c++-mode-hook #'my/cmake-ide-find-project)
)

(require 'rtags)

(require 'dap-cpptools)
