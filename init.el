(setq custom-file "~/.emacs.d/custom.el")

(push '(fullscreen . maximized) default-frame-alist)

(defvar my/gc-cons-threshold-orig gc-cons-threshold)
(defvar my/gc-cons-percentage-orig gc-cons-percentage)
(setq gc-cons-threshold (* 256 1024 1024))
(setq read-process-output-max (* 4 1024 1024))
(setq comp-async-jobs-number 8)
(setq comp-deferred-compilation t)
(setq gc-cons-percentage 0.6)

(defvar my/file-name-handler-alist-orig file-name-handler-alist)
(setq file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold my/gc-cons-threshold-orig
                  gc-cons-percentage my/gc-cons-percentage-orig
                  file-name-handler-alist my/file-name-handler-alist-orig)
            (run-with-idle-timer 5 nil #'garbage-collect)))

(setq read-process-output-max (* 1024 1024))

(setq auto-save-default nil)
(setq make-backup-files nil)

(when (boundp 'native-comp-async-report-warnings-errors)
  (setq native-comp-async-report-warnings-errors nil))
(when (boundp 'native-comp-deferred-compilation)
  (setq native-comp-deferred-compilation t))

(setq use-package-verbose nil)

(with-eval-after-load 'tramp
  (setq tramp-ssh-controlmaster-options
        "-o ControlMaster=auto -o ControlPath='~/.ssh/tramp-%%r@%%h:%%p' -o ControlPersist=60s")
  (setq tramp-auto-save-directory (expand-file-name "tramp-autosave" user-emacs-directory)))

(run-with-idle-timer 10 t #'garbage-collect)

(setq tab-bar-close-button-show nil
      tab-bar-new-button-show nil)

(set-face-attribute 'tab-bar nil :height 160)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(package-initialize)

;; Maple Mono 17
(set-frame-font "FantasqueSansM Nerd Font 20" nil t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

(setq frame-background-mode 'dark)

(require 'grep)
(setq grep-command "grep -nH --color=auto -r -e ")

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)

(setq-default fill-column 80)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

(fringe-mode '(8 . 8))

(setq-default indent-tabs-mode nil)     ;; expandtab
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default standard-indent 4)
(electric-indent-mode 1)                ;; smart indent-ish
(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)
(setq display-time-default-load-average nil)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(save-place-mode 1)
(global-hl-line-mode 1)

(setq savehist-additional-variables '(register-alist))
(savehist-mode 1)
(which-key-mode 1)
(editorconfig-mode 1)

(require 'ido)
(ido-mode 1)
(ido-everywhere 1)
(setq ido-enable-flex-matching t      ;; fuzzy matching
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-save-directory-list-file
      (expand-file-name "ido.last" user-emacs-directory))

;; (global-completion-preview-mode 1)
(fido-vertical-mode 1)
(setq completion-styles '(basic flex) ;; fuzzy fido
      completions-sort 'historical
      completions-max-height 20
      completions-format 'one-column
      completion-auto-select t
      completion-auto-help 'visible)

(defun scroll-half-page-down ()
  "scroll down half the page"
  (interactive)
  (scroll-down (/ (window-body-height) 2)))
(defun scroll-half-page-up ()
  "scroll up half the page"
  (interactive)
  (scroll-up (/ (window-body-height) 2)))

;; Keymaps

(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (beginning-of-line)
    (when (or (> arg 0) (not (bobp)))
      (forward-line)
      (when (or (< arg 0) (not (eobp)))
        (transpose-lines arg))
      (forward-line -1)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(general-define-key
 :keymaps 'global
 "C-v" #'scroll-half-page-up
 "M-v" #'scroll-half-page-down
 "C-x _" #'maximize-window
 "C-c c" #'project-compile
 "C-c t" (lambda () (interactive) (tab-new) (vterm) (recenter))
 "C-c C-b" 'grep
 "C-c C" #'compile
 "C-c C-l" (lambda () (interactive) (duplicate-line) (next-line))
 "M-p" #'move-text-up
 "M-n" #'move-text-down)

;; Theme

(use-package almost-mono-themes
  :ensure t
  :config
  (add-to-list 'default-frame-alist '(undecorated . t))
  (load-theme 'almost-mono-black t)
  (when (memq 'almost-mono-black custom-enabled-themes)
    (custom-set-faces
     '(line-number ((t (:foreground "#3a3f5a" :background nil))))
     '(line-number-current-line ((t (:foreground "#c5c9c5" :background nil :weight bold))))
     '(tab-bar ((t (:background "#000000" :foreground "#c0caf5" :box nil))))
     '(tab-bar-tab ((t (:background "#222222" :foreground "#c5c9c5" :weight bold))))
     '(tab-bar-tab-inactive ((t (:background nil :foreground "#565f89" :box nil)))))
    (set-frame-parameter nil 'alpha-background 70)
    (add-to-list 'default-frame-alist '(alpha-background . 70))
    (add-to-list 'default-frame-alist '(background-color . "#000000"))
    (set-face-background 'default "#000000")))

;; Plugins

(use-package vterm
  :ensure t
  :commands vterm)

(use-package undo-tree
  :init
  (setq undo-tree-visualizer-timestamps t
        undo-tree-history-directory-alist `(("." . ,(expand-file-name "undo-history" user-emacs-directory)))
        undo-tree-auto-save-history t
        undo-limit 10000000
        undo-strong-limit 10000000)
  :config
  (global-undo-tree-mode))

(use-package magit
  :ensure t
  :commands (magit-status magit-dispatch)
  :bind (("C-x g" . magit-status)))

(use-package expand-region
  :ensure t
  :bind (("M-h" . er/expand-region)))

(use-package multiple-cursors
  :vc (:url "https://github.com/magnars/multiple-cursors.el"
            :rev :newest
            :branch "master")
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-S-c C-S-c" . mc/edit-lines)
         ("C-M->" . mc/skip-to-next-like-this)
         ("C-M-<" . mc/skip-to-previous-like-this)))

(use-package avy
  :vc (:url "https://github.com/abo-abo/avy"
            :rev :newest
            :branch "master")
  :bind (("C-'" . avy-goto-word-1)))

(use-package yasnippet
  :vc (:url "https://github.com/joaotavora/yasnippet"
            :rev :newest
            :branch "master")
  :ensure t
  :config
  (yas-global-mode 1))

(use-package eglot
  :ensure t
  :commands (eglot eglot-ensure)
  :hook
  ((python-mode . eglot-ensure)
   (python-ts-mode . eglot-ensure)
   (typescript-ts-mode . eglot-ensure)
   (typescript-mode . eglot-ensure)
   (js-mode . eglot-ensure)
   (js2-mode . eglot-ensure)
   (web-mode . eglot-ensure)
   (html-mode . eglot-ensure)
   (css-mode . eglot-ensure)
   (c-mode . eglot-ensure)
   (cpp-mode . eglot-ensure))
  :config
  ;; pip install rassumfrassum (https://github.com/joaotavora/rassumfrassum)

  ;; uv tool install ty ruff
  (add-to-list 'eglot-server-programs
               '((python-ts-mode python-mode)
                 . ("rass" "--" "ty" "server" "--" "ruff" "server")))

  ;; bun install -g vscode-langservers-extracted @biomejs/biome typescript-language-server typescript
  (add-to-list 'eglot-server-programs
               '((html-mode)
                 . ("rass" "--" "vscode-html-language-server" "--stdio" "--" "biome" "lsp-proxy")))
  (add-to-list 'eglot-server-programs
               '((css-mode)
                 . ("rass" "--" "vscode-css-language-server" "--stdio" "--" "biome" "lsp-proxy")))
  (add-to-list 'eglot-server-programs
               '((typescript-ts-mode typescript-mode js-mode js2-mode)
                 . ("rass" "--" "typescript-language-server" "--stdio" "--" "biome" "lsp-proxy")))

  ;; sudo dnf install clang-tools-extra (Fedora)
  (add-to-list 'eglot-server-programs
               '((c-mode c-ts-mode c++-mode c++-ts-mode)
                 . ("clangd"
                    "--background-index"
                    "--clang-tidy"
                    "--completion-style=detailed"
                    "--header-insertion=never"
                    "--pch-storage=memory")))
  :bind
  (:map eglot-mode-map
        ("C-c a" . eglot-code-actions)
        ("C-c r" . eglot-rename)))

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :config
  (setq corfu-auto t
        corfu-cycle t))

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

(use-package emmet-mode
  :ensure t
  :hook ((html-mode css-mode sgml-mode web-mode js-jsx-mode tsx-ts-mode) . emmet-mode))

(use-package flycheck
  :init (global-flycheck-mode))

(use-package apheleia
  :config
  (apheleia-global-mode +1))

(use-package typescript-ts-mode
  :mode (("\\.ts\\'"  . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode)))

(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev :newest
            :branch "main")
  :bind
  (:map copilot-completion-map
        ("<tab>" . copilot-accept-completion)
        ("TAB" . copilot-accept-completion)))

;; (add-hook 'prog-mode-hook 'copilot-mode)
