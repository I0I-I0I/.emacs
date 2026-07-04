;;; init.el --- Emacs init -*- lexical-binding: t; -*-

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(push '(fullscreen . maximized) default-frame-alist)
(add-to-list 'default-frame-alist '(undecorated . t))
(set-frame-parameter nil 'undecorated t)

(defvar my/gc-cons-threshold-orig gc-cons-threshold)
(defvar my/gc-cons-percentage-orig gc-cons-percentage)

;; Fix encoding
(defun my/revert-buffer-utf8 ()
  "Revert current buffer using UTF-8 (unix) without confirmation."
  (interactive)
  (let ((coding-system-for-read 'utf-8-unix)
        (coding-system-for-write 'utf-8-unix))
    (revert-buffer :ignore-auto :noconfirm)))

(global-set-key (kbd "C-c u") #'my/revert-buffer-utf8)

(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)

;; Set PATH
(defun my/add-to-path (dir)
  (let ((path (expand-file-name dir)))
    (when (file-directory-p path)
      (unless (member path exec-path)
        (setq exec-path (cons path exec-path)))
      (let ((current (getenv "PATH")))
        (unless (string-match-p (regexp-quote path) current)
          (setenv "PATH" (concat path path-separator current)))))))

(my/add-to-path "~/.emacs.d/mason/bin")

;;; windows specific
(when (eq system-type 'windows-nt)
  (my/add-to-path "C:/Program Files/Git/usr/bin")
  (my/add-to-path "D:/apps/texlive/2026/bin/windows")
  (my/add-to-path "D:/apps/vips/bin"))

;;; Override map
(defvar my/override-map (make-sparse-keymap))
(define-minor-mode my/override-mode
  "Highest priority keymap."
  :global t
  :keymap my/override-map)
(my/override-mode 1)

;; Package and use-package
(require 'package)

(setq package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa"  . "https://melpa.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t
      use-package-verbose nil)

;; Core
(use-package emacs
  :ensure nil
  :demand t
  :init
  (use-package display-line-numbers
    :ensure nil
    :hook ((prog-mode . display-line-numbers-mode)
           (text-mode . (lambda () (display-line-numbers-mode -1))))
    :config
    (setq display-line-numbers-type 'relative))

  (setq gc-cons-threshold (* 64 1024 1024)
        gc-cons-percentage 0.1
        read-process-output-max (* 4 1024 1024)
        auto-save-default nil
        make-backup-files nil)

  (when (boundp 'native-comp-async-report-warnings-errors)
    (setq native-comp-async-report-warnings-errors nil))
  (when (boundp 'native-comp-deferred-compilation)
    (setq native-comp-deferred-compilation t))

  (run-with-idle-timer 10 t #'garbage-collect)

  (setq-default fill-column 120
                indent-tabs-mode nil
                tab-width 4
                c-basic-offset 4
                standard-indent 4)
  (add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

  (fringe-mode '(8 . 8))
  (electric-indent-mode 1)
  (setq inhibit-startup-screen t
        ring-bell-function 'ignore
        display-time-default-load-average nil)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)

  ;; (treesit-auto-install-grammar 'always)
  ;; (treesit-enabled-modes t)

  ;; (delete-pair-push-mark t)                    ; EMACS-31: pushes a mark after
  ;;                                       ; delete-pair so C-x C-x selects what was inside
  ;; (ibuffer-human-readable-size t)              ; EMACS-31: KB/MB instead of raw byte counts
  ;; (ielm-history-file-name ...)                 ; EMACS-31: IELM input history is finally persisted
  ;; (kill-region-dwim 'emacs-word)               ; EMACS-31: C-w with no region kills a word
  ;; (native-comp-async-on-battery-power nil)     ; EMACS-31: stop native-comp jobs on battery
  ;; (view-lossage-auto-refresh t)                ; EMACS-31: live-updating C-h l, great for teaching/debugging
  ;; (display-fill-column-indicator-warning nil)  ; EMACS-31
  ;; (dired-hide-details-hide-absolute-location t); EMACS-31: hide the absolute dir path in dired-hide-details-mode
  ;; (world-clock-sort-order "%FT%T")             ; EMACS-31: sort the world clock sanely
  ;; (zone-all-frames t)                          ; EMACS-31
  ;; (zone-all-windows-in-frame t)                ; EMACS-31
  ;; (uniquify-after-kill-buffer-flag t)

  (add-hook 'before-save-hook #'delete-trailing-whitespace)
  (save-place-mode 1)
  (global-hl-line-mode 1)
  (repeat-mode 1)

  (global-auto-revert-mode 1)
  (setq auto-revert-interval 1)
  (setq auto-revert-verbose nil)

  (global-visual-line-mode 1)
  (defun my-disable-visual-line-and-truncate ()
    (when (bound-and-true-p visual-line-mode) (visual-line-mode -1))
    (when (bound-and-true-p truncate-lines) (toggle-truncate-lines)))

  (add-hook 'ibuffer-mode-hook 'my-disable-visual-line-and-truncate)
  (add-hook 'buffer-menu-mode-hook 'my-disable-visual-line-and-truncate)

  (fset 'yes-or-no-p 'y-or-n-p)
  (setq savehist-additional-variables '(register-alist))
  (savehist-mode 1)

  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
        mouse-wheel-progressive-speed nil
        mouse-wheel-follow-mouse t
        scroll-margin 5
        scroll-step 1
        scroll-conservatively 10000
        fast-but-imprecise-scrolling t
        scroll-preserve-screen-position t)

  (require 'uniquify)
  (setq uniquify-buffer-name-style 'post-forward)

  ;; Navigation helpers
  (defun scroll-half-page-down ()
    (interactive)
    (scroll-down (/ (window-body-height) 2)))

  (defun scroll-half-page-up ()
    (interactive)
    (scroll-up (/ (window-body-height) 2)))

  ;; Move text
  (defun move-text-internal (arg)
    (cond
     ((and mark-active transient-mark-mode)
      (when (> (point) (mark)) (exchange-point-and-mark))
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

  (defun move-text-down (arg) (interactive "*p") (move-text-internal arg))
  (defun move-text-up   (arg) (interactive "*p") (move-text-internal (- arg)))

  :bind
  (("C-v" . scroll-half-page-up)
   ("M-v" . scroll-half-page-down)
   ("M-&" . with-editor-async-shell-command)
   ("M-!" . with-editor-shell-command)
   ("C-x _" . maximize-window)
   ("C-c c" . project-compile)
   ("C-c C" . compile)
   ("C-c C-b" . grep)
   ("C-c C-o" . browse-url-at-point)
   ("C-c o" . find-file-at-point)
   ("C-c C-M-o" . browse-url-xdg-open)
   ("M-<down>" . move-text-down)
   ("M-<up>" . move-text-up)
   ("M-S-<right>" . org-increase-number-at-point)
   ("M-S-<left>" . org-decrease-number-at-point)))

(use-package crux
  :ensure t
  :config
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-buffer untabify)
  (crux-with-region-or-line comment-or-uncomment-region)
  (crux-with-region-or-sexp-or-line kill-region)
  (crux-with-region-or-point-to-eol kill-ring-save)
  :bind (("C-a"     . crux-move-beginning-of-line)
         ;; ("C-c o"   . crux-open-with)
         ("C-c D"   . crux-delete-file-and-buffer)
         ("C-c r"   . crux-rename-file-and-buffer)
         ("C-c k"   . crux-kill-other-buffers)
         ("C-c d"   . crux-duplicate-current-line-or-region)
         ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region)
         ("C-c n"   . crux-cleanup-buffer-or-region)
         ("C-c f"   . crux-recentf-find-file)
         ("C-c F"   . crux-recentf-find-directory)
         ("C-c u"   . crux-view-url)
         ("C-c e"   . crux-eval-and-replace)
         ("C-c s"   . crux-swap-windows)
         ("C-c w"   . crux-copy-file-preserve-attributes)
         ("C-c I"   . crux-find-user-init-file)
         ("C-x 4 t" . crux-transpose-windows)
         ("C-c TAB" . crux-indent-defun)
         ("C-c b"   . crux-switch-to-previous-buffer)
         ("C-^"     . crux-top-join-line)
         ("C-c C-k" . crux-kill-and-join-forward)
         ("C-S-k"   . crux-kill-whole-line)
         ("M-o"     . crux-other-window-or-switch-buffer)
         ("C-g"     . crux-keyboard-quit-dwim)
         ("C-c M-c" . crux-kill-buffer-truename)
         ("C-<return>"   . crux-smart-open-line)
         ("C-S-<return>" . crux-smart-open-line-above)
         :map dired-mode-map
         ("z" . crux-open-with)))

;;; Font
(use-package ligature
  :config
  (ligature-set-ligatures 't '("==" "!=" "===" "!==" "&&" "||" "!!" ">>" "<<"))
  (global-ligature-mode t))

(use-package fontaine
  :custom
  (fontaine-presets
   '((maple-mono
      :default-family "Maple Mono NF CN"
      :default-height 170
      :line-spacing 1)
     (cascadia
      :default-family "Cascadia Code"
      :default-height 140
      :line-spacing 1)))
  :config
  (fontaine-mode t)
  (fontaine-set-preset 'maple-mono))

;;; Which key
(use-package which-key
  :ensure t
  :config
  (which-key-mode 1))

;;; Editor config
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;;; Server
(use-package server
  :ensure nil
  :config
  (unless (server-running-p) (server-start)))

;; Grep
(use-package wgrep
  :ensure t)

(use-package grep
  :ensure nil
  :custom
  (grep-command "grep -nH --color=auto -r -e "))

(use-package rg
  :ensure t
  :config
  (rg-enable-default-bindings)
  (rg-enable-menu))

;; Surround

(use-package emacs-surround
  :vc (:url "https://github.com/ganmacs/emacs-surround"
            :rev :newest
            :branch "master")
  :bind ("C-q" . emacs-surround))

;; Completion
(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 1)
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  (corfu-quit-no-match 'separator)
  :config
  (setq tab-always-indent 'complete)
  (setq completion-cycle-threshold 3)
  (corfu-history-mode 1)
  (corfu-popupinfo-mode 1)
  :init (global-corfu-mode)
  :hook
  (eshell-mode . (lambda () (setq-local corfu-auto nil))))

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (when (fboundp 'cape-symbol)
    (add-to-list 'completion-at-point-functions #'cape-symbol)))

(use-package icomplete
  :ensure nil
  :init
  (fido-vertical-mode 1)
  :bind (:map icomplete-minibuffer-map
              ("C-j" . icomplete-fido-exit))
  :custom
  (completion-styles '(basic flex))
  (completions-format 'one-column)
  (completions-max-height 30)
  (completions-sort 'historical)
  (completion-auto-select nil)
  (completions-detailed t)
  (completions-highlight-face 'completions-highlight)
  (completion-auto-help 'visible))

;; Theme

(use-package lambda-themes
  :vc (:url "https://github.com/Lambda-Emacs/lambda-themes"
            :rev :newest
            :branch "main")
  :demand t
  :config
  (setq lambda-themes-set-italic-comments t
        lambda-themes-set-italic-keywords t
        lambda-themes-set-variable-pitch t)
  (load-theme 'lambda-dark t))

;; IBuffer
(use-package ibuffer
  :ensure nil
  :hook (ibuffer-mode . (lambda ()
                          (ibuffer-auto-mode 1)
                          (ibuffer-switch-to-saved-filter-groups "default")))
  :config
  (setq ibuffer-show-empty-filter-groups nil
        ibuffer-always-show-last-buffer nil
        ibuffer-default-sorting-mode 'recency
        ibuffer-human-readable-size t)

  :bind (("C-x C-b" . ibuffer)
         :map ibuffer-mode-map
         ("g" . ibuffer-update)
         ("/" . ibuffer-filter-by-mode)
         ("C-c / n" . ibuffer-filter-by-name)
         ("C-c / f" . ibuffer-filter-by-filename)
         ("C-c / c" . ibuffer-clear-filter-groups)))

;;; Tramp
(use-package tramp
  :ensure nil
  :defer t
  :init
  (setq tramp-default-method "ssh")
  (setq tramp-verbose 1)
  (setq tramp-use-ssh-controlmaster-options nil)
  :config
  (setq tramp-default-remote-shell "/bin/bash")
  (setq tramp-remote-shell "/bin/bash")
  (setq tramp-remote-shell-login '("-l"))
  (setq tramp-remote-shell-args '("-c"))
  (setq tramp-chunksize 2000)
  (setq tramp-inline-compress-start-size 1000)
  (setq remote-file-name-inhibit-cache nil)
  (setq tramp-completion-reread-directory-timeout nil)
  (setq tramp-auto-save-directory "~/.emacs.d/tramp-autosave/")
  (setq auth-sources '("~/.authinfo.gpg"))
  (setq auth-source-cache-expiry nil)
  (add-to-list 'backup-directory-alist
               (cons tramp-file-name-regexp "~/.emacs.d/tramp-backups/")))

;; Eshell
(use-package eshell
  :ensure nil
  :hook (eshell-mode . (lambda ()
                         (display-line-numbers-mode -1)
                         (setq eshell-command-aliases-list
                               '(("la" "ls -Alhvp --group-directories-first --color=always")
                                 ("py" "uv run python")))))
  :init
  (use-package vterm
    :ensure t
    :commands (vterm)
    :hook (vterm-mode . (lambda () (display-line-numbers-mode -1)))
    :bind ("C-c t" . vterm))

  (use-package eat
    :ensure t
    :commands (eat eat-eshell-mode)
    :hook (eshell-mode . eat-eshell-mode))

  (use-package bash-completion
    :ensure t
    :if (or (and (eq system-type 'gnu/linux) (executable-find "bash"))
            (and (eq system-type 'windows-nt) (executable-find "wsl.exe")))
    :after eshell
    :config
    (when (eq system-type 'windows-nt)
      (setq bash-completion-prog "wsl.exe")
      (setq bash-completion-args '("--" "bash")))
    (defun +eshell-bash-completion-capf-nonexclusive ()
      (let* ((bol-pos (save-mark-and-excursion
                        (eshell-bol)
                        (point)))
             (compl (bash-completion-dynamic-complete-nocomint
                     bol-pos
                     (point) t)))
        (when compl
          (append compl '(:exclusive no)))))
    (defun +eshell-setup-bash-completion-h ()
      (add-hook 'completion-at-point-functions
                #'+eshell-bash-completion-capf-nonexclusive nil t))
    (add-hook 'eshell-mode-hook #'+eshell-setup-bash-completion-h))

  (require 'seq)
  (require 'subr-x)

  (defun my/eshell--buffer-p (b)
    (with-current-buffer b
      (derived-mode-p 'eshell-mode)))

  (defun my/eshell--buf-dir (b)
    "Return normalized `default-directory` for eshell buffer B."
    (with-current-buffer b
      (file-truename (expand-file-name default-directory))))

  (defun my/eshell--current-dir ()
    (file-truename (expand-file-name default-directory)))

  (defun my/eshell--find-by-dir (dir)
    "Find an existing eshell buffer whose `default-directory` matches DIR."
    (let ((target (file-truename (expand-file-name dir))))
      (seq-find (lambda (b)
                  (and (my/eshell--buffer-p b)
                       (string= (my/eshell--buf-dir b) target)))
                (buffer-list))))

  (defun my/eshell--new-in-dir (dir)
    "Create a new eshell buffer and start it in DIR."
    (let ((default-directory dir))
      (eshell t)))

  (defun my/toggle-eshell-here ()
    "Jump to an eshell buffer for the current directory, or create one."
    (interactive)
    (let* ((dir (my/eshell--current-dir))
           (buf (my/eshell--find-by-dir dir)))
      (if (derived-mode-p 'eshell-mode)
          (my/eshell--new-in-dir dir)
        (cond
         (buf (switch-to-buffer buf))
         (t   (my/eshell--new-in-dir dir))))))

  (require 'project)

  (defun my/eshell--project-dir ()
    "Return project root if available, else current `default-directory`."
    (if-let* ((pr (project-current nil)))
        (file-truename (project-root pr))
      (my/eshell--current-dir)))

  (defun my/toggle-eshell-project ()
    "Jump to an eshell buffer for the project root (or current dir), or create one."
    (interactive)
    (let* ((dir (my/eshell--project-dir))
           (buf (my/eshell--find-by-dir dir)))
      (if (derived-mode-p 'eshell-mode)
          (my/eshell--new-in-dir dir)
        (if buf
            (switch-to-buffer buf)
          (my/eshell--new-in-dir dir)))))

  :bind (("C-c e" . my/toggle-eshell-here)
         ("C-x p e" . my/toggle-eshell-project))

  :config
  (setq eshell-visual-commands '()
        shell-browse-url-functions nil
        eshell-cmpl-cycle-completions nil
        eshell-cmpl-ignore-case t
        eshell-cmpl-autolist t
        eshell-cmpl-expand-tildes t
        eshell-cmpl-replace-by-expanded-string t
        eshell-history-size 10000
        eshell-buffer-maximum-lines 10000
        eshell-scroll-to-bottom-on-input 'all
        eshell-scroll-show-maximum-output t
        eshell-hist-ignoredups t
        eshell-prefer-lisp-functions nil)

  (defun eshell/o (&rest args)
    (let ((target (string-join args " ")))
      (cond ((eq system-type 'darwin) (start-process "open" nil "open" target))
            ((eq system-type 'windows-nt) (w32-shell-execute "open" target))
            (t (start-process "xdg-open" nil "xdg-open" target)))))

  (defun my/eshell-history-fuzzy ()
    (interactive)
    (require 'em-hist)
    (let* ((items (and eshell-history-ring
                       (delete-dups (ring-elements eshell-history-ring))))
           (choice (completing-read "Eshell history: " items nil t)))
      (when (and choice (not (string-empty-p choice)))
        (let ((bol (save-excursion
                     (eshell-bol)
                     (point))))
          (delete-region bol (point))
          (insert choice)))))

  (add-hook 'eshell-mode-hook
            (lambda ()
              (keymap-set eshell-mode-map "C-r" #'my/eshell-history-fuzzy)
              (keymap-set eshell-mode-map "C-l" #'eshell/clear))))

;; Undo
(use-package undo-fu
  :init
  (use-package undo-fu-session
    :config
    (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
    (undo-fu-session-global-mode))

  (use-package vundo
    :custom
    (vundo-glyph-alist vundo-unicode-symbols)
    :bind ("C-x u" . vundo))

  :bind (("C-/" . undo-fu-only-undo)
         ("C-?" . undo-fu-only-redo)))

;; Git
(use-package magit
  :ensure t
  :commands (magit-status magit-dispatch)
  :bind (("C-x v d" . magit-status)
         ("C-x v v" . magit-commit-create)
         ("C-x v l" . magit-log-buffer-file)
         ("C-x v L" . magit-log)
         ("C-x v b" . magit-blame-addition)
         ("C-x v u" . magit-revert)
         ("C-x v P" . magit-push-current)
         ("C-x v p" . magit-pull-branch)
         ("C-x v !" . magit-dispatch))
  :custom
  (magit-process-connection-type nil)
  (vc-handled-backends '(Git))
  :config
  (setq magit-display-buffer-function
        #'magit-display-buffer-same-window-except-diff-v1)
  (when (eq system-type 'windows-nt)
    (setq magit-git-executable "C:/Program Files/Git/bin/git.exe")))

(use-package diff-hl
  :ensure t
  :custom
  (diff-hl-disable-on-remote t)
  :init
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (global-diff-hl-mode)
  (global-diff-hl-show-hunk-mouse-mode))

(use-package git-timemachine
  :vc (:url "https://codeberg.org/pidu/git-timemachine"
            :rev :newest
            :branch "master")
  :bind (("C-x v t" . git-timemachine)))

;; Treesitter
(setq treesit-language-source-alist
      '((elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (svelte "https://github.com/tree-sitter-grammars/tree-sitter-svelte")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (jsdoc "https://github.com/tree-sitter/tree-sitter-jsdoc")))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; Selection
(use-package expreg
  :bind (("M-h" . expreg-expand)
         ("M-H" . expreg-contract)))

;; Multi cursors
(use-package multiple-cursors
  :vc (:url "https://github.com/magnars/multiple-cursors.el"
            :rev :newest
            :branch "master")
  :demand t
  :bind (("M-n" . mc/mark-next-like-this)
         ("M-p" . mc/mark-previous-like-this)
         ("M-S-n" . mc/skip-to-next-like-this)
         ("M-S-p" . mc/skip-to-previous-like-this)
         ("C-c M-n" . mc/mark-all-like-this)
         ("C-c C-a" . mc/edit-lines)))

;; Jump
(use-package avy
  :vc (:url "https://github.com/abo-abo/avy"
            :rev :newest
            :branch "master")
  :bind (("C-'" . avy-goto-word-1)))

(use-package better-jumper
  :ensure t
  :init
  (better-jumper-mode 1)
  :config
  (setq better-jumper-context 'window)

  (defun my/better-jumper--set-jump-before (&rest _)
    (better-jumper-set-jump))

  (dolist (cmd '(xref-find-definitions
                 xref-find-references
                 imenu
                 avy-goto-word-1
                 beginning-of-buffer
                 end-of-buffer
                 backward-paragraph
                 forward-paragraph
                 switch-to-buffer
                 pop-to-buffer
                 other-window
                 isearch-forward isearch-backward
                 query-replace query-replace-regexp
                 next-error previous-error occur
                 consult-imenu))
    (advice-add cmd :before #'my/better-jumper--set-jump-before))
  :bind (("C-<" . better-jumper-jump-backward)
         ("C->" . better-jumper-jump-forward)))

(use-package harpoon
  :ensure t
  :custom
  (harpoon-project-package 'project)
  :bind (:map my/override-map
              ("M-e" . harpoon-quick-menu-hydra)
              ("M-0" . harpoon-add-file)
              ("M-1" . harpoon-go-to-1)
              ("M-2" . harpoon-go-to-2)
              ("M-3" . harpoon-go-to-3)
              ("M-4" . harpoon-go-to-4)
              ("M-5" . harpoon-go-to-5)
              ("M-6" . harpoon-go-to-6)
              ("M-7" . harpoon-go-to-7)
              ("M-8" . harpoon-go-to-8)
              ("M-9" . harpoon-go-to-9)))

(use-package emmet-mode
  :ensure t
  :hook ((html-mode css-mode sgml-mode web-mode js-jsx-mode tsx-ts-mode) . emmet-mode))

;; Docker
(use-package docker
  :ensure t
  :bind ("C-c D" . docker))

;; AI
(use-package copilot
  :ensure t
  :bind (("C-c A" . copilot-mode)
         :map copilot-completion-map
         ("<tab>" . copilot-accept-completion)
         ("TAB" . copilot-accept-completion)))

;; (use-package codeium
;;   :vc (:url "https://github.com/Exafunction/codeium.el"
;;             :rev :newest)
;;   :init
;;   (add-to-list 'completion-at-point-functions #'codeium-completion-at-point))

(use-package agent-shell
  :ensure t
  :config
  ;; (setq agent-shell-openai-authentication
  ;;       (agent-shell-openai-make-authentication :login t))
  (setq agent-shell-google-authentication
        (agent-shell-google-make-authentication :login t))
  :bind (("C-c a" . agent-shell)
         :map agent-shell-mode-map
         ("RET" . newline)
         ("C-c C-c" . shell-maker-submit)
         ("C-c C-k" . agent-shell-interrupt)))

;; Zen mode
(use-package sublimity
  :ensure t
  :config (require 'sublimity-attractive)
  :bind (("C-c z" . sublimity-mode)))

;; Dirvish
(use-package dirvish
  :ensure t
  :init
  (use-package dired
    :ensure nil
    :hook (dired-mode . dired-omit-mode)
    :custom (dired-omit-files (rx string-start (or "." "..") string-end)))

  (use-package dired-clipboard
    :vc (:url "https://github.com/kn66/dired-clipboard.el"
              :rev :newest
              :branch "main")
    :hook (dired-mode . dired-clipboard-mode))

  (use-package ready-player
    :ensure t
    :custom
    (ready-player-autoplay nil)
    (ready-player-thumbnail-max-pixel-height 600)
    :config (ready-player-mode))

  (use-package dired-preview
    :ensure t
    :custom
    (dired-preview-delay 0.2)
    (dired-preview-max-size (expt 2 20))
    (dired-preview-ignored-extensions-regexp nil)
    :config
    (defun my/dired-preview-display-action-alist ()
      `((display-buffer-in-side-window)
        (side . right)
        (window-width . 0.65)))
    (setq dired-preview-display-action-alist #'my/dired-preview-display-action-alist)
    (defun my/dired-preview-recenter (file)
      (when-let* ((buf (dired-preview--get-preview-buffer file))
                  (win (get-buffer-window buf)))
        (with-selected-window win
          (goto-char (point-min))
          (recenter 0))))
    (advice-add 'dired-preview-display-file :after #'my/dired-preview-recenter)
    :bind (:map dired-mode-map
                ("C-c p" . dired-preview-mode)))

  (dirvish-override-dired-mode)
  :hook (dired-mode . (lambda () (display-line-numbers-mode -1)))
  :custom
  (dirvish-attributes '(vc-state subtree-state collapse git-msg file-size file-time))
  (dired-listing-switches "-alh --group-directories-first")
  :config (dirvish-side-follow-mode)
  :bind (:map dirvish-mode-map
              ("TAB" . dirvish-subtree-toggle)
              ("h" . dired-up-directory)
              ("l" . dired-find-file)
              ("<backtab>" . dirvish-layout-toggle)))

(use-package perspective
  :bind
  (("C-x C-S-b" . persp-ibuffer)
   ("C-x C-b" . persp-buffer-menu)
   ("C-x b" . persp-switch-to-buffer*)
   ("C-x B" . persp-switch-to-buffer)
   ("C-x k" . persp-kill-buffer*)
   ("C-c p l" . persp-switch-last))
  :init
  (setq switch-to-prev-buffer-skip
        (lambda (win buff bury-or-kill)
          (not (persp-is-current-buffer buff))))

  (add-hook 'ibuffer-hook
            (lambda ()
              (persp-ibuffer-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic))))

  (add-hook 'kill-emacs-hook #'persp-state-save)

  (add-hook
   'emacs-startup-hook
   (lambda ()
     (when (file-exists-p persp-state-default-file)
       (persp-state-load persp-state-default-file))))

  :custom
  (persp-mode-prefix-key (kbd "C-c p"))
  (persp-state-default-file
   (expand-file-name "perspective-session" user-emacs-directory))
  :init
  (persp-mode))

;; Org mode
(use-package org-tempo
  :ensure nil)

(use-package valign
  :ensure t
  :hook
  (org-mode-hook . valign-mode))

(use-package org-fragtog
  :ensure t
  :hook (org-mode-hook . org-fragtog-mode))

(use-package org-appear
  :vc (:url "https://github.com/awth13/org-appear"
            :rev :newest
            :branch "master")
  :custom
  (org-appear-autolinks t)
  :hook (org-mode-hook . org-appear-mode))

(use-package org-modern
  :ensure t
  :hook
  (org-mode . org-modern-mode)
  :init
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda))

(use-package org
  :ensure nil                     ; Built into Emacs
  :mode ("\\.org\\'" . org-mode)
  :hook
  ((org-mode . visual-line-mode)
   (org-mode . variable-pitch-mode)
   (org-mode . org-indent-mode))
  :custom
  ;; Editing
  (org-startup-indented t)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-ellipsis " ▾")
  (org-return-follows-link t)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-edit-src-content-indentation 0)
  (org-confirm-babel-evaluate nil)

  (org-startup-with-inline-images t)
  (org-startup-with-latex-preview t)

  ; (org-preview-latex-default-process 'dvipng)
  (org-preview-latex-default-process 'dvisvgm)
  (org-format-latex-options
   (plist-put org-format-latex-options :scale 1.6))

  (org-log-done 'time)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t))))

;; LSP

(use-package mason
  :ensure t
  :demand t
  :config
  (mason-setup))

(use-package eglot
  :ensure nil
  :defer t
  :hook
  ((python-ts-mode
    js-ts-mode
    typescript-ts-mode
    tsx-ts-mode
    svelte-ts-mode
    json-ts-mode)
   . eglot-ensure)
  :custom
  (eglot-events-buffer-size 2000)
  :config
  (add-to-list
   'eglot-server-programs
   `(svelte-ts-mode
     . ,(eglot-alternatives
         '(
           ("rass" "--" "svelteserver" "--stdio" "--" "oxlint" "--lsp")
           ("svelteserver" "--stdio")
           ))))

  (add-to-list
   'eglot-server-programs
   `((typescript-ts-mode tsx-ts-mode js-ts-mode)
     . ,(eglot-alternatives
         '(
           ("rass" "--" "typescript-language-server" "--stdio" "--" "oxlint" "--lsp")
           ("typescript-language-server" "--stdio")
           ))))

  (add-to-list
   'eglot-server-programs
   `(python-ts-mode
     . ,(eglot-alternatives
         '(
           ("rass" "--" "ty" "server" "--" "ruff" "server")
           ("rass" "--" "basedpyright-langserver" "--stdio" "--" "ruff" "server")
           ("ty" "server")
           ("basedpyright-langserver" "--stdio")
           ))))

  :bind
  ("C-c l a" . eglot-code-actions)
  ("C-c l r" . eglot-rename)
  ("C-c l o" . eglot-code-action-organize-imports)
  ("C-c l f" . eglot-format-buffer)
  ("C-c l i" . eglot-find-implementation)
  ("C-c l t" . eglot-find-typeDefinition)
  ("C-c l e" . eglot-events-buffer)
  ("C-c l s" . eglot-shutdown)
  ("C-c l S" . eglot)
  ("C-c l =" . eglot-reconnect)
  ("C-c l x" . eglot-code-action-quickfix)
  ("M-N" . flymake-goto-next-error)
  ("M-P" . flymake-goto-prev-error)
  ("C-c l d" . flymake-show-buffer-diagnostics)
  ("C-c l D" . flymake-show-project-diagnostics)
  ("C-c l ?" . flymake-show-diagnostic))

(use-package svelte-ts-mode
  :vc (:url "https://github.com/leafOfTree/svelte-ts-mode"
            :rev :newest
            :branch "main")
  :ensure t
  :mode "\\.svelte\\'")

(use-package markdown-ts-mode
  :ensure t
  :defer t)
