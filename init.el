;;; init.el --- Emacs init -*- lexical-binding: t; -*-

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(push '(fullscreen . maximized) default-frame-alist)
(add-to-list 'default-frame-alist '(undecorated . t))
(set-frame-parameter nil 'undecorated t)

(defvar my/gc-cons-threshold-orig gc-cons-threshold)
(defvar my/gc-cons-percentage-orig gc-cons-percentage)

(set-language-environment 'utf-8)
(set-locale-environment "utf-8")

(setq buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Fix encoding
(defun my/revert-buffer-utf8 ()
  "Revert current buffer using UTF-8 (unix) without confirmation."
  (interactive)
  (let ((coding-system-for-read 'utf-8-unix)
        (coding-system-for-write 'utf-8-unix))
    (revert-buffer :ignore-auto :noconfirm)))

(global-set-key (kbd "C-c u") #'my/revert-buffer-utf8)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))
(setq file-name-coding-system 'utf-8-unix)
(setq locale-coding-system 'utf-8-unix)

;; Set PATH
(defun my/add-to-path (dir)
  (let ((path (expand-file-name dir)))
    (when (file-directory-p path)
      (unless (member path exec-path)
        (setq exec-path (cons path exec-path)))
      (let ((current (getenv "PATH")))
        (unless (string-match-p (regexp-quote path) current)
          (setenv "PATH" (concat path path-separator current)))))))

;;; windows specific
(when (eq system-type 'windows-nt)
  (my/add-to-path "C:/Program Files/Git/usr/bin")
  (my/add-to-path "D:/programm/vips/bin"))

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

;; Tab naming helpers
(defun my/tab-context-name ()
  "Project name or fallback to directory name."
  (or (when-let* ((proj (project-current nil)))
        (file-name-nondirectory
         (directory-file-name (project-root proj))))
      (file-name-nondirectory
       (directory-file-name default-directory))))

(defun my/tab--get (key &optional tab)
  "Get KEY from TAB alist (defaults to current tab)."
  (alist-get key (or tab (tab-bar--current-tab))))

(defun my/tab--set (key value &optional tab)
  "Set KEY to VALUE in TAB alist (defaults to current tab)."
  (let* ((tab (or tab (tab-bar--current-tab)))
         (cell (assq key tab)))
    (if cell
        (setcdr cell value)
      (setcdr tab (cons (cons key value) (cdr tab)))))
  value)

(defun my/tab-auto-rename (&rest _)
  "Auto-rename current tab to match project/directory context.
Won't clobber a manually renamed tab."
  (when (bound-and-true-p tab-bar-mode)
    (let* ((name (my/tab-context-name))
           (tab  (tab-bar--current-tab))
           (cur  (alist-get 'name tab))
           (last (my/tab--get 'my/auto-name tab)))
      (when (and name
                 (not (equal name last))
                 (or (null last) (equal cur last)))
        (my/tab--set 'my/auto-name name tab)
        (tab-bar-rename-tab name)))))

(defun my/tab-rename-to-context ()
  "Force rename current tab to context name (overwrites manual name)."
  (interactive)
  (let ((name (my/tab-context-name)))
    (when name
      (my/tab--set 'my/auto-name name)
      (tab-bar-rename-tab name))))

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

  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq gc-cons-threshold my/gc-cons-threshold-orig
                    gc-cons-percentage my/gc-cons-percentage-orig)
              (run-with-idle-timer 5 nil #'garbage-collect)))

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

  (add-hook 'before-save-hook #'delete-trailing-whitespace)
  (save-place-mode 1)
  (global-hl-line-mode 1)
  (repeat-mode 1)

  (global-auto-revert-mode 1)
  (setq auto-revert-interval 1)
  (setq auto-revert-verbose nil)

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

  (define-prefix-command 'my/c-z-map)
  (defvar my/c-z-z-map (make-sparse-keymap))
  (define-key global-map (kbd "C-z") my/c-z-map)
  (define-key my/c-z-map (kbd "z") my/c-z-z-map)

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

;;; Ligature
(use-package ligature
  :config
  (ligature-set-ligatures 't '("==" "!=" "===" "!==" "&&" "||" "!!" ">>" "<<"))
  (global-ligature-mode t))

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

;;; Tab-bar
(use-package tab-bar
  :ensure nil
  :demand t
  :init
  (tab-bar-mode 1)
  :custom
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show nil)
  (tab-bar-show nil)
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-new-tab-name-function #'my/tab-context-name)
  :config
  (tab-bar-history-mode 1)
  (set-face-attribute 'tab-bar nil :height 160)
  (add-hook 'buffer-list-update-hook #'my/tab-auto-rename)
  (with-eval-after-load 'project
    (add-hook 'project-switch-hook #'my/tab-auto-rename))
  :bind
  (("C-x t R" . my/tab-rename-to-context)))

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
  (setq ibuffer-saved-filter-groups
        '(("default"
           ("Code" (derived-mode . prog-mode))
           ("EAF" (mode . eaf-mode))
           ("Org" (or (mode . org-mode)
                      (filename . "\\.org$")))
           ("Shell" (or (mode . eshell-mode)
                        (mode . vterm-mode)
                        (mode . shell-mode)
                        (mode . term-mode)))
           ("Emacs" (or (name . "^\\*scratch\\*$")
                        (name . "^\\*Help\\*$")
                        (name . "^\\*Buffer List\\*$")
                        (name . "^\\*eaf\\*$")
                        (name . "^\\*eaf-epc.*\\*$")
                        (name . "^\\*Disabled command\\*$")
                        (name . "^\\*Async-native-compile-log\\*$")
                        (name . "^\\*Messages\\*$")
                        (name . "^\\*Backtrace\\*$")
                        (name . "^\\*Warnings\\*$")
                        (name . "^\\*Completions\\*$")))
           ("Dired" (mode . dired-mode))
           ("Magit" (name . "^\\*magit"))
           ("Process" (name . "^\\*")))))

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

(use-package bufferlo
  :ensure t
  :after ibuffer
  :demand t
  :config
  (unless (fboundp 'bufferlo--ibuffer-do-bufferlo-remove-prompt)
    (defun bufferlo--ibuffer-do-bufferlo-remove-prompt (op)
      "`ibuffer' prompt helper for OP."
      (let ((bookmark-name (when (fboundp 'bufferlo--current-bookmark-name)
                             (bufferlo--current-bookmark-name))))
        (format "%s from %slocals:" op
                (if bookmark-name
                    (format-message "bufferlo bookmark `%s' " bookmark-name)
                  "")))))
  (bufferlo-mode 1)
  :bind (("C-x b" . bufferlo-switch-to-buffer)
         ("C-x B" . bufferlo-find-buffer-switch)
         ("C-x C-M-b" . bufferlo-ibuffer-orphans)
         ("C-x C-b" . bufferlo-ibuffer)
         ("C-x C-S-B" . ibuffer)
         ("C-x t 0" . bufferlo-tab-close-kill-buffers)))

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

;; Desktop (restore session)
(use-package desktop
  :ensure nil
  :init
  (setq desktop-dirname             (expand-file-name "desktop/" user-emacs-directory)
        desktop-base-file-name      "desktop"
        desktop-base-lock-name      "lock"
        desktop-path               (list desktop-dirname)
        desktop-save               t
        desktop-load-locked-desktop t
        desktop-restore-frames      t)

  (setq desktop-files-not-to-save
        (concat desktop-files-not-to-save
                "\\|\\`/ssh:"
                "\\|\\`/sudo:"
                "\\|\\`/docker:"
                "\\|\\`/scp:"
                "\\|\\`/rsync:"))

  (setq desktop-buffers-not-to-save
        (concat "\\` "
                "\\|\\`\\*Messages\\*\\'"
                "\\|\\`\\*scratch\\*\\'"
                "\\|\\`\\*Help\\*\\'"
                "\\|\\`\\*Compile-Log\\*\\'"
                "\\|\\`\\*Warnings\\*\\'"
                "\\|\\`\\*Async-native-compile-log\\*\\'"
                "\\|\\`\\*tramp/.*\\*\\'"))
  (add-to-list 'desktop-modes-not-to-save 'ibuffer-mode)
  :config
  (with-eval-after-load 'desktop (require 'org))
  (make-directory desktop-dirname t)
  (desktop-save-mode 1)
  (remove-hook 'window-setup-hook #'desktop-read))

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
(use-package undo-tree
  :ensure t
  :custom
  (undo-tree-visualizer-timestamps t)
  (undo-tree-history-directory-alist `(("." . ,(expand-file-name "undo-history" user-emacs-directory))))
  (undo-tree-auto-save-history t)
  (undo-limit 10000000)
  (undo-strong-limit 10000000)
  :config
  (global-undo-tree-mode))

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

;; Selection
(use-package expand-region
  :ensure t
  :bind (("M-h" . er/expand-region)))

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

;; Treesitter
(use-package treesit-auto
  :ensure t
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package emmet-mode
  :ensure t
  :hook ((html-mode css-mode sgml-mode web-mode js-jsx-mode tsx-ts-mode) . emmet-mode))

;; Docker
(use-package docker
  :ensure t
  :bind ("C-z d" . docker))

;; AI
(use-package copilot
  :ensure t
  :bind (("C-z A" . copilot-mode)
         :map copilot-completion-map
         ("<tab>" . copilot-accept-completion)
         ("TAB" . copilot-accept-completion)))

(use-package agent-shell
  :ensure t
  :config
  (add-to-list 'agent-shell-engines
               '(agy . (:command ("agy" "--experimental-acp")
                                 :name "Antigravity Agent")))
  ;; (setq agent-shell-openai-authentication
  ;;       (agent-shell-openai-make-authentication :login t))
  (setq agent-shell-google-authentication
        (agent-shell-google-make-authentication :login t))
  :bind (("C-z a" . agent-shell)
         :map agent-shell-mode-map
         ("RET" . newline)
         ("C-c C-c" . shell-maker-submit)
         ("C-c C-k" . agent-shell-interrupt)))

;; Zen mode
(use-package sublimity
  :ensure t
  :config
  ;; (require 'sublimity-scroll)
  (require 'sublimity-attractive)
  :bind (("C-z Z" . sublimity-mode)))

;; Dirvish
(use-package dirvish
  :ensure t
  :init
  (use-package dired
    :ensure nil
    :hook (dired-mode . dired-omit-mode)
    :custom (dired-omit-files (rx string-start (or "." "..") string-end)))

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

;; Translate
(use-package gt
  :ensure t
  :init
  (use-package posframe :ensure t)
  (setq gt-langs '(auto ru))

  :config
  (require 'gt-render-posframe)

  (defvar my/gt-target-langs
    '(("ru" . ru)
      ("en" . en)
      ("de" . de)
      ("ja" . ja))
    "Languages for gt target selection.")

  (defun my/gt-set-target-lang ()
    "Set target language for gt keeping source as auto."
    (interactive)
    (let* ((choice (completing-read
                    "Translate to: "
                    (mapcar #'car my/gt-target-langs)
                    nil t))
           (lang (cdr (assoc choice my/gt-target-langs))))
      (setq gt-langs (list 'auto lang))
      (message "gt: auto -> %s" lang)))

  (defun my/gt-translate-to ()
    "Pick target language and translate."
    (interactive)
    (call-interactively #'my/gt-set-target-lang)
    (gt-translate))

  (with-eval-after-load 'gt
    (with-eval-after-load 'gt-result
      (define-key gt-result-mode-map (kbd "y") #'my/gt-copy-translation)))

  (setq gt-default-translator
        (gt-translator
         :taker (gt-taker
                 :text (lambda ()
                         (if (use-region-p)
                             (buffer-substring-no-properties
                              (region-beginning) (region-end))
                           (or (current-kill 0 t) "")))
                 :prompt nil
                 :pick nil)
         :engines (list (gt-google-engine))
         :render (gt-posframe-pop-render
                  :padding 10
                  :forecolor "#ffffff"
                  :backcolor "#222222"
                  :frame-params (list :border-width 1))))

  (with-eval-after-load 'gt
    (defun my/gt-posframe-buffer ()
      "Return buffer currently used by gt posframe."
      (when (and (boundp 'gt--render) gt--render)
        (let ((buf (plist-get gt--render :buffer)))
          (when (buffer-live-p buf)
            buf))))

    (defun my/gt-copy-from-posframe ()
      "Copy translation text from gt posframe."
      (interactive)
      (let ((buf (or (my/gt-posframe-buffer)
                     (car (seq-filter
                           (lambda (b)
                             (string-match-p "\\*gt" (buffer-name b)))
                           (buffer-list))))))
        (unless (buffer-live-p buf)
          (user-error "No gt translation buffer found"))

        (with-current-buffer buf
          (let ((text (string-trim
                       (buffer-substring-no-properties
                        (point-min) (point-max)))))
            (kill-new text)
            (message "Translation copied")))))

    (global-set-key (kbd "C-M-c") #'my/gt-copy-from-posframe))

  :bind (("C-M-y" . gt-translate)
         ("C-M-S-y" . my/gt-translate-to)))
