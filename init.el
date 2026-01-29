;;; init.el --- Emacs init -*- lexical-binding: t; -*-

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror 'nomessage)

(push '(fullscreen . maximized) default-frame-alist)
(add-to-list 'default-frame-alist '(undecorated . t))
(set-frame-parameter nil 'undecorated t)

(defvar my/gc-cons-threshold-orig gc-cons-threshold)
(defvar my/gc-cons-percentage-orig gc-cons-percentage)

;; Font
(when (display-graphic-p)
  (when (find-font (font-spec :family "Maple Mono"))
    (set-frame-font
     (pcase system-type
       ('windows-nt "Maple Mono-14")
       ('gnu/linux  "Maple Mono-18"))
     t t))
  (when (find-font (font-spec :family "Maple Mono NF"))
    (set-frame-font
     (pcase system-type
       ('windows-nt "Maple Mono NF-14")
       ('gnu/linux  "Maple Mono NF-18"))
     t t)))

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
  (or (when-let ((proj (project-current nil)))
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

  (setq display-line-numbers-type 'relative)
  (global-display-line-numbers-mode 1)

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

  (global-auto-revert-mode 1)
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
        scroll-preserve-screen-position t)

  (custom-set-variables
   '(uniquify-buffer-name-style 'post-forward nil (uniquify)))

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

  ;; Files
  (defun open-file-at-point ()
    (interactive)
    (require 'ffap)
    (let ((file (or (ffap-file-at-point) (thing-at-point 'filename t))))
      (if file
          (find-file (expand-file-name file))
        (user-error "No filename at point"))))

  (define-prefix-command 'my/c-z-map)
  (defvar my/c-z-z-map (make-sparse-keymap))
  (define-key global-map (kbd "C-z") my/c-z-map)
  (define-key my/c-z-map (kbd "z") my/c-z-z-map)

  :bind
  (("C-v" . scroll-half-page-up)
   ("M-v" . scroll-half-page-down)
   ("C-x _" . maximize-window)
   ("C-c c" . project-compile)
   ("C-c C" . compile)
   ("C-c C-b" . grep)
   ("C-c C-l" . (lambda () (interactive) (duplicate-line) (next-line)))
   ("C-c o" . open-file-at-point)
   ("C-c C-o" . browse-url-at-point)
   ("C-M-<down>" . move-text-down)
   ("C-M-<up>" . move-text-up)))

;; Tab-bar
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
  (set-face-attribute 'tab-bar nil :height 160)
  (add-hook 'buffer-list-update-hook #'my/tab-auto-rename)
  (with-eval-after-load 'project
    (add-hook 'project-switch-hook #'my/tab-auto-rename))
  :bind
  (("C-x t R" . my/tab-rename-to-context)))

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

;; Completion UI
(use-package icomplete
  :ensure nil
  :init
  (fido-vertical-mode 1)
  :custom
  (completion-styles '(basic flex))
  (completions-format 'one-column)
  (completions-max-height 30)
  (completions-sort 'historical)
  (completion-auto-select nil)
  (completions-detailed t)
  (completions-highlight-face 'completions-highlight)
  (completion-auto-help 'visible))

(use-package which-key
  :ensure t
  :config
  (which-key-mode 1))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;; Theme/UI
(defvar my/saved-themes nil)
(defun my/toggle-transparency ()
  (interactive)
  (when (display-graphic-p)
    (let* ((raw (frame-parameter nil 'alpha))
           (alpha (if (consp raw) (car raw) (or raw 100)))
           (on? (= alpha 100)))
      (set-frame-parameter nil 'alpha (if on? '(72 . 72) '(100 . 100)))
      (if on?
          (progn
            (setq my/saved-themes custom-enabled-themes)
            (set-face-background 'default "#000000"))
        (progn
          (set-face-background 'default nil)
          (mapc #'disable-theme custom-enabled-themes)
          (mapc #'enable-theme (reverse my/saved-themes)))))))

(global-set-key (kbd "C-c T") #'my/toggle-transparency)

(use-package minimal-dashboard
  :vc (:url "https://github.com/dheerajshenoy/minimal-dashboard.el"
            :rev :newest
            :branch "main")
  :hook (minimal-dashboard-mode-hook . (lambda ()
                                         (display-line-numbers-mode 0)))
  :init
  (setq initial-buffer-choice #'minimal-dashboard)
  :custom
  (minimal-dashboard-buffer-name "Dashboard")
  (minimal-dashboard-text (lambda () (format "started in %s" (emacs-init-time))))
  (minimal-dashboard-image-scale 1.25)
  (minimal-dashboard-enable-resize-handling t)
  (minimal-dashboard-modeline-shown nil))

(use-package lambda-themes
  :vc (:url "https://github.com/Lambda-Emacs/lambda-themes"
            :rev :newest
            :branch "main")
  :demand t
  :init
  (add-to-list 'custom-theme-load-path
               (file-name-directory (locate-library "lambda-themes")))
  :config
  (setq lambda-themes-set-italic-comments t
        lambda-themes-set-italic-keywords t
        lambda-themes-set-variable-pitch t)
  (load-theme 'lambda-dark t))

(use-package mood-line
  :ensure t
  :config
  (let ((bg (face-background 'mode-line nil t)))
    (set-face-attribute
     'mode-line nil
     :box `(:line-width 6 :color ,bg)))
  (let ((bg (face-background 'mode-line-inactive nil t)))
    (set-face-attribute
     'mode-line-inactive nil
     :box `(:line-width 6 :color ,bg)))

  (defun my/mode-line-context ()
    "Project name or fallback to directory name (mode line)."
    (propertize (format "[%s]" (my/tab-context-name))
                'face 'mode-line-emphasis))

  (mood-line-mode)

  (with-eval-after-load 'mood-line
    (setq-default mode-line-format
                  (list
                   '(:eval (my/mode-line-context))
                   '(:eval (mood-line--process-format mood-line-format))))))

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
  :init (bufferlo-mode 1)
  :bind (("C-x b" . bufferlo-switch-to-buffer)
         ("C-x B" . switch-to-buffer)
         ("C-x C-M-b" . bufferlo-ibuffer-orphans)
         ("C-x C-b" . bufferlo-ibuffer)
         ("C-x C-S-B" . ibuffer)))

;; Eshell
(use-package eshell
  :ensure nil
  :hook (eshell-mode . (lambda ()
                         (display-line-numbers-mode -1)
                         (setq eshell-command-aliases-list
                               '(("la" "ls -Alhvp --group-directories-first --color=always")
                                 ("py" "uv run python")))))
  :init
  (use-package corfu
    :ensure t
    :custom
    (corfu-auto nil)
    (corfu-cycle t)
    (corfu-preselect 'prompt)
    (corfu-quit-no-match 'separator)
    :hook
    ((shell-mode eshell-mode) . corfu-mode)
    (shell-mode . (lambda ()
                    (setq-local corfu-auto-delay 0.2
                                corfu-auto-prefix 2))))

  (use-package vterm
    :ensure t
    :commands (vterm)
    :hook (vterm-mode . (lambda () (display-line-numbers-mode -1)))
    :bind ("C-c t" . vterm))

  (use-package eat
    :ensure t
    :commands (eat eat-eshell-mode)
    :hook (eshell-mode . eat-eshell-mode))

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
      (cond
       (buf (switch-to-buffer buf))
       (t   (my/eshell--new-in-dir dir)))))

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
      (if buf
          (switch-to-buffer buf)
        (my/eshell--new-in-dir dir))))

  :bind (("C-c e" . my/toggle-eshell-here)
         ("C-x p e" . my/toggle-eshell-project))

  :config
  (advice-add 'eshell-browse-url :override #'ignore)
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

  (advice-add 'browse-url :before (lambda (&rest _) (backtrace)))

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

(use-package bash-completion
  :ensure t
  :if (not (eq system-type 'windows-nt))
  :after eshell
  :if (or (and (eq system-type 'gnu/linux)
               (executable-find "bash"))
          (and (eq system-type 'windows-nt)
               (executable-find "wsl.exe")))
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

;; Consult
(use-package consult
  :ensure t
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ;; ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g r" . consult-grep-match)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))
  :init
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

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
  :bind (("C-z g g" . magit-status))
  :custom
  (magit-process-connection-type nil)
  :config
  (when (eq system-type 'windows-nt)
    (setq magit-git-executable "C:/Program Files/Git/bin/git.exe")))

(use-package git-timemachine
  :vc (:url "https://codeberg.org/pidu/git-timemachine"
            :rev :newest
            :branch "master")
  :bind (("C-z g t" . git-timemachine)))

;; Selection
(use-package expand-region
  :ensure t
  :bind (("M-h" . er/expand-region)))

;; Multi cursors
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

;; Jump
(use-package avy
  :vc (:url "https://github.com/abo-abo/avy"
            :rev :newest
            :branch "master")
  :bind (("C-'" . avy-goto-word-1)))

;; Treesitter
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(defun my/treesit-install ()
  (interactive)
  (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist)))

(setq major-mode-remap-alist
      '((yaml-mode . yaml-ts-mode)
        (bash-mode . bash-ts-mode)
        (js2-mode . js-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (json-mode . json-ts-mode)
        (css-mode . css-ts-mode)
        (python-mode . python-ts-mode)))

;; LSP
(use-package lsp-bridge
  :vc (:url "https://github.com/manateelazycat/lsp-bridge"
            :rev :newest
            :branch "master")
  :demand t
  :init
  (use-package markdown-mode
    :ensure t)

  (use-package yasnippet
    :vc (:url "https://github.com/joaotavora/yasnippet"
              :rev :newest
              :branch "master")
    :ensure t
    :config
    (yas-global-mode 1))

  (use-package cape
    :ensure t
    :init
    (add-to-list 'completion-at-point-functions #'cape-file)
    (add-to-list 'completion-at-point-functions #'cape-dabbrev)
    (add-to-list 'completion-at-point-functions #'cape-keyword)
    (add-hook 'completion-at-point-functions #'cape-elisp-block))

  (setq acm-enable-quick-access nil
        acm-enable-icon nil
        acm-enable-capf t
        acm-enable-path t
        acm-enable-search-file-words t
        acm-enable-yas t
        acm-enable-doc t
        acm-enable-doc-markdown-render t
        acm-enable-lsp-workspace-symbol t
        acm-candidate-match-function 'regexp-quote
        acm-backend-lsp-enable-auto-import t)

  (dolist (dir '("~/.bun/bin" "~/.local/bin"))
    (let* ((path (expand-file-name dir))
           (path-list (split-string (or (getenv "PATH") "") path-separator t)))
      (when (file-directory-p path)
        (unless (member path exec-path)
          (setq exec-path (cons path exec-path)))
        (unless (member path path-list)
          (setenv "PATH" (concat path path-separator (getenv "PATH")))))))
  (setq lsp-bridge-user-langserver-dir (expand-file-name "~/.emacs.d/lsp-bridge/langserver/"))
  (setq lsp-bridge-user-multiserver-dir (expand-file-name "~/.emacs.d/lsp-bridge/multiserver/"))

  (setq lsp-bridge-enable-hover-diagnostic nil
        lsp-bridge-enable-signature-help t
        lsp-bridge-enable-auto-format-code nil)
  :config
  (add-to-list 'lsp-bridge-multi-lang-server-extension-list '("py" . "pyrefly_ruff"))
  (dolist (ext '("ts" "tsx" "js" "jsx"))
    (add-to-list 'lsp-bridge-multi-lang-server-extension-list (cons ext "tsls_oxlint_oxfmt")))

  (defun my/lsp-bridge-xref-backend () 'lsp-bridge)
  (add-hook 'lsp-bridge-mode-hook (lambda () (add-hook 'xref-backend-functions #'my/lsp-bridge-xref-backend nil t)))
  (global-lsp-bridge-mode)
  :bind (:map lsp-bridge-mode-map
              ("M-."   . lsp-bridge-find-def)
              ("C-c M-." . lsp-bridge-find-type-def)
              ("M-,"   . lsp-bridge-find-def-return)
              ("M-?"   . lsp-bridge-find-references)
              ("C-h ." . lsp-bridge-popup-documentation)
              ("C-c l s" . lsp-bridge-workspace-list-symbols)
              ("C-c l r" . lsp-bridge-rename)
              ("C-c l a" . lsp-bridge-code-action)
              ("C-c l d" . lsp-bridge-diagnostic-list)
              ("C-c l D" . lsp-bridge-workspace-diagnostic-list)
              ("C-c C-n" . lsp-bridge-diagnostic-jump-next)
              ("C-c C-p" . lsp-bridge-diagnostic-jump-prev)
              ("C-c l f" . lsp-bridge-format-buffer)))

(use-package emmet-mode
  :ensure t
  :hook ((html-mode css-mode sgml-mode web-mode js-jsx-mode tsx-ts-mode) . emmet-mode))

(with-eval-after-load 'compile
  (defconst my/pyrefly-compilation-regexp
    "^[[:space:]]*-->[[:space:]]+\\(.+\\):\\([0-9]+\\):\\([0-9]+\\)$")
  (add-to-list
   'compilation-error-regexp-alist-alist
   (list 'pyrefly my/pyrefly-compilation-regexp 1 2 3))
  (defun my/compilation-ensure-pyrefly ()
    (add-to-list 'compilation-error-regexp-alist 'pyrefly))
  (add-hook 'compilation-mode-hook #'my/compilation-ensure-pyrefly)
  (add-hook 'compilation-start-hook (lambda (_proc) (my/compilation-ensure-pyrefly))))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode +1))

;; DAP
(use-package dap-mode
  :ensure t
  :config
  (setq dap-auto-configure-features '(sessions locals controls tooltip)))

;; Docker
(use-package docker
  :ensure t
  :bind ("C-z d" . docker))

;; AI
(use-package copilot
  :ensure t
  :bind (("C-z a a" . copilot-mode)
         :map copilot-completion-map
         ("<tab>" . copilot-accept-completion)
         ("TAB" . copilot-accept-completion)))

(use-package agent-shell
  :ensure t
  :config
  (setq agent-shell-preferred-agent-config
        (agent-shell-openai-make-codex-config))
  (setq agent-shell-openai-authentication
        (agent-shell-openai-make-authentication :login t))
  (setq agent-shell-openai-codex-command '("npx" "@zed-industries/codex-acp"))
  :bind (("C-z a c" . agent-shell)))

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
  :init (use-package dired
          :ensure nil
          :hook (dired-mode . dired-omit-mode)
          :custom (dired-omit-files (rx string-start (or "." "..") string-end)))

  (dirvish-override-dired-mode)
  :hook (dired-mode . (lambda () (display-line-numbers-mode -1)))
  :custom
  (dirvish-attributes '(vc-state subtree-state collapse git-msg file-size file-time))
  (dirvish-preview-dispatchers '(image video audio archive pdf))
  (dirvish-default-layout '(0 0.35 0.65))
  (dired-listing-switches "-alh --group-directories-first")
  :config (dirvish-side-follow-mode)
  :bind (:map dirvish-mode-map
              ("TAB" . dirvish-subtree-toggle)
              ("h" . dired-up-directory)
              ("l" . dired-find-file)
              ("<backtab>" . dirvish-layout-toggle)))

;; Org
(use-package org
  :ensure nil
  :mode ("\\.org\\'" . org-mode)
  :hook ((org-mode . visual-line-mode)
         (org-mode . org-indent-mode)
         (org-mode . (lambda () (display-line-numbers-mode -1))))
  :config
  (setq org-startup-folded 'content
        org-startup-indented t
        org-hide-emphasis-markers t))

(use-package valign
  :ensure t
  :hook ((org-mode-hook . valign-mode))
  :custom (valign-fancy-bar t))

;; Media
(use-package mpvi
  :ensure t
  :init (use-package emms :ensure t)
  :custom (mpvi-mpv-ontop-p t)
  :bind (("C-z C-v" . mpvi-play)
         ("C-z v s" . mpvi-speed)))

;; EAF
(use-package eaf
  :vc (:url "https://github.com/emacs-eaf/emacs-application-framework"
            :rev :newest
            :branch "master")
  :commands (eaf-open-browser eaf-open eaf-search-it eaf-open-url-at-point eaf-open-browser-with-history eaf-open-pdf-from-history)
  :init (use-package htmlize :ensure t)
  :custom
  (eaf-browser-translate-language "en")
  (eaf-browser-continue-where-left-off t)
  (eaf-browser-enable-adblocker t)
  (eaf-browser-default-search-engine "duckduckgo")
  (eaf-browse-blank-page-url "https://duckduckgo.com")
  (eaf-pdf-dark-mode "force")
  (browse-url-browser-function #'eaf-open-browser)
  :config
  (require 'eaf-browser)
  (require 'eaf-org-previewer)
  (require 'eaf-markdown-previewer)
  (require 'eaf-pdf-viewer)

  (defalias 'browse-web #'eaf-open-browser)

  (defun adviser-find-file (orig-fn file &rest args)
    (let ((fn (if (commandp 'eaf-open) #'eaf-open orig-fn)))
      (pcase (file-name-extension file)
        ((or "pdf" "epub") (funcall fn file))
        (_ (apply orig-fn file args)))))

  (advice-add #'find-file :around #'adviser-find-file)

  :bind (("C-c C-o" . eaf-open-url-at-point)
         :map my/c-z-z-map
         ("C-f" . eaf-open)
         ("u" . eaf-open-browser)
         ("h" . eaf-open-browser-with-history)
         ("s" . eaf-search-it)
         ("p" . eaf-open-pdf-from-history)))
