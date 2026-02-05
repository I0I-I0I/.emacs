;;; init.el --- Emacs init -*- lexical-binding: t; -*-

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror 'nomessage)

(push '(fullscreen . maximized) default-frame-alist)
(add-to-list 'default-frame-alist '(undecorated . t))
(set-frame-parameter nil 'undecorated t)

(defvar my/gc-cons-threshold-orig gc-cons-threshold)
(defvar my/gc-cons-percentage-orig gc-cons-percentage)

(set-language-environment 'utf-8)
(set-locale-environment "utf-8")

(setq buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

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
   ("M-<down>" . move-text-down)
   ("M-<up>" . move-text-up)))

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
  (tab-bar-history-mode 1)
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
(defgroup my/transparency nil
  "Toggle frame transparency while forcing background to black."
  :group 'frames)

(defcustom my/transparency-alpha-on 75
  "Alpha (opacity) value used when transparency is enabled."
  :type 'integer)

(defcustom my/transparency-alpha-off 100
  "Alpha (opacity) value used when transparency is disabled."
  :type 'integer)

(defvar my/transparency--bg-param 'my/transparency--stored-bg
  "Frame parameter key where we store original background colors.")

(defun my/transparency--alpha-param ()
  "Return the frame parameter symbol to use for transparency."
  (if (eq system-type 'windows-nt)
      'alpha
    'alpha-background))

(defun my/transparency--get-face-bg (face frame)
  "Get FACE background on FRAME (or nil if unspecified)."
  (face-background face frame t))

(defun my/transparency--set-face-bg (face bg)
  "Set FACE background to BG (nil means reset to theme/default)."
  (if bg
      (set-face-background face bg nil)
    (set-face-background face 'unspecified nil)))

(defcustom my/transparency-refresh-method 'jiggle
  "How to force-refresh the frame after changing alpha.
- nil: do nothing
- redraw: redraw only (lightweight, may not help)
- jiggle: tiny resize nudge (usually fixes compositor lag)"
  :type '(choice (const :tag "None" nil)
                 (const :tag "Redraw" redraw)
                 (const :tag "Jiggle resize" jiggle)))

(defun my/transparency--refresh-frame (&optional frame)
  "Force the compositor/WM to notice recent frame parameter changes."
  (let ((frame (or frame (selected-frame))))
    (pcase my/transparency-refresh-method
      ('nil nil)
      ('redraw
       (redraw-frame frame)
       (force-window-update frame)
       (redisplay t))
      ('jiggle
       (let ((w (frame-width frame))
             (h (frame-height frame)))
         (run-with-idle-timer
          0 nil
          (lambda ()
            (when (frame-live-p frame)
              (ignore-errors
                (set-frame-size frame (1+ w) h t)
                (set-frame-size frame w h t))
              (redraw-frame frame)
              (force-window-update frame)
              (redisplay t)))))))))

(defun my/transparency--store-current-bgs (&optional frame)
  "Store current background colors for key faces into FRAME parameter."
  (let* ((frame (or frame (selected-frame)))
         (stored (frame-parameter frame my/transparency--bg-param)))
    (unless stored
      (set-frame-parameter
       frame my/transparency--bg-param
       (list
        (cons 'default (my/transparency--get-face-bg 'default frame))
        (cons 'line-number (my/transparency--get-face-bg 'line-number frame))
        (cons 'line-number-current-line (my/transparency--get-face-bg 'line-number-current-line frame))
        (cons 'header-line (my/transparency--get-face-bg 'header-line frame)))))))

(defun my/transparency--restore-bgs (&optional frame)
  "Restore background colors from FRAME parameter."
  (let* ((frame (or frame (selected-frame)))
         (stored (frame-parameter frame my/transparency--bg-param)))
    (when stored
      (my/transparency--set-face-bg 'default (alist-get 'default stored))
      (my/transparency--set-face-bg 'line-number (alist-get 'line-number stored))
      (my/transparency--set-face-bg 'line-number-current-line
                                    (alist-get 'line-number-current-line stored))
      (my/transparency--set-face-bg 'header-line (alist-get 'header-line stored))
      (let ((bg (alist-get 'header-line stored)))
        (set-face-attribute 'header-line nil
                            :box `(:line-width 6 :color ,(or bg "unspecified"))))
      (set-frame-parameter frame my/transparency--bg-param nil))))

(defun my/transparency--enabled-p (&optional frame)
  "Non-nil if transparency is currently enabled on FRAME."
  (let* ((frame (or frame (selected-frame)))
         (param (my/transparency--alpha-param))
         (raw (frame-parameter frame param))
         (alpha (cond
                 ((consp raw) (car raw))
                 ((numberp raw) raw)
                 (t my/transparency-alpha-off))))
    (< alpha my/transparency-alpha-off)))

(defun my/transparency--apply-alpha (alpha &optional frame)
  "Set transparency ALPHA on FRAME using the right parameter."
  (let* ((frame (or frame (selected-frame)))
         (param (my/transparency--alpha-param)))
    (set-frame-parameter frame param alpha)))

(defun my/transparency--force-black-ui ()
  (my/transparency--set-face-bg 'default "#000000")
  (my/transparency--set-face-bg 'line-number "#000000")
  (my/transparency--set-face-bg 'line-number-current-line "#000000")
  (my/transparency--set-face-bg 'header-line "#000000")
  (set-face-attribute 'header-line nil :box '(:line-width 6 :color "#000000")))

(defun my/toggle-transparency ()
  "Toggle transparency on the current frame, forcing background to black when enabled."
  (interactive)
  (unless (display-graphic-p)
    (user-error "Transparency needs a graphical frame"))
  (let ((frame (selected-frame)))
    (if (my/transparency--enabled-p frame)
        (progn
          (my/transparency--apply-alpha my/transparency-alpha-off frame)
          (my/transparency--restore-bgs frame)
          (my/transparency--refresh-frame frame)
          (message "Transparency: OFF"))
      (my/transparency--store-current-bgs frame)
      (my/transparency--force-black-ui)
      (my/transparency--apply-alpha my/transparency-alpha-on frame)
      (my/transparency--refresh-frame frame)
      (message "Transparency: ON"))))

(defun my/transparency--update-bg-after-theme (&rest _)
  "If transparency is active, refresh stored background after theme change."
  (dolist (frame (frame-list))
    (when (my/transparency--enabled-p frame)
      (set-frame-parameter frame my/transparency--bg-param nil)
      (my/transparency--store-current-bgs frame)
      (my/transparency--set-face-bg 'default "#000000")
      (my/transparency--set-face-bg 'line-number "#000000")
      (my/transparency--set-face-bg 'line-number-current-line "#000000"))))

(advice-add 'load-theme :after #'my/transparency--update-bg-after-theme)

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
  (let ((bg (face-background 'header-line nil t)))
    (set-face-attribute
     'header-line nil
     :box `(:line-width 6 :color ,bg)))

  (defun my/mode-line-context ()
    "Project name or fallback to directory name (mode line)."
    (propertize (format "[%s]" (my/tab-context-name))
                'face 'mode-line-emphasis))

  (mood-line-mode)
  (setq-default mode-line-format nil)
  (setq-default header-line-format
                (list
                 '(:eval (my/mode-line-context))
                 '(:eval (mood-line--process-format mood-line-format)))))

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
  :init
  (bufferlo-mode)
  :config
  :bind (("C-x b" . bufferlo-switch-to-buffer)
         ("C-x B" . bufferlo-find-buffer-switch)
         ("C-x C-M-b" . bufferlo-ibuffer-orphans)
         ("C-x C-b" . bufferlo-ibuffer)
         ("C-x C-S-B" . ibuffer)
         ("C-x t 0" . bufferlo-tab-close-kill-buffers)))

;; Completion
(use-package corfu
  :ensure t
  :custom
  (corfu-auto nil)
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  (corfu-quit-no-match 'separator)
  :init (global-corfu-mode)
  :hook
  (lsp-bridge-mode . (lambda () (corfu-mode -1))))

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
              (keymap-set eshell-mode-map "M-r" #'my/eshell-history-fuzzy)
              (keymap-set eshell-mode-map "C-l" #'eshell/clear))))

;; Consult
(use-package consult
  :ensure t
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c M" . consult-man)
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
  :hook ((python-mode
          python-ts-mode
          web-mode
          js-mode
          js-ts-mode
          typescript-mode
          typescript-ts-mode
          tsx-ts-mode
          css-mode
          css-ts-mode
          json-mode
          json-ts-mode
          yaml-mode
          yaml-ts-mode
          emacs-lisp-mode) . lsp-bridge-mode)
  :init
  (use-package markdown-mode
    :ensure t)

  (use-package web-mode
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

(use-package flyspell
  :ensure nil
  :init
  (setq ispell-program-name "hunspell"
        ispell-dictionary "en_US"
        flyspell-issue-message-flag nil
        flyspell-delay 0.5
        ispell-choices-win-default-height 20)
  :hook
  ((text-mode . flyspell-mode)
   (prog-mode . flyspell-prog-mode)))

(use-package apheleia
  :ensure t
  :init
  (apheleia-global-mode +1)

  :config
  (setf (alist-get 'ruff-format apheleia-formatters)
        '("ruff" "format" "-"))
  (setf (alist-get 'oxfmt apheleia-formatters)
        '("oxfmt" "--stdin"))

  ;; Optional: if you want Ruff to also apply safe fixes on save,
  ;; you can run "ruff check --fix" BEFORE formatting using a "sequential" formatter.
  ;; Uncomment if desired.
  ;;
  ;; (setf (alist-get 'ruff-fix+format apheleia-formatters)
  ;;       '((:formatter "ruff" "check" "--fix" "--exit-zero" "--stdin-filename" filepath "-")
  ;;         (:formatter "ruff" "format" "-")))

  (setf (alist-get 'python-mode apheleia-mode-alist) 'ruff-format)
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) 'ruff-format)

  (dolist (mode '(js-mode js-ts-mode
                          typescript-mode typescript-ts-mode
                          tsx-ts-mode
                          web-mode))
    (setf (alist-get mode apheleia-mode-alist) 'oxfmt))

  (dolist (mode '(json-mode json-ts-mode))
    (setf (alist-get mode apheleia-mode-alist) 'oxfmt))

  :bind (("C-c f" . apheleia-format-buffer)))

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
  (dired-listing-switches "-alh --group-directories-first")
  :config (dirvish-side-follow-mode)
  :bind (:map dirvish-mode-map
              ("TAB" . dirvish-subtree-toggle)
              ("h" . dired-up-directory)
              ("l" . dired-find-file)
              ("<backtab>" . dirvish-layout-toggle)))

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

;; Org
(use-package org
  :ensure nil
  :mode ("\\.org\\'" . org-mode)
  :hook ((org-mode . visual-line-mode)
         (org-mode . org-indent-mode)
         (org-mode . (lambda () (display-line-numbers-mode -1))))
  :config
  (setq org-startup-indented t
        org-hide-emphasis-markers t))

(use-package valign
  :ensure t
  :hook ((org-mode-hook . valign-mode))
  :custom (valign-fancy-bar t))

;; Translate
(use-package gt
  :ensure t
  :init
  (use-package posframe :ensure t)

  :config
  (setq gt-langs '(auto ru))
  (require 'gt-render-posframe)

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
  :bind (("C-M-y" . gt-translate)))

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

  (defun my/eaf-disable-header-line ()
    "Disable header line in EAF buffers."
    (setq-local header-line-format nil)
    (setq-local mode-line-format nil))

  (add-hook 'eaf-mode-hook #'my/eaf-disable-header-line)

  :bind (("C-c C-o" . eaf-open-url-at-point)
         :map my/c-z-z-map
         ("C-f" . eaf-open)
         ("u" . eaf-open-browser)
         ("h" . eaf-open-browser-with-history)
         ("s" . eaf-search-it)
         ("p" . eaf-open-pdf-from-history)))

;;; --- Smart open: EAF for PDFs, system apps for audio/video/images/etc ---
(require 'seq)
(require 'subr-x)
(require 'mailcap)

(defgroup my/open nil "My file opening rules." :group 'convenience)

(defcustom my/open-with-eaf-exts '("pdf" "epub")
  "File extensions to open via EAF."
  :type '(repeat string)
  :group 'my/open)

(defcustom my/open-with-system-exts
  '("png" "jpg" "jpeg" "gif" "webp" "svg"
    "mp4" "mkv" "mov" "webm" "avi" "m4v"
    "mp3" "flac" "wav" "ogg" "m4a" "opus"
    "zip" "7z" "rar" "tar" "gz" "bz2" "xz")
  "File extensions to open via system default apps (fallback)."
  :type '(repeat string)
  :group 'my/open)

(defcustom my/open-media-players
  '(("mpv.desktop" . "mpv")
    ("vlc.desktop" . "vlc")
    ("org.gnome.Celluloid.desktop" . "celluloid"))
  "Preferred media players when xdg-mime default matches a desktop entry."
  :type '(alist :key-type string :value-type string)
  :group 'my/open)

(defcustom my/dirvish-video-thumb-size 640
  "Max width (px) for Dirvish video thumbnails."
  :type 'integer
  :group 'dirvish)

(defun my/file-ext (path)
  (downcase (or (file-name-extension path) "")))

(defun my/local-file-p (path)
  (and path
       (not (file-remote-p path))
       (file-exists-p path)))

(defun my/mime-type (path)
  (or (mailcap-file-name-to-mime-type path) ""))

(defun my/xdg-mime-default (mime)
  (when (and (stringp mime) (not (string-empty-p mime))
             (executable-find "xdg-mime"))
    (with-temp-buffer
      (when (eq 0 (call-process "xdg-mime" nil t nil "query" "default" mime))
        (string-trim (buffer-string))))))

(defun my/media-file-p (path)
  (and (my/local-file-p path)
       (let ((mt (my/mime-type path)))
         (or (string-prefix-p "video/" mt)
             (string-prefix-p "audio/" mt)))))

(defun my/open-media-direct (path)
  (let* ((mime (my/mime-type path))
         (desk (my/xdg-mime-default mime))
         (cmd  (and desk (cdr (assoc desk my/open-media-players)))))
    (when (and cmd (executable-find cmd))
      (make-process :name "my-open-media" :command (list cmd path) :noquery t)
      t)))

(defun my/open-in-system-app (path)
  (unless (my/local-file-p path)
    (user-error "Not a local existing file: %s" path))
  (pcase system-type
    ('darwin
     (make-process :name "my-open" :command (list "open" path) :noquery t))
    ('windows-nt
     (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" path t t)))
    (_
     (cond
      ((and (my/media-file-p path)
            (my/open-media-direct path))
       t)
      ((executable-find "gio")
       (make-process :name "my-open" :command (list "gio" "open" path) :noquery t))
      ((executable-find "xdg-open")
       (make-process :name "my-open" :command (list "xdg-open" path) :noquery t))
      (t
       (user-error "No opener found (need xdg-open or gio)"))))))

(defun my/open-file-smart (path)
  (let* ((path (expand-file-name path))
         (ext  (my/file-ext path)))
    (cond
     ((member ext my/open-with-eaf-exts)
      (require 'eaf)
      (require 'eaf-pdf-viewer nil t)
      (require 'eaf-browser nil t)
      (eaf-open path)
      t)

     ((my/media-file-p path)
      (my/open-in-system-app path)
      t)

     ((and (my/local-file-p path)
           (member ext my/open-with-system-exts))
      (my/open-in-system-app path)
      t)

     (t nil))))

(defun my/find-file-around (orig file &rest args)
  "Route some file types to EAF or system apps."
  (let ((path (and file (expand-file-name file))))
    (if (and path (my/open-file-smart path))
        nil
      (apply orig file args))))

(advice-add 'find-file :around #'my/find-file-around)

(with-eval-after-load 'dired
  (defun my/dired-find-file-around (orig &rest args)
    (let ((path (dired-get-file-for-visit)))
      (if (my/open-file-smart path)
          nil
        (apply orig args))))
  (advice-add 'dired-find-file :around #'my/dired-find-file-around)

  (defun my/dired-do-find-marked-files-around (orig &rest args)
    (let ((files (dired-get-marked-files)))
      (if (and files (seq-every-p #'my/open-file-smart files))
          nil
        (apply orig args))))
  (advice-add 'dired-do-find-marked-files :around #'my/dired-do-find-marked-files-around))

(defun my/open-current-file-externally ()
  "Open current file (or Dired file at point) in system default app."
  (interactive)
  (let ((path (or buffer-file-name
                  (and (derived-mode-p 'dired-mode) (dired-get-file-for-visit)))))
    (unless path (user-error "No file here"))
    (my/open-in-system-app (expand-file-name path))))

(global-set-key (kbd "C-c x") #'my/open-current-file-externally)
