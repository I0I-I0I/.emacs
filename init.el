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
  :config
  (setq lambda-themes-set-italic-comments t
        lambda-themes-set-italic-keywords t
        lambda-themes-set-variable-pitch t)
  (load-theme 'lambda-dark t))

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  (setq-default header-line-format (doom-modeline-set-main-modeline))
  (setq-default mode-line-format nil)

  (setq doom-modeline-bar-width 0)
  (display-battery-mode 1)

  (dolist (face '(mode-line mode-line-inactive header-line))
    (set-face-attribute face nil :box nil))

  :custom
  (doom-modeline-icon nil)
  (doom-modeline-check nil)
  (doom-modeline-buffer-file-name-style 'relative-to-project)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-percent-position '(-4 "%p"))
  (doom-modeline-total-line-number t)
  (doom-modeline-position-line-format '("%l")))

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
  (setq register-preview-delay 0.5))

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
              ("C-`" . harpoon-quick-menu-hydra)
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

;; LSP
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook
  (((web-mode
     js-mode js-ts-mode js-jsx-mode
     typescript-mode typescript-ts-mode tsx-ts-mode
     css-mode css-ts-mode
     json-mode json-ts-mode
     yaml-mode yaml-ts-mode
     python-mode python-ts-mode) . lsp-deferred))
  :init
  (use-package flycheck
    :ensure t
    :init (global-flycheck-mode)
    :config
    (setq flycheck-display-errors-delay 0.25)
    (setq flycheck-indication-mode 'right-fringe)
    (setq flycheck-highlighting-mode 'symbols))

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


  (setq xref-show-xrefs-function #'xref-show-definitions-buffer)
  (setq xref-show-definitions-function #'xref-show-definitions-buffer)

  :config
  (setq lsp-completion-provider :none
        lsp-diagnostics-provider :flycheck
        lsp-eldoc-enable-hover nil
        lsp-signature-auto-activate t
        lsp-enable-on-type-formatting nil
        lsp-javascript-format-enable nil
        lsp-typescript-format-enable nil
        lsp-headerline-breadcrumb-enable nil
        lsp-idle-delay 0.3
        lsp-log-io nil
        lsp-format-buffer-on-save nil)

  (dolist (client '(basedpyright pylsp pyright ruff))
    (add-to-list 'lsp-disabled-clients client))

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("ty" "server"))
    :major-modes '(python-ts-mode python-mode)
    :server-id 'ty-ls
    :priority 1))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("ruff" "server"))
    :major-modes '(python-ts-mode python-mode)
    :server-id 'ruff-ls
    :priority 2
    :add-on? t))

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("oxlint" "--lsp"))
    :activation-fn (lambda (_filename major-mode)
                     (memq major-mode '(js-mode js-ts-mode typescript-mode typescript-ts-mode tsx-ts-mode)))
    :server-id 'oxlint-ls
    :add-on? t))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("oxfmt" "--lsp"))
    :activation-fn (lambda (_filename major-mode)
                     (memq major-mode '(js-mode js-ts-mode typescript-mode typescript-ts-mode tsx-ts-mode)))
    :server-id 'oxfmt-ls
    :add-on? t))
  :bind
  (:map lsp-mode-map
        ("M-." . xref-find-definitions)
        ("C-c M-." . lsp-find-type-definition)
        ("M-," . xref-go-back)
        ("M-?" . xref-find-references)
        ("C-h ." . lsp-describe-thing-at-point)
        ("C-c l s" . lsp-workspace-symbol)
        ("C-c l r" . lsp-rename)
        ("C-c l a" . lsp-execute-code-action)
        ("C-c l d" . flycheck-list-errors)
        ("C-c C-n" . flycheck-next-error)
        ("C-c C-p" . flycheck-previous-error)
        ("C-c l D" . lsp-diagnostics)
        ("C-c p" . previous-error)
        ("C-c n" . next-error)
        ("C-c l f" . lsp-format-buffer)))

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

(use-package flyspell
  :ensure nil
  :init
  (setq ispell-local-dictionary-alist
        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "['’]" nil ("-d" "en_US") nil utf-8)
          ("ru_RU" "[[:alpha:]]" "[^[:alpha:]]" "['’]" nil ("-d" "ru_RU") nil utf-8)
          ("en_US,ru_RU" "[[:alpha:]]" "[^[:alpha:]]" "['’]" nil ("-d" "en_US,ru_RU") nil utf-8)))
  (let ((aspell (executable-find "aspell"))
        (hunspell (executable-find "hunspell")))
    (cond
     (aspell
      (setq ispell-program-name aspell
            ispell-dictionary "en_US"
            ispell-extra-args '("--sug-mode=ultra")))
     (hunspell
      (setq ispell-program-name hunspell
            ispell-dictionary "en_US,ru_RU"
            ispell-extra-args nil))
     (t
      (setq ispell-program-name "ispell"
            ispell-dictionary "en_US"))))
  (setq flyspell-issue-message-flag nil
        flyspell-delay 0.5
        ispell-choices-win-default-height 20)
  :hook
  ((agent-shell-mode . flyspell-mode)
   (text-mode . flyspell-mode)
   (prog-mode . flyspell-prog-mode)))

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
  :bind (("C-z A" . copilot-mode)
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
  (setq agent-shell-openai-codex-command '("bunx" "@zed-industries/codex-acp"))
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


;; Org
(use-package org
  :ensure nil
  :mode ("\\.org\\'" . org-mode)
  :hook ((org-mode . visual-line-mode)
         (org-mode . org-indent-mode)
         (org-mode . (lambda () (display-line-numbers-mode -1))))
  :init
  (use-package valign
    :ensure t
    :hook ((org-mode-hook . valign-mode))
    :custom (valign-fancy-bar t))

  :config
  (setq org-startup-indented t
        org-hide-emphasis-markers t))

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

;; EAF
(use-package eaf
  :vc (:url "https://github.com/emacs-eaf/emacs-application-framework"
            :rev :newest
            :branch "master")
  :commands (eaf-open-browser eaf-open eaf-search-it eaf-open-url-at-point eaf-open-browser-with-history eaf-open-pdf-from-history)
  :demand t
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
