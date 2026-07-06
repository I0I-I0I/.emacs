;;; init.el --- Emacs init -*- lexical-binding: t; -*-

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

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
(setq-default buffer-file-coding-system 'utf-8-unix)

;; Set PATH
(defun my/add-to-path (dir)
  (let ((path (expand-file-name dir)))
    (when (file-directory-p path)
      (unless (member path exec-path)
        (setq exec-path (cons path exec-path)))
      (let* ((current (or (getenv "PATH") ""))
             (parts (split-string current path-separator t)))
        (unless (member path parts)
          (setenv "PATH" (mapconcat #'identity (cons path parts) path-separator)))))))

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

(unless (package-installed-p 'use-package)
  (unless package-archive-contents
    (package-refresh-contents))
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
    :config
    (defun my/display-line-numbers--turn-on-unless-text (orig &rest args)
      "Turn on line numbers only outside `text-mode' buffers."
      (unless (derived-mode-p 'text-mode)
        (apply orig args)))

    (advice-add 'display-line-numbers--turn-on
                :around #'my/display-line-numbers--turn-on-unless-text)
    (setq display-line-numbers-type 'relative)
    (global-display-line-numbers-mode)
    :bind
    ("C-x L" . global-display-line-numbers-mode))

  (setq gc-cons-threshold (* 64 1024 1024)
        gc-cons-percentage 0.1
        read-process-output-max (* 4 1024 1024)
        auto-save-default t
        make-backup-files t
        backup-directory-alist `(("." . ,(expand-file-name "backups/" user-emacs-directory)))
        auto-save-file-name-transforms
        `((".*" ,(expand-file-name "auto-save-list/" user-emacs-directory) t)))

  (when (boundp 'native-comp-async-report-warnings-errors)
    (setq native-comp-async-report-warnings-errors nil))
  (when (boundp 'native-comp-deferred-compilation)
    (setq native-comp-deferred-compilation t))

  (run-with-idle-timer 10 t #'garbage-collect)

  (setq-default fill-column 1000
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

  (setq delete-pair-push-mark t
        kill-region-dwim 'emacs-word
        native-comp-async-on-battery-power nil
        view-lossage-auto-refresh t
        display-fill-column-indicator-warning nil
        dired-hide-details-hide-absolute-location t)

  (add-hook 'prog-mode-hook
            (lambda ()
              (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)))
  (save-place-mode 1)
  (global-hl-line-mode 1)
  (repeat-mode 1)

  (define-globalized-minor-mode global-mode-line-invisible-mode
    mode-line-invisible-mode
    (lambda () (mode-line-invisible-mode 1)))
  (global-mode-line-invisible-mode 1)

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

  ;; Vim-like C-r C-w for minibuffer prompts.
  (defun my-minibuffer-insert-symbol-at-point ()
    "Insert the symbol at point from the buffer that opened the minibuffer."
    (interactive)
    (let* ((window (minibuffer-selected-window))
           (text (when (window-live-p window)
                   (with-current-buffer (window-buffer window)
                     (save-excursion
                       (goto-char (window-point window))
                       (or (thing-at-point 'symbol t)
                           (thing-at-point 'word t)))))))
      (unless text
        (user-error "No symbol at point"))
      (insert (substring-no-properties text))))

  (defun my/copy-current-line-number ()
    "Copy the current line number to the kill ring."
    (interactive)
    (kill-new (number-to-string (line-number-at-pos)))
    (message "Copied line number: %s" (current-kill 0)))

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
   ("M-S-<left>" . org-decrease-number-at-point)
   ("C-x m" . global-mode-line-invisible-mode)
   ("C-c L" . my/copy-current-line-number)
   ("C-x F" . toggle-frame-fullscreen)
   :map minibuffer-local-map
   ("M-l" . my-minibuffer-insert-symbol-at-point)
   :map minibuffer-local-ns-map
   ("M-l" . my-minibuffer-insert-symbol-at-point)
   :map minibuffer-local-completion-map
   ("M-l" . my-minibuffer-insert-symbol-at-point)
   :map minibuffer-local-must-match-map
   ("M-l" . my-minibuffer-insert-symbol-at-point)))

(use-package crux
  :ensure t
  :preface
  (declare-function crux-indent-region-region-or-buffer "crux")
  (declare-function crux-untabify-region-or-buffer "crux")
  (declare-function crux-comment-or-uncomment-region-region-or-line "crux")
  (declare-function crux-kill-region-region-or-sexp-or-line "crux")
  (declare-function crux-kill-ring-save-region-or-point-to-eol "crux")
  :config
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-buffer untabify)
  (crux-with-region-or-line comment-or-uncomment-region)
  (crux-with-region-or-sexp-or-line kill-region)
  (crux-with-region-or-point-to-eol kill-ring-save)
  :bind (("C-a"     . crux-move-beginning-of-line)
         ;; ("C-c o"   . crux-open-with)
         ("C-c C-D" . crux-delete-file-and-buffer)
         ("C-c r"   . crux-rename-file-and-buffer)
         ("C-c k"   . crux-kill-other-buffers)
         ("C-c d"   . crux-duplicate-current-line-or-region)
         ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region)
         ("C-c n"   . crux-cleanup-buffer-or-region)
         ("C-c f"   . crux-recentf-find-file)
         ("C-c F"   . crux-recentf-find-directory)
         ("C-c U"   . crux-view-url)
         ("C-c E"   . crux-eval-and-replace)
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

;; Mode line
(defvar my/mode-line-extra-format nil
  "Extra mode line segments appended after the core mode line.")

(defun my/mode-line-buffer-name ()
  "Return the project-relative file name, or the buffer name for non-file buffers."
  (if buffer-file-name
      (if-let* ((project (unless (file-remote-p buffer-file-name)
                           (project-current nil))))
          (file-relative-name buffer-file-name (project-root project))
        (abbreviate-file-name buffer-file-name))
    (buffer-name)))

(defun my/mode-line-perspective ()
  "Return the current Perspective name for the mode line."
  (when (and (bound-and-true-p persp-mode)
             (fboundp 'persp-current-name))
    (let ((name (persp-current-name)))
      (unless (string= name "")
        (format "[%s] " name)))))

(defun my/mode-line-vc ()
  "Return compact VC branch info."
  (when vc-mode
    (format " %s" (string-trim vc-mode))))

(setq-default
 mode-line-format
 '("%e "
   (:eval (my/mode-line-perspective))
   "%* "
   (:eval (my/mode-line-buffer-name))
   "  ("
   mode-name
   ")"
   (:eval (my/mode-line-vc))
   mode-line-misc-info
   my/mode-line-extra-format
   mode-line-end-spaces))

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

;;; Spelling
(use-package jinx
  :ensure t
  :hook (emacs-startup . global-jinx-mode)
  :custom
  (jinx-languages "en_US-large ru_RU")
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)
         :map jinx-overlay-map
         ("M-n" . nil)
         ("M-p" . nil)
         ("M-N" . jinx-next)
         ("M-P" . jinx-previous)))

;;; Server
(use-package server
  :ensure nil
  :config
  (unless (or noninteractive (server-running-p))
    (condition-case err
        (server-start)
      (file-error
       (message "Could not start Emacs server: %s" (error-message-string err))))))

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
  :hook
  (eshell-mode . (lambda () (setq-local corfu-auto nil)))
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
  (require 'corfu-history)
  (require 'corfu-popupinfo)
  (corfu-history-mode 1)
  (corfu-popupinfo-mode 1)
  :init (global-corfu-mode))

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (require 'cape-keyword)
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

  :bind (("C-x C-M-b" . ibuffer)
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
(use-package vterm
  :ensure t
  :commands (vterm)
  :hook (vterm-mode . (lambda () (display-line-numbers-mode -1)))
  :bind ("C-c t" . vterm))

(use-package eshell
  :ensure nil
  :preface
  (declare-function bash-completion-dynamic-complete-nocomint "bash-completion")
  (declare-function eshell-bol "em-prompt")
  (declare-function eshell/clear "em-script")
  (declare-function w32-shell-execute "w32fns")
  (require 'project)
  (require 'ring)
  (require 'seq)
  (require 'subr-x)

  (defun my/eshell--buffer-p (b)
    (with-current-buffer b
      (derived-mode-p 'eshell-mode)))

  (defun my/eshell--buf-dir (b)
    "Return normalized `default-directory` for eshell buffer B."
    (with-current-buffer b
      (expand-file-name default-directory)))

  (defun my/eshell--current-dir ()
    (expand-file-name default-directory))

  (defun my/eshell--find-by-dir (dir)
    "Find an existing eshell buffer whose `default-directory` matches DIR."
    (let ((target (expand-file-name dir)))
      (seq-find (lambda (b)
                  (and (my/eshell--buffer-p b)
                       (string= (my/eshell--buf-dir b) target)))
                (buffer-list))))

  (defun my/eshell--new-in-dir (dir)
    "Create a new eshell buffer and start it in DIR."
    (let ((default-directory dir))
      (eshell t)))

  (defvar my/eshell--return-buffers (make-hash-table :test 'eq :weakness 'key)
    "Eshell buffers mapped to the buffer they should toggle back to.")

  (defun my/eshell--open-new (dir origin)
    "Create an eshell in DIR and remember ORIGIN as its return buffer."
    (my/eshell--new-in-dir dir)
    (when (my/eshell--buffer-p (current-buffer))
      (puthash (current-buffer) origin my/eshell--return-buffers)))

  (defun my/eshell--return-from (buf)
    "Switch from eshell BUF back to its remembered buffer."
    (let ((origin (gethash buf my/eshell--return-buffers)))
      (if (buffer-live-p origin)
          (switch-to-buffer origin)
        (switch-to-buffer (other-buffer buf t)))))

  (defun my/eshell--toggle-buffer (buf dir)
    "Toggle to BUF, or create an eshell in DIR when BUF is nil."
    (let ((origin (current-buffer)))
      (cond
       ((eq origin buf)
        (my/eshell--return-from buf))
       (buf
        (puthash buf origin my/eshell--return-buffers)
        (switch-to-buffer buf))
       (t
        (my/eshell--open-new dir origin)))))

  (defun my/toggle-eshell-here (&optional new)
    "Toggle an eshell buffer for the current directory.
With prefix argument NEW, always create a new eshell buffer."
    (interactive "P")
    (let* ((dir (my/eshell--current-dir))
           (buf (my/eshell--find-by-dir dir)))
      (if new
          (my/eshell--open-new dir (current-buffer))
        (my/eshell--toggle-buffer buf dir))))

  (defun my/eshell--project-dir ()
    "Return project root if available, else current `default-directory`."
    (if-let* ((pr (project-current nil)))
        (expand-file-name (project-root pr))
      (my/eshell--current-dir)))

  (defun my/toggle-eshell-project (&optional new)
    "Toggle an eshell buffer for the project root.
With prefix argument NEW, always create a new eshell buffer."
    (interactive "P")
    (let* ((dir (my/eshell--project-dir))
           (buf (my/eshell--find-by-dir dir)))
      (if new
          (my/eshell--open-new dir (current-buffer))
        (my/eshell--toggle-buffer buf dir))))

  (defun my/eshell-history-fuzzy ()
    (interactive)
    (require 'em-hist)
    (let ((items (and eshell-history-ring
                      (delete-dups (ring-elements eshell-history-ring)))))
      (if (not items)
          (user-error "No Eshell history")
        (let ((choice (completing-read "Eshell history: " items nil t)))
          (when (and choice (not (string-empty-p choice)))
            (let ((bol (save-excursion
                         (eshell-bol)
                         (point))))
              (delete-region bol (point))
              (insert choice)))))))

  :hook (eshell-mode . (lambda ()
                         (display-line-numbers-mode -1)))
  :init
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

  :bind (("C-c e" . my/toggle-eshell-here)
         ("C-x p e" . my/toggle-eshell-project))

  :config
  (setq eshell-modules-list '(eshell-alias
                              eshell-basic
                              eshell-cmpl
                              eshell-dirs
                              eshell-glob
                              eshell-hist
                              eshell-ls
                              eshell-pred
                              eshell-prompt
                              eshell-script
                              eshell-term
                              eshell-tramp
                              eshell-unix)
        eshell-command-aliases-list '(("la" "ls -Alhvp --group-directories-first --color=always")
                                      ("ll" "ls -lh --group-directories-first --color=always")
                                      ("py" "uv run python"))
        eshell-save-history-on-exit t
        eshell-visual-commands '()
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
    (let ((target (if args (string-join args " ") ".")))
      (cond ((eq system-type 'darwin) (start-process "open" nil "open" target))
            ((eq system-type 'windows-nt) (w32-shell-execute "open" target))
            ((executable-find "xdg-open") (start-process "xdg-open" nil "xdg-open" target))
            (t (user-error "No opener found")))))

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
  (require 'diff-hl-show-hunk)
  (global-diff-hl-show-hunk-mouse-mode))

(use-package git-timemachine
  :vc (:url "https://codeberg.org/pidu/git-timemachine"
            :rev :newest
            :branch "master")
  :bind (("C-x v t" . git-timemachine)))

;; Treesitter
(setq treesit-language-source-alist
      '((elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (svelte "https://github.com/tree-sitter-grammars/tree-sitter-svelte")))

(when (fboundp 'treesit-install-language-grammar)
  (unless (treesit-language-available-p 'svelte)
    (treesit-install-language-grammar 'svelte)))

(setopt treesit-enabled-modes t
        treesit-auto-install-grammar 'always)

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
         ("M-N" . mc/skip-to-next-like-this)
         ("M-P" . mc/skip-to-previous-like-this)
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
  :preface
  (defun my/better-jumper--set-jump-before (&rest _)
    (better-jumper-set-jump))
  :init
  (better-jumper-mode 1)
  :config
  (setq better-jumper-context 'window)

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
  :hook ((html-mode css-mode sgml-mode web-mode js-jsx-mode tsx-ts-mode svelte-ts-mode) . emmet-mode))

;; Docker
(use-package docker
  :ensure t
  :bind ("C-c C-d" . docker))

;; AI
(use-package copilot
  :ensure t
  :bind (("C-c A" . copilot-mode)
         :map copilot-completion-map
         ("<tab>" . copilot-accept-completion)
         ("TAB" . copilot-accept-completion)))

(use-package pi-coding-agent
  :ensure t
  :bind
  ("C-c a" . pi-coding-agent)
  (:map pi-coding-agent-chat-mode-map
        ("C-c a" . pi-coding-agent-toggle))
  (:map pi-coding-agent-input-mode-map
        ("C-c a" . pi-coding-agent-toggle)))

;; (use-package codeium
;;   :vc (:url "https://github.com/Exafunction/codeium.el"
;;             :rev :newest)
;;   :init
;;   (add-to-list 'completion-at-point-functions #'codeium-completion-at-point))

;; (use-package agent-shell
;;   :ensure t
;;   :config
;;   ;; (setq agent-shell-openai-authentication
;;   ;;       (agent-shell-openai-make-authentication :login t))
;;   (setq agent-shell-google-authentication
;;         (agent-shell-google-make-authentication :login t))
;;   :bind (("C-c a" . agent-shell)
;;          :map agent-shell-mode-map
;;          ("RET" . newline)
;;          ("C-c C-c" . shell-maker-submit)
;;          ("C-c C-k" . agent-shell-interrupt)))

;; Zen mode
(use-package sublimity
  :ensure t
  :config
  (require 'sublimity-attractive)
  (sublimity-mode)
  :bind (("C-c z" . sublimity-mode)))

;; Dired
(use-package dired
  :ensure nil
  :hook
  (dired-mode . dired-omit-mode)
  (dired-mode . (lambda () (display-line-numbers-mode -1)))
  :custom
  (dired-omit-files (rx string-start (or "." "..") string-end))
  (dired-isearch-filenames 'dwim)
  (dired-create-destination-dirs 'always)
  (dired-create-destination-dirs-on-trailing-dirsep t)
  (confirm-nonexistent-file-or-buffer nil)
  (delete-by-moving-to-trash 'move-file-to-trash)
  (dired-listing-switches
   (if (eq system-type 'darwin)
       "-alh"
     "-alh --group-directories-first"))
  :config
  (defun dired-subdir-aware (orig-fun &rest args)
    (if (eq major-mode 'dired-mode)
        (let ((default-directory (dired-current-directory)))
          (apply orig-fun args))
      (apply orig-fun args)))

  (dolist (fun '(find-file-read-args ido-expand-directory))
    (advice-add fun :around 'dired-subdir-aware))
  :bind (:map dired-mode-map
              ("h" . dired-up-directory)
              ("l" . dired-find-file)
              ("C-u i" . dired-kill-subdir)))

(use-package dired-clipboard
  :vc (:url "https://github.com/kn66/dired-clipboard.el"
            :rev :newest
            :branch "main")
  :hook (dired-mode . dired-clipboard-mode))

(use-package ready-player
  :ensure t
  :custom
  (ready-player-autoplay nil)
  (ready-player-thumbnail-max-pixel-height 500)
  :config (ready-player-mode))

(use-package dired-preview
  :ensure t
  :custom
  (dired-preview-delay 0.2)
  (dired-preview-max-size (expt 2 20))
  (dired-preview-ignored-extensions-regexp nil)
  (dired-preview-display-action-alist
   '((display-buffer-in-side-window)
     (side . right)
     (window-width . 0.5)
     (preserve-size . (t . t))))
  :bind (:map dired-mode-map
              ("C-c C-p" . dired-preview-mode)))

;; Tmux  Perspective

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
  (persp-show-modestring nil)
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

(use-package org-download
  :ensure t
  :vc (:url "https://github.com/abo-abo/org-download"
            :rev :newest
            :branch "master")
  :hook
  (dired-mode-hook . org-download-enable)
  :custom
  (org-download-method 'directory)
  (org-download-image-dir "images")
  (org-download-heading-lvl nil)
  (org-download-timestamp "%Y%m%d-%H%M%S_")

  :bind
  (:map org-mode-map
        ("C-c C-y" . org-download-clipboard)))

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
  :ensure nil
  :preface
  (define-prefix-command 'my/org-prefix-map)

  (defun my/org-confirm-babel-evaluate (lang _body)
    (not (member lang '("emacs-lisp"))))

  (defun my/org-find-note ()
    "Open `find-file' from `org-directory' to search or create notes."
    (interactive)
    (let ((default-directory (file-name-as-directory org-directory)))
      (call-interactively #'find-file)))
  :init
  (global-set-key (kbd "C-z") my/org-prefix-map)

  (use-package calendar
    :ensure nil
    :custom
    (calendar-week-start-day 1)
    (calendar-mark-diary-entries-flag t)
    (calendar-mark-holidays-flag t)
    :bind
    (:map my/org-prefix-map
          ("k" . calendar)))

  :mode ("\\.org\\'" . org-mode)
  :hook
  ((org-mode . visual-line-mode)
   (org-mode . variable-pitch-mode)
   (org-mode . org-indent-mode))
  :custom
  ;; Editing
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-insert-heading-respect-content t)
  (org-M-RET-may-split-line '((default . nil)))

  (org-startup-indented t)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-ellipsis " ▾")
  (org-return-follows-link t)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-edit-src-content-indentation 0)
  (org-confirm-babel-evaluate #'my/org-confirm-babel-evaluate)
  (org-startup-with-inline-images t)
  (org-startup-with-latex-preview t)

  (org-src-preserve-indentation t)
  (org-src-window-setup 'current-window)
  (org-confirm-babel-evaluate #'my/org-confirm-babel-evaluate)

  (org-directory (expand-file-name "~/Dropbox/Apps/remotely-save/notes/"))
  (org-agenda-files (list org-directory))
  (org-agenda-span 7)
  (org-deadline-warning-days 7)
  (org-agenda-start-with-log-mode t)

  (org-agenda-custom-commands
   '(("d" "Daily Agenda"
      ((agenda "" ((org-agenda-span 'day)))))))

  (org-default-notes-file (expand-file-name "inbox.org" org-directory))
  (org-todo-keywords
   '((sequence "TODO(t)" "WAIT(w@)" "|" "DONE(d!)" "CANCELLED(c@)")))
  (org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (org-outline-path-complete-in-steps nil)
  (org-refile-use-outline-path 'file)
  (org-capture-templates
   '(("t" "Task" entry
      (file "inbox.org")
      "* TODO %?\n")))

                                        ; (org-preview-latex-default-process 'dvipng)
  (org-preview-latex-default-process 'dvisvgm)
  (org-format-latex-options
   (plist-put org-format-latex-options :scale 1.6))

  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t)))
  :bind
  (:map my/org-prefix-map
        ("a" . org-agenda)
        ("c" . org-capture)
        ("n" . my/org-find-note)
        ("T" . org-todo-list)
        :map org-mode-map
        ("C-c C-d" . org-deadline)))

;; LSP

(use-package mason
  :ensure t
  :demand t
  :config
  (mason-ensure))

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
  (eglot-events-buffer-size 0)
  :config
  (setq-default eglot-workspace-configuration
                '(:ty (:diagnosticMode "workspace")
                      ;; vtsls asks Eglot for section "", so it needs a whole
                      ;; VS-Code-style configuration tree here rather than only
                      ;; :typescript/:javascript sections.
                      "" (:typescript (:tsserver (:experimental (:enableProjectDiagnostics t)))
                                      :javascript (:tsserver (:experimental (:enableProjectDiagnostics t))))
                      :typescript (:tsserver (:experimental (:enableProjectDiagnostics t)))
                      :javascript (:tsserver (:experimental (:enableProjectDiagnostics t)))))

  (cl-defmethod eglot-client-capabilities :around ((server eglot-lsp-server))
    "Advertise generic workspace diagnostic support to LSP servers."
    (let ((capabilities (cl-call-next-method)))
      (plist-put (plist-get capabilities :workspace)
                 :diagnostics '(:refreshSupport t))
      capabilities))
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
           ("rass" "--" "vtsls" "--stdio" "--" "oxlint" "--lsp")
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

  (defun my/eglot-workspace-diagnostics--supports-lsp-p ()
    "Return non-nil if the current Eglot server advertises workspace diagnostics."
    (eglot-server-capable :diagnosticProvider :workspaceDiagnostics))

  (defun my/eglot-workspace-diagnostics--list-only-p (entry server)
    "Return non-nil if ENTRY is a list-only diagnostic for SERVER."
    (let ((file (car entry)))
      (and (stringp file)
           (> (length file) 0)
           (eq (get-text-property 0 'eglot--server file) server))))

  (defun my/eglot-workspace-diagnostics--has-list-only-p (server)
    "Return non-nil if Flymake already has project diagnostics for SERVER."
    (seq-some
     (lambda (entry)
       (my/eglot-workspace-diagnostics--list-only-p entry server))
     flymake-list-only-diagnostics))

  (defun my/eglot-workspace-diagnostics--clear (server)
    "Clear workspace diagnostics previously stored for SERVER by this command."
    (setq flymake-list-only-diagnostics
          (cl-remove-if
           (lambda (entry)
             (let ((file (car entry)))
               (and (my/eglot-workspace-diagnostics--list-only-p entry server)
                    (get-text-property 0 'my/eglot-workspace-diagnostics file))))
           flymake-list-only-diagnostics)))

  (defun my/eglot-workspace-diagnostics--mark (server)
    "Mark current list-only diagnostics for SERVER as owned by this command."
    (setq flymake-list-only-diagnostics
          (mapcar
           (lambda (entry)
             (let ((file (car entry)))
               (when (my/eglot-workspace-diagnostics--list-only-p entry server)
                 (put-text-property 0 (length file)
                                    'my/eglot-workspace-diagnostics t
                                    file))
               entry))
           flymake-list-only-diagnostics)))

  (defun my/eglot-workspace-diagnostics--show-pushed (server)
    "Show diagnostics that SERVER already pushed for unopened files."
    (message "%s has pushed project diagnostics; showing Flymake project diagnostics"
             (eglot--server-name server))
    (flymake-show-project-diagnostics))

  (defun my/eglot-workspace-diagnostics--request (server)
    "Request workspace diagnostics from SERVER and store them in Flymake."
    (let* ((report (eglot--request
                    server
                    :workspace/diagnostic
                    '(:previousResultIds [])
                    :timeout 60))
           (items (append (plist-get report :items) nil))
           (diagnostic-count 0))
      (my/eglot-workspace-diagnostics--clear server)
      (dolist (item items)
        (when (equal (plist-get item :kind) "full")
          (cl-incf diagnostic-count (length (plist-get item :items)))
          (eglot--flymake-handle-push
           server
           (plist-get item :uri)
           (plist-get item :items)
           (plist-get item :version)
           #'ignore)))
      (my/eglot-workspace-diagnostics--mark server)
      (message "%s workspace diagnostics: %d diagnostics in %d reports"
               (eglot--server-name server) diagnostic-count (length items))
      (flymake-show-project-diagnostics)))

  (defun my/eglot-workspace-diagnostics ()
    "Fetch or display workspace/project diagnostics for the current Eglot server.

Prefer the standard LSP `workspace/diagnostic' request.  If a server does not
advertise that capability, try the request anyway so wrappers/proxies that omit
capabilities can still work.  When the request is unavailable but the server has
already pushed diagnostics for unopened files, fall back to Flymake's project
view."
    (interactive)
    (require 'cl-lib)
    (require 'flymake)
    (require 'seq)
    (let ((server (eglot-current-server)))
      (unless server
        (user-error "No Eglot server in current buffer"))
      (unless (my/eglot-workspace-diagnostics--supports-lsp-p)
        (message "%s does not advertise workspace diagnostics; trying anyway"
                 (eglot--server-name server)))
      (condition-case err
          (my/eglot-workspace-diagnostics--request server)
        (jsonrpc-error
         (if (my/eglot-workspace-diagnostics--has-list-only-p server)
             (my/eglot-workspace-diagnostics--show-pushed server)
           (user-error "%s failed workspace diagnostics: %s"
                       (eglot--server-name server)
                       (or (alist-get 'jsonrpc-error-message (cddr err))
                           (error-message-string err))))))))

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
  ("C-c C-n" . flymake-goto-next-error)
  ("C-c C-p" . flymake-goto-prev-error)
  ("C-c l d" . flymake-show-buffer-diagnostics)
  ("C-c l D" . my/eglot-workspace-diagnostics)
  ("C-c l ?" . flymake-show-diagnostic))

(use-package apheleia
  :ensure t
  :config
  (dolist (mode '(js-ts-mode
                  typescript-ts-mode
                  tsx-ts-mode
                  svelte-ts-mode))
    (setf (alist-get mode apheleia-mode-alist) 'oxfmt))

  (setf (alist-get 'python-ts-mode apheleia-mode-alist)
        '(ruff-isort ruff))

  (apheleia-global-mode +1))

(use-package svelte-ts-mode
  :vc (:url "https://github.com/leafOfTree/svelte-ts-mode"
            :rev :newest
            :branch "main")
  :ensure t
  :bind (:map svelte-ts-mode-map
              ("C-c C-n" . flymake-goto-next-error)
              ("C-c C-p" . flymake-goto-prev-error))
  :mode "\\.svelte\\'")

(use-package markdown-ts-mode
  :ensure t
  :defer t)

;; RSS

(use-package elfeed
  :ensure t
  :bind ("C-c f" . elfeed)
  :custom
  (elfeed-feeds
   '(
     ;; Emacs
     ("https://emacs.stackexchange.com/feeds" emacs q-and-a)
     ("https://protesilaos.com/codelog.xml" emacs linux)
     ("https://irreal.org/blog/?feed=rss2" emacs)
     ("https://sachachua.com/blog/feed/" emacs org)

     ;; Developer experience / platform engineering
     ("https://newsletter.pragmaticengineer.com/feed" dx management)
     ("https://platformweekly.com/feed" dx platform)
     ("https://platformengineering.org/blog/rss.xml" dx platform)
     ("https://cloud.google.com/blog/products/devops-sre/rss" dora devops)
     ("https://humanitec.com/blog/rss.xml" dx platform)
     ("https://leaddev.com/rss.xml" dx management)
     ("https://martinfowler.com/feed.atom" architecture)

     ;; LLMs / AI research
     ("https://openai.com/news/rss.xml" ai openai llm)
     ("https://huggingface.co/blog/feed.xml" ai huggingface llm ml)
     ("https://simonwillison.net/atom/everything/" ai llm tools)
     ("https://rss.arxiv.org/rss/cs.CL" ai research nlp llm)
     ("https://hnrss.org/newest?q=LLM+OR+language+model+OR+agent+OR+inference"
      ai llm news)

     ;; Programming / systems
     ("https://news.ycombinator.com/rss" programming news)
     ("https://lobste.rs/rss" programming systems)
     ("https://jvns.ca/atom.xml" programming linux systems)
     ("https://danluu.com/atom.xml" programming systems longform)
     ("https://lwn.net/headlines/rss" linux kernel opensource))))

(use-package remoto
  :vc (:url "https://github.com/agzam/remoto.el"
            :rev :newest
            :branch "main")
  :demand t)
