;; git clone https://git.savannah.gnu.org/git/emacs.git
;; cd emacs
;; git checkout emacs-30.2
;; ./autogen.sh
;; ./configure --prefix=/usr/local --with-x --with-x-toolkit=gtk3 --without-pgtk --with-native-compilation --with-tree-sitter --with-modules --with-cairo --with-imagemagick --with-xft
;; make -j$(nproc)
;; sudo make install

(setq custom-file "~/custom.el")

(declare-function cape-keyword "cape")
(declare-function ffap-file-at-point "ffap")

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

(set-face-attribute 'tab-bar nil :height 160)
(setq tab-bar-close-button-show nil
      tab-bar-new-button-show nil)

;; Maple Mono 17
(set-frame-font "FantasqueSansM Nerd Font 20" nil t)

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

(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)
(fset 'yes-or-no-p 'y-or-n-p)
(setq savehist-additional-variables '(register-alist))
(savehist-mode 1)
(which-key-mode 1)
(editorconfig-mode 1)

(prefer-coding-system 'utf-8-unix)
(setq-default buffer-file-coding-system 'utf-8-unix)

(custom-set-variables
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify)))

(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(setq whitespace-line-column 80)
(global-whitespace-mode t)

;; (global-completion-preview-mode 1)
(fido-vertical-mode 1)
(setq completion-styles '(basic flex)
      completions-format 'vertical
      completions-max-height 15
      completions-sort 'historical
      completion-auto-select nil)

(setq completions-detailed t
      completions-highlight-face 'completions-highlight)

(setq completion-auto-help 'visible
      completions-format 'one-column)

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

(defun open-file-at-point ()
  "Open the file name at point. Supports ~/ and ./ paths."
  (interactive)
  (require 'ffap)
  (let ((file (or (ffap-file-at-point) (thing-at-point 'filename t))))
    (if file
        (find-file (expand-file-name file))
      (user-error "No filename at point"))))

(global-unset-key (kbd "C-z"))

(keymap-global-set "C-v" #'scroll-half-page-up)
(keymap-global-set "M-v" #'scroll-half-page-down)
(keymap-global-set "C-x _" #'maximize-window)
(keymap-global-set "C-c c" #'project-compile)
(keymap-global-set "C-c C" #'compile)
(keymap-global-set "C-c C-b" #'grep)
(keymap-global-set "C-c C-l" (lambda () (interactive) (duplicate-line) (next-line)))
(keymap-global-set "C-c o" #'open-file-at-point)
(keymap-global-set "C-c C-o" #'browse-url-at-point)
(keymap-global-set "M-p" #'move-text-up)
(keymap-global-set "M-n" #'move-text-down)

;; Plugins

(require 'package)
(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Theme

(use-package almost-mono-themes
  :ensure t
  :config
  (add-to-list 'default-frame-alist '(undecorated . t))
  (load-theme 'almost-mono-black t)
  (setq frame-background-mode 'light)
  (when (memq 'almost-mono-black custom-enabled-themes)
    (custom-set-faces
     '(line-number ((t (:foreground "#3a3f5a" :background nil))))
     '(line-number-current-line ((t (:foreground "#c5c9c5" :background nil :weight bold))))
     '(tab-bar ((t (:background "#000000" :foreground "#c0caf5" :box nil))))
     '(tab-bar-tab ((t (:background "#222222" :foreground "#c5c9c5" :weight bold))))
     '(tab-bar-tab-inactive ((t (:background nil :foreground "#565f89" :box nil)))))
    (setq frame-background-mode 'dark)
    (set-frame-parameter nil 'alpha-background 70)
    (add-to-list 'default-frame-alist '(alpha-background . 70))
    (add-to-list 'default-frame-alist '(background-color . "#000000"))
    (set-face-background 'default "#000000")))

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
           ("Process" (name . "^\\*"))
           )))

  (setq ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-always-show-last-buffer nil)
  (setq ibuffer-default-sorting-mode 'recency)
  (setq ibuffer-human-readable-size t)

  :bind (("C-x C-b" . ibuffer)
         :map ibuffer-mode-map
         ("g" . ibuffer-update)
         ("/" . ibuffer-filter-by-mode)
         ("C-c / n" . ibuffer-filter-by-name)
         ("C-c / f" . ibuffer-filter-by-filename)
         ("C-c / c" . ibuffer-clear-filter-groups)))

(defun buffers-with-mode (mode)
  (let* ((name (if (symbolp mode) (symbol-name mode) mode))
         (mode-sym (intern (if (string-suffix-p "-mode" name)
                               name
                             (concat name "-mode"))))
         (alt-sym  (intern (string-remove-suffix "-mode" name))))
    (seq-filter
     (lambda (buf)
       (with-current-buffer buf
         (or (derived-mode-p mode-sym alt-sym)
             (and (boundp mode-sym) (symbol-value mode-sym))
             (and (boundp alt-sym)  (symbol-value alt-sym)))))
     (buffer-list))))

(defun buffer-names-with-mode (mode)
  (mapcar #'buffer-name (buffers-with-mode mode)))

(defun switch-to-buffer-with-mode (mode)
  (interactive)
  (let* ((buffers (buffers-with-mode mode))
         (names   (mapcar #'buffer-name buffers)))
    (switch-to-buffer
     (completing-read "Switch to buffer: " names nil t))))

(defun my/mode-match-p (mode)
  (let* ((s (if (symbolp mode) (symbol-name mode) (format "%s" mode)))
         (pair (if (string-match "\\`\\(.+\\)-mode\\'" s)
                   (cons (intern s) (intern (match-string 1 s)))
                 (cons (intern (concat s "-mode")) (intern s))))
         (m (car pair))
         (alt (cdr pair)))
    (or (derived-mode-p m alt)
        (and (boundp m) (symbol-value m))
        (and (boundp alt) (symbol-value alt)))))

(defun my/candidate->buffer (cand)
  (cond
   ((bufferp cand) cand)
   ((stringp cand) (get-buffer cand))
   ((consp cand)
    (or (my/candidate->buffer (cdr cand))
        (my/candidate->buffer (car cand))))
   (t nil)))

(defun my/buffer-has-any-mode-p (buf modes)
  (with-current-buffer buf
    (catch 'hit
      (dolist (m modes)
        (when (my/mode-match-p m) (throw 'hit t)))
      nil)))

(defun my/switch-to-buffer-excluding-modes (excluded-modes)
  (interactive)
  (let* ((excluded (if (listp excluded-modes) excluded-modes (list excluded-modes)))
         (pred (lambda (cand)
                 (let ((buf (my/candidate->buffer cand)))
                   (and buf (not (my/buffer-has-any-mode-p buf excluded)))))))
    (switch-to-buffer
     (read-buffer "Switch to buffer: " (other-buffer) t pred))))

(keymap-global-set "C-z C-z b" (lambda () (interactive) (switch-to-buffer-with-mode 'eaf-mode)))
(keymap-global-set "C-z b" (lambda () (interactive) (switch-to-buffer-with-mode 'prog-mode)))
(keymap-global-set "C-x b" (lambda () (interactive) (my/switch-to-buffer-excluding-modes '(eaf-mode prog-mode))))

;; Term/Shell

(use-package vterm
  :ensure t
  :commands vterm
  :hook (vterm-mode . (lambda () (display-line-numbers-mode -1)))
  :bind (("C-c t" . vterm)))

(use-package eshell
  :ensure nil
  :hook (eshell-mode . (lambda () (display-line-numbers-mode -1)))
  :config
  (setq eshell-scroll-to-bottom-on-input 'all)
  :bind (("C-c e" . eshell)))

;; Undo tree

(use-package undo-tree
  :ensure t
  :init
  (setq undo-tree-visualizer-timestamps t
        undo-tree-history-directory-alist `(("." . ,(expand-file-name "undo-history" user-emacs-directory)))
        undo-tree-auto-save-history t
        undo-limit 10000000
        undo-strong-limit 10000000)
  :config
  (global-undo-tree-mode))

;;; Git

(use-package magit
  :ensure t
  :commands (magit-status magit-dispatch)
  :bind (("C-c g g" . magit-status)))

(use-package git-timemachine
  :vc (:url "https://codeberg.org/pidu/git-timemachine"
            :rev :newest
            :branch "master")
  :bind (("C-c g t" . git-timemachine)))

;; Expand region

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

;; Jumps

(use-package avy
  :vc (:url "https://github.com/abo-abo/avy"
            :rev :newest
            :branch "master")
  :bind (("C-'" . avy-goto-word-1)))

;; LSP

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
        ("C-c l f" . eglot-format)
        ("C-c l a" . eglot-code-actions)
        ("C-c l r" . eglot-rename)))

(use-package yasnippet
  :vc (:url "https://github.com/joaotavora/yasnippet"
            :rev :newest
            :branch "master")
  :ensure t
  :config
  (yas-global-mode 1))

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :config
  (setq corfu-auto t
        corfu-cycle t))

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode 1)
  (setq corfu-auto nil
        corfu-cycle t
        corfu-echo-delay 1.0
        corfu-popupinfo-delay 1.0))

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
  :ensure t
  :init (global-flycheck-mode))

(use-package apheleia
  :ensure t
  :config
  (apheleia-global-mode +1))

(use-package typescript-ts-mode
  :mode (("\\.ts\\'"  . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode)))

;; AI

(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev :newest
            :branch "main")
  :bind (("C-c a c" . copilot-mode)
         :map copilot-completion-map
         ("<tab>" . copilot-accept-completion)
         ("TAB" . copilot-accept-completion)))

;; Utils

;;; Video player

(use-package emms
  :ensure t)

;; https://github.com/lorniu/mpvi
;; ~/.config/mpv/input.conf
;; %APPDATA%\mpv\input.conf
;;;  Ctrl+r cycle-values video-rotate "0" "270" "180" "90"
(use-package mpvi
  :ensure t
  :config
  (setq mpvi-mpv-ontop-p t)
  :bind (("C-c C-v" . mpvi-play)
         ("C-c v s" . mpvi-speed)))

;; Other

(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")
(require 'eaf)
(require 'eaf-browser)
(require 'eaf-org-previewer)
(require 'eaf-image-viewer)
(require 'eaf-music-player)
(require 'eaf-video-player)
(require 'eaf-markdown-previewer)
(require 'eaf-file-manager)
(require 'eaf-pdf-viewer)

;; (defun adviser-find-file (orig-fn file &rest args)
;;   (let ((fn (if (commandp 'eaf-open) #'eaf-open orig-fn)))
;;     (pcase (file-name-extension file)
;;       ((or "pdf" "epub")
;;        (funcall fn file))
;;       (_
;;        (apply orig-fn file args)))))

;; (advice-add #'find-file :around #'adviser-find-file)

(setq eaf-browser-translate-language "en")
(setq eaf-browser-continue-where-left-off t)
(setq eaf-browser-enable-adblocker t)
(setq eaf-browser-default-search-engine "duckduckgo")
(setq eaf-browse-blank-page-url "https://duckduckgo.com")
(setq eaf-pdf-dark-mode "force")

(setq browse-url-browser-function #'eaf-open-browser)
(defalias 'browse-web #'eaf-open-browser)

(keymap-global-set "C-c C-o"   #'eaf-open-url-at-point)
(keymap-global-set "C-z C-j"   #'eaf-open-in-file-manager)
(keymap-global-set "C-z C-z f" #'eaf-open)
(keymap-global-set "C-z C-z u" #'eaf-open-browser)
(keymap-global-set "C-z C-z h" #'eaf-open-browser-with-history)
(keymap-global-set "C-z C-z s" #'eaf-search-it)
(keymap-global-set "C-z C-z p" #'eaf-open-pdf-from-history)
