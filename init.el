(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
                                        ; (load custom-file 'noerror 'nomessage)

(push '(fullscreen . maximized) default-frame-alist)

(defvar my/gc-cons-threshold-orig gc-cons-threshold)
(defvar my/gc-cons-percentage-orig gc-cons-percentage)

;; Font
(when (display-graphic-p)
  (when (find-font (font-spec :family "Maple Mono NF"))
    (set-frame-font
     (pcase system-type
       ('windows-nt "Maple Mono NF-16")
       ('gnu/linux  "Maple Mono NF-20"))
     t t)))

;; windows specific
(when (eq system-type 'windows-nt)
  (prefer-coding-system 'utf-8-unix)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix))
  (set-selection-coding-system 'utf-16-le)
  (set-clipboard-coding-system 'utf-16-le)
  (setenv "PATH"
          (concat "C:/Program Files/Git/usr/bin;"
                  (getenv "PATH")))
  (add-to-list 'exec-path "C:/Program Files/Git/usr/bin"))

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
  (setq gc-cons-threshold (* 256 1024 1024)
        gc-cons-percentage 0.6
        read-process-output-max (* 4 1024 1024)
        comp-async-jobs-number 8
        comp-deferred-compilation t)

  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq gc-cons-threshold my/gc-cons-threshold-orig
                    gc-cons-percentage my/gc-cons-percentage-orig)
              (run-with-idle-timer 5 nil #'garbage-collect)))

  (setq auto-save-default nil
        make-backup-files nil)

  (when (boundp 'native-comp-async-report-warnings-errors)
    (setq native-comp-async-report-warnings-errors nil))
  (when (boundp 'native-comp-deferred-compilation)
    (setq native-comp-deferred-compilation t))

  (run-with-idle-timer 10 t #'garbage-collect)

  (set-face-attribute 'tab-bar nil :height 160)
  (setq tab-bar-close-button-show nil
        tab-bar-new-button-show nil)

  (setq display-line-numbers-type 'relative)
  (global-display-line-numbers-mode 1)

  (setq-default fill-column 80
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

  (custom-set-variables
   '(uniquify-buffer-name-style (quote post-forward) nil (uniquify)))

  ;; Navigation
  (defun scroll-half-page-down ()
    "scroll down half the page"
    (interactive)
    (scroll-down (/ (window-body-height) 2)))

  (defun scroll-half-page-up ()
    "scroll up half the page"
    (interactive)
    (scroll-up (/ (window-body-height) 2)))

  ;; Editing helpers
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
    "Move region (transient-mark-mode active) or current line arg lines down."
    (interactive "*p")
    (move-text-internal arg))

  (defun move-text-up (arg)
    "Move region (transient-mark-mode active) or current line arg lines up."
    (interactive "*p")
    (move-text-internal (- arg)))

  ;; Files
  (defun open-file-at-point ()
    "Open the file name at point. Supports ~/ and ./ paths."
    (interactive)
    (require 'ffap)
    (let ((file (or (ffap-file-at-point) (thing-at-point 'filename t))))
      (if file
          (find-file (expand-file-name file))
        (user-error "No filename at point"))))
  (define-prefix-command 'my/c-z-map)
  (global-set-key (kbd "C-z") my/c-z-map)
  :bind
  (("C-z" . my/c-z-map)
   ("C-v" . scroll-half-page-up)
   ("M-v" . scroll-half-page-down)
   ("C-x _" . maximize-window)
   ("C-c c" . project-compile)
   ("C-c C" . compile)
   ("C-c C-b" . grep)
   ("C-c C-l" . (lambda () (interactive) (duplicate-line) (next-line)))
   ("C-c o" . open-file-at-point)
   ("C-c C-o" . browse-url-at-point)
   ("M-p" . move-text-up)
   ("M-n" . move-text-down)))

;; Grep
(use-package grep
  :ensure nil
  :custom
  (grep-command "grep -nH --color=auto -r -e "))

(use-package rg
  :ensure t
  :config
  (rg-enable-default-bindings)
  (rg-enable-menu))

;; Whitespace
(use-package whitespace
  :ensure nil
  :custom
  (whitespace-style '(face empty tabs lines-tail trailing))
  (whitespace-line-column 120)
  :config
  (global-whitespace-mode t))

;; Completion UI
(use-package icomplete
  :ensure nil
  :init
  (fido-vertical-mode 1)
  :custom
  (completion-styles '(basic flex))
  (completions-format 'one-column)
  (completions-max-height 15)
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

(use-package lambda-line
  :vc (:url "https://github.com/Lambda-Emacs/lambda-line"
            :rev :newest
            :branch "main")
  :after lambda-themes
  :custom
  (lambda-line-icon-time t)
  (lambda-line-clockface-update-fontset "ClockFaceRect")
  (lambda-line-position 'bottom)
  (lambda-line-abbrev t)
  (lambda-line-prefix t)
  (lambda-line-hspace "  ")
  (lambda-line-prefix-padding nil)
  (lambda-line-status-invert nil)
  (lambda-line-vc-symbol " ±")
  (lambda-line-gui-ro-symbol  " ×")
  (lambda-line-gui-mod-symbol " ●")
  (lambda-line-gui-rw-symbol  " ◯")
  (lambda-line-space-top +.50)
  (lambda-line-space-bottom -.50)
  (lambda-line-symbol-position 0)
  :config
  (lambda-line-mode))

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

;; Buffer helpers
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

(keymap-set my/c-z-map "C-z b" (lambda () (interactive) (switch-to-buffer-with-mode 'eaf-mode)))
(keymap-set my/c-z-map "b" (lambda () (interactive) (switch-to-buffer-with-mode 'prog-mode)))
(keymap-set my/c-z-map "e" (lambda () (interactive) (switch-to-buffer-with-mode 'eshell-mode)))

;; Eshell
(use-package eshell
  :ensure nil
  :hook (eshell-mode . (lambda () (display-line-numbers-mode -1)))
  :custom
  (eshell-scroll-to-bottom-on-input 'all)
  :bind (("C-c e" . eshell))
  :config
  (setq eshell-cmpl-cycle-completions nil
        eshell-cmpl-ignore-case t
        eshell-cmpl-autolist t
        eshell-cmpl-expand-tildes t
        eshell-cmpl-replace-by-expanded-string t)
  (setq eshell-history-size 5000
        eshell-scroll-to-bottom-on-input t
        eshell-scroll-show-maximum-output t
        eshell-hist-ignoredups t
        eshell-prefer-lisp-functions nil)

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

(use-package eat
  :ensure t
  :commands (eat eat-eshell-mode)
  :hook (eshell-mode . eat-eshell-mode))

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

(use-package lsp-bridge
  :vc (:url "https://github.com/manateelazycat/lsp-bridge"
            :rev :newest
            :branch "master")
  :commands (lsp-bridge-mode)
  :hook ((python-mode
          python-ts-mode
          js-ts-mode
          tsx-ts-mode
          typescript-ts-mode
          javascript-mode
          json-mode
          css-mode
          html-mode
          web-mode
          c-mode
          cpp-mode) . lsp-bridge-mode)
  :init
  (setq acm-enable-codeium t)
  ;; (setq acm-enable-icon nil
  ;;       acm-enable-capf t
  ;;       acm-enable-search-file-words t
  ;;       acm-enable-yas t
  ;;       acm-enable-doc t
  ;;       acm-enable-doc-markdown-render t
  ;;       acm-doc-delay 0.2
  ;;       acm-candidate-match-function 'regexp-quote
  ;;       acm-enable-search-words t
  ;;       acm-backend-lsp-enable-auto-import t)

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

  ;; (setq lsp-bridge-enable-hover-diagnostic nil
  ;;       lsp-bridge-enable-signature-help t
  ;;       lsp-bridge-enable-auto-format-code nil)
  :config
  (add-to-list 'lsp-bridge-multi-lang-server-extension-list '("py" . "ty_ruff"))
  (dolist (ext '("ts" "tsx" "js" "jsx"))
    (add-to-list 'lsp-bridge-multi-lang-server-extension-list (cons ext "tsls_oxlint_oxfmt")))

  ;; (defun my/lsp-bridge-xref-backend () 'lsp-bridge)
  ;; (add-hook 'lsp-bridge-mode-hook (lambda () (add-hook 'xref-backend-functions #'my/lsp-bridge-xref-backend nil t)))
  (global-lsp-bridge-mode)
  ;; (add-hook 'lsp-bridge-mode-hook
  ;;           (lambda ()
  ;;             (when (fboundp 'corfu-mode)
  ;;               (corfu-mode -1))))
  :bind (:map lsp-bridge-mode-map
              ("M-."   . lsp-bridge-find-def)
              ("M-,"   . lsp-bridge-find-def-return)
              ("M-?"   . lsp-bridge-find-references)
              ("C-h ." . lsp-bridge-popup-documentation)
              ("C-c l s" . lsp-bridge-workspace-list-symbols)
              ("C-c l r" . lsp-bridge-rename)
              ("C-c l a" . lsp-bridge-code-action)
              ("C-c l d" . lsp-bridge-diagnostics-list)
              ("C-c l f" . lsp-bridge-format-buffer)))

;; (use-package corfu
;;   :ensure t
;;   :init
;;   (global-corfu-mode)
;;   :custom
;;   (corfu-auto t)
;;   (corfu-cycle t)
;;   (corfu-echo-delay 0.2)
;;   (corfu-popupinfo-delay 0.2))

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
  (require 'sublimity-scroll)
  (require 'sublimity-attractive)
  :bind (("C-z z" . sublimity-mode)))

;; Media
(use-package emms
  :ensure t)

(use-package mpvi
  :ensure t
  :custom
  (mpvi-mpv-ontop-p t)
  :bind (("C-z C-v" . mpvi-play)
         ("C-z v s" . mpvi-speed)))

;; EAF
;; (use-package eaf
;;   :vc (:url "https://github.com/emacs-eaf/emacs-application-framework"
;;             :rev :newest
;;             :branch "master")
;;   :demand t
;;   :custom
;;   (eaf-browser-translate-language "en")
;;   (eaf-browser-continue-where-left-off t)
;;   (eaf-browser-enable-adblocker t)
;;   (eaf-browser-default-search-engine "duckduckgo")
;;   (eaf-browse-blank-page-url "https://duckduckgo.com")
;;   (eaf-pdf-dark-mode "force")
;;   (browse-url-browser-function #'eaf-open-browser)
;;   :config
;;   (require 'eaf-browser)
;;   (require 'eaf-org-previewer)
;;   (require 'eaf-image-viewer)
;;   (require 'eaf-markdown-previewer)
;;   (require 'eaf-pdf-viewer)

;;   (defalias 'browse-web #'eaf-open-browser)

;;   (defun adviser-find-file (orig-fn file &rest args)
;;     (let ((fn (if (commandp 'eaf-open) #'eaf-open orig-fn)))
;;       (pcase (file-name-extension file)
;;         ((or "pdf" "epub") (funcall fn file))
;;         (_ (apply orig-fn file args)))))

;;   (advice-add #'find-file :around #'adviser-find-file)

;;   (keymap-global-set "C-c C-o" #'eaf-open-url-at-point)
;;   (keymap-set my/c-z-map "C-z f" #'eaf-open)
;;   (keymap-set my/c-z-map "C-z u" #'eaf-open-browser)
;;   (keymap-set my/c-z-map "C-z h" #'eaf-open-browser-with-history)
;;   (keymap-set my/c-z-map "C-z s" #'eaf-search-it)
;;   (keymap-set my/c-z-map "C-z p" #'eaf-open-pdf-from-history))
