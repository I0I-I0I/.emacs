;; git clone https://git.savannah.gnu.org/git/emacs.git
;; cd emacs
;; git checkout emacs-30.2
;; ./autogen.sh
;; ./configure --prefix=/usr/local --with-x --with-x-toolkit=gtk3 --without-pgtk --with-native-compilation --with-tree-sitter --with-modules --with-cairo --with-imagemagick --with-xft
;; make -j$(nproc)
;; sudo make install

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (eq system-type 'windows-nt)
  (setenv "HOME" (getenv "USERPROFILE"))
  (setq default-directory (file-name-as-directory (getenv "HOME"))))

(when (display-graphic-p)
  (let ((font (if (eq system-type 'gnu/linux)
                  "FantasqueSansM Nerd Font 20"
                "FantasqueSansM Nerd Font 16")))
    (when (find-font (font-spec :name font))
      (set-frame-font font nil t))))

(declare-function cape-keyword "cape")
(declare-function ffap-file-at-point "ffap")

(push '(fullscreen . maximized) default-frame-alist)

(defvar my/gc-cons-threshold-orig gc-cons-threshold)
(defvar my/gc-cons-percentage-orig gc-cons-percentage)

;; Core
(use-package emacs
  :ensure nil
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
   '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))))

;; Grep
(use-package grep
  :ensure nil
  :custom
  (grep-command "grep -nH --color=auto -r -e "))

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
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
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

;; Keymaps
(global-unset-key (kbd "C-z"))
(defvar my/c-z-map (let ((m (make-sparse-keymap))) m))
(keymap-global-set "C-z" my/c-z-map)

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

;; Packages
(use-package package
  :ensure nil
  :init
  (setq package-archives
        '(("gnu"   . "https://elpa.gnu.org/packages/")
          ("nongnu" . "https://elpa.nongnu.org/nongnu/")
          ("melpa" . "https://melpa.org/packages/")))
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents)))

(use-package use-package
  :ensure t
  :config
  (setq use-package-verbose nil)
  :custom
  (use-package-always-ensure t))

(use-package which-key
  :ensure t
  :config
  (which-key-mode 1))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

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
    (when (display-graphic-p)
      (set-frame-parameter nil 'alpha-background 70)
      (add-to-list 'default-frame-alist '(alpha-background . 70))
      (add-to-list 'default-frame-alist '(background-color . "#000000"))
      (set-face-background 'default "#000000"))))

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
  :bind (("C-z g g" . magit-status)))

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

;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))

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
  (global-lsp-bridge-mode)

  (setq acm-enable-quick-access t          ;; allow quick select by 1..0
        acm-enable-icon t                  ;; icons (nice-to-have)
        acm-enable-doc t                   ;; popup docs
        acm-enable-doc-markdown-render t   ;; better docs rendering (if available)
        acm-doc-delay 0.2                  ;; like corfu-popupinfo-delay
        acm-candidate-max-number 30        ;; similar to corfu candidate count
        acm-candidate-match-function 'regexp-quote ;; conservative matching
        acm-enable-search-words t          ;; improves filtering in many stacks
        acm-backend-lsp-enable-auto-import t)

  (setq lsp-bridge-enable-completion-in-minibuffer t)

  (dolist (dir '("~/.bun/bin" "~/.local/share/zana/bin" "~/.local/bin"))
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
  (add-to-list 'lsp-bridge-multi-lang-server-extension-list '("py" . "ty_ruff"))
  (dolist (ext '("ts" "tsx" "js" "jsx"))
    (add-to-list 'lsp-bridge-multi-lang-server-extension-list (cons ext "tsls_oxlint_oxfmt")))

  (defun my/lsp-bridge-xref-backend () 'lsp-bridge)
  (add-hook 'lsp-bridge-mode-hook (lambda () (add-hook 'xref-backend-functions #'my/lsp-bridge-xref-backend nil t)))
  (add-hook 'lsp-bridge-mode-hook (lambda () (corfu-mode -1)))

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

(use-package corfu
  :ensure t
  ;; :hook ((eshell-mode . corfu-mode))
  :init
  (global-corfu-mode)
  :custom
  (setq corfu-auto-trigger ".")
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-echo-delay 0.2)
  (corfu-popupinfo-delay 0.2))

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
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

;; Video
(use-package mpvi
  :ensure t
  :custom
  (mpvi-mpv-ontop-p t)
  :bind (("C-z C-v" . mpvi-play)
         ("C-z v s" . mpvi-speed)))

;; EAF
(use-package eaf
  :vc (:url "https://github.com/emacs-eaf/emacs-application-framework"
            :rev :newest
            :branch "master")
  :demand t
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
  (require 'eaf-image-viewer)
  (require 'eaf-markdown-previewer)
  (require 'eaf-pdf-viewer)

  (defalias 'browse-web #'eaf-open-browser)

  (defun adviser-find-file (orig-fn file &rest args)
    (let ((fn (if (commandp 'eaf-open) #'eaf-open orig-fn)))
      (pcase (file-name-extension file)
        ((or "pdf" "epub") (funcall fn file))
        (_ (apply orig-fn file args)))))

  (advice-add #'find-file :around #'adviser-find-file)

  (keymap-global-set "C-c C-o" #'eaf-open-url-at-point)
  (keymap-set my/c-z-map "C-z f" #'eaf-open)
  (keymap-set my/c-z-map "C-z u" #'eaf-open-browser)
  (keymap-set my/c-z-map "C-z h" #'eaf-open-browser-with-history)
  (keymap-set my/c-z-map "C-z s" #'eaf-search-it)
  (keymap-set my/c-z-map "C-z p" #'eaf-open-pdf-from-history))
