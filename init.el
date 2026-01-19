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

(with-eval-after-load 'lsp-mode
  (setq lsp-idle-delay 0.2
        lsp-enable-symbol-highlighting nil
        lsp-headerline-breadcrumb-enable nil
        lsp-file-watch-threshold 2000
        lsp-enable-file-watchers nil))

(with-eval-after-load 'tramp
  (setq tramp-ssh-controlmaster-options
        "-o ControlMaster=auto -o ControlPath='~/.ssh/tramp-%%r@%%h:%%p' -o ControlPersist=60s")
  (setq tramp-auto-save-directory (expand-file-name "tramp-autosave" user-emacs-directory)))

(run-with-idle-timer 10 t #'garbage-collect)

;; C-x C-b			list-buffers
;; C-x C-c			save-buffers-kill-terminal
;; C-x C-d			list-directory
;; C-x TAB			indent-rigidly
;; C-x C-j			dired-jump
;; C-x C-l			downcase-region
;; C-x C-u			upcase-region
;;
;; C-x b			switch-to-buffer
;; C-x d			dired
;; C-x k			kill-buffer
;; C-x l			count-lines-page
;; C-x o			other-window
;; C-x C-;			comment-line
;; C-x C-<left>	previous-buffer
;; C-x C-<right>	next-buffer
;; C-x <left>		previous-buffer
;; C-x <right>		next-buffer
;;
;; C-x 0			delete-window
;; C-x 1			delete-other-windows
;; C-x 2			split-window-below
;; C-x 3			split-window-right
;;
;; C-x 4 C-f		find-file-other-window
;; C-x 4 C-j		dired-jump-other-window
;; C-x 4 0			kill-buffer-and-window
;; C-x 4 b			switch-to-buffer-other-window
;; C-x 4 d			dired-other-window
;; C-x 4 f			find-file-other-window
;; C-x 4 p			project-other-window-command
;;
;; C-x 5 ...        other frame
;; C-x 5 u			undelete-frame
;;
;; C-x t C-f		find-file-other-tab
;; C-x t RET		tab-switch
;; C-x t 0			tab-close
;; C-x t 1			tab-close-other
;; C-x t 2			tab-new
;; C-x t p			project-other-tab-command
;; C-x t M			tab-move-to
;; C-x t N			tab-new-to
;; C-x t O			tab-previous
;; C-x t o			tab-next
;; C-x t d			dired-other-tab
;; C-x t u			tab-undo
;;
;; C-x v ...       vc
;;
;; M-z						zap-to-char
;; M-s . search
;; M-s o search
;; M-g i symbols
;; M-g n			next-error
;; M-g p			previous-error
;;
;; Find file
;; M-f	Prompt for a file and use find to locate it.
;; M-d	Prompt for a directory and use find to locate it.
;; C-d	Open the specified directory in Dired mode.
;; C-j	Use the current input string verbatim.
;; C-s	Put the first element at the end of the list.
;; C-r	Put the last element at the start of the list.
;; M-g M-g			goto-line
;; M-^
;;
;; C-t       transpose-char
;; M-t       transpose-word
;; C-j       line without indent
;; C-q       like C-v in 'vim'
;; C-M-SPC   makr sexp
;; C-M-\     indent region ('=' in 'vim')
;; C-M-d     down list
;; C-M-a     start of function
;; C-M-e     end of function
;; C-M-f     forvard sexp
;; C-M-b     backword sexp
;; C-M-h     mark defun
;; C-M-k     kill sexp
;; C-M-o     split line
;; C-M-n/p   forward/backword list
;; C-M-s/r   isearch farword/backword regexp
;; C-M-backspace backword kill sepx
;; M-@       mark word
;; C-S-backspace  kill line
;; M-%       query replace
;; M-(       insert parentheses
;; M-)       move past close par
;; M-c       capitalize
;; M-l       lowercase
;; M-u       uppercase
;; M-z       zap-to-char

;; M-?       references
;; M-.       definitions
;; M-,       go back

(require 'server)
(unless (server-running-p)
  (server-start))

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

(which-key-mode 1)

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

(when (daemonp)
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (select-frame frame)
              (when (display-graphic-p frame)
                ;; Load GUI settings
                (load-theme 'almost-mono-black t)
                (tool-bar-mode -1)
                (scroll-bar-mode -1)))))

(setq server-use-tcp t
      server-host "127.0.0.1"
      server-port 12345)

(use-package undo-tree
  :init
  (setq undo-tree-visualizer-timestamps t
        undo-tree-history-directory-alist `(("." . ,(expand-file-name "undo-history" user-emacs-directory)))
        undo-tree-auto-save-history t
        undo-limit 10000000
        undo-strong-limit 10000000)
  :config
  (global-undo-tree-mode))

(use-package flyspell
  :ensure nil
  :hook ((text-mode . flyspell-mode)
         (prog-mode . (lambda () (flyspell-mode -1))))
  :config
  (setq ispell-program-name "aspell"))

(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-C-i-jump t
        evil-respect-visual-line-mode t)
  :config
  (evil-mode 1)
  (evil-ex-define-cmd "W" 'evil-write)
  (evil-ex-define-cmd "Wa" (lambda (&rest _args) (interactive) (evil-write-all))))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-numbers
  :vc (:url "https://github.com/cofi/evil-numbers"
            :rev :newest
            :branch "master")
  :commands (evil-numbers/inc-at-pt evil-numbers/dec-at-pt))

(use-package evil-mc
  :ensure t
  :config
  (global-evil-mc-mode  1)
  (evil-define-key 'visual evil-mc-key-map
                   "A" #'evil-mc-make-cursor-in-visual-selection-end
                   "I" #'evil-mc-make-cursor-in-visual-selection-beg)
  (evil-define-key '(normal visual) 'global (kbd "M-I") 'evil-mc-undo-all-cursors)
  (evil-define-key '(normal visual) 'global (kbd "M-n") 'evil-mc-make-and-goto-next-match)
  (evil-define-key '(normal visual) 'global (kbd "M-p") 'evil-mc-make-and-goto-prev-match)
  (evil-define-key '(normal visual) 'global (kbd "M-N") 'evil-mc-skip-and-goto-next-match)
  (evil-define-key '(normal visual) 'global (kbd "M-P") 'evil-mc-skip-and-goto-prev-match)
  (evil-define-key '(normal visual) 'global (kbd "M-j") 'evil-mc-make-cursor-move-next-line)
  (evil-define-key '(normal visual) 'global (kbd "M-k") 'evil-mc-make-cursor-move-prev-line)
  (evil-define-key '(normal visual) 'global (kbd "C-M-j") 'evil-mc-make-and-goto-next-cursor)
  (evil-define-key '(normal visual) 'global (kbd "C-M-k") 'evil-mc-make-and-goto-prev-cursor)
  (evil-define-key '(normal visual) 'global (kbd "M-u") 'evil-mc-undo-last-added-cursor)
  (evil-define-key '(normal visual) 'global (kbd "M-A") 'evil-mc-make-all-cursors)
  (evil-define-key '(normal visual) 'global (kbd "M-q") 'evil-mc-pause-cursors)
  (evil-define-key '(normal visual) 'global (kbd "M-r") 'evil-mc-resume-cursors)
  (evil-define-key '(normal visual) 'global (kbd "M-a") 'evil-mc-make-cursor-here))

(use-package general
  :after evil
  :config
  (general-define-key
   :keymaps 'global
   "C-c c" #'project-compile
   "C-c C" #'compile
   "C-c r" #'list-registers)

  (general-define-key
   :states '(normal)
   :prefix "C-w"
   "t" (lambda () (interactive) (tab-new) (vterm) (recenter)))

  (general-define-key
   :states '(normal)
   "C-f" 'project-find-file
   "C-b" 'grep
   "C-l" (lambda () (interactive) (duplicate-line) (next-line))
   "n" (lambda () (interactive) (evil-search-next) (recenter))
   "N" (lambda () (interactive) (evil-search-previous) (recenter)))

  (general-define-key
   :states '(normal visual)
   "+" 'evil-numbers/inc-at-pt
   "-" 'evil-numbers/dec-at-pt)

  (general-define-key
   :states '(normal visual insert)
   "C-e" (lambda () (interactive) (dotimes (_ 3) (scroll-up-line)))
   "C-y" (lambda () (interactive) (dotimes (_ 3) (scroll-down-line))))

  (general-define-key
   :states '(normal)
   "M-c" (lambda ()
           (interactive)
           (when buffer-file-name
             (kill-new (expand-file-name buffer-file-name))
             (message "Copied: %s" (expand-file-name buffer-file-name))))))

(use-package vterm
  :ensure t
  :commands vterm)

(use-package eat
  :ensure t
  :config
  (setf eshell-visual-commands nil
        eat-term-name "xterm-256color")
  :hook ((eshell-mode . eat-eshell-mode)
         (eshell-mode . eat-eshell-visual-command-mode)
         (eat-mode . (lambda () (visual-line-mode -1)))))

(use-package eshell
  :ensure t
  :config
  (defun eshell-git-info ()
	"Return a string with git info."
	(when (eq (call-process "git" nil nil nil "rev-parse" "--is-inside-work-tree") 0)
	  (let* ((branch-raw (shell-command-to-string "git rev-parse --abbrev-ref HEAD"))
			 (branch (if (or (string-match-p "^fatal" branch-raw)
							 (string-match-p "^error" branch-raw))
						 "Unknown"
					   (string-trim branch-raw)))
			 (dirty (not
					 (string= "" (string-trim (shell-command-to-string "git status --porcelain")))))
			 (dirty-info (if dirty " ✎" " ✔")))
		(concat (propertize "⎇ " 'face 'modus-themes-fg-green-warmer)
				(propertize branch 'face 'modus-themes-fg-magenta-warmer)
				(propertize dirty-info 'face
							(if dirty 'modus-themes-fg-red 'modus-themes-fg-green))))))

  (defun eshell-prompt-multiline ()
	"Eshell Multiline Git prompt."
	(let ((separator (propertize " | " 'face 'font-lock-comment-face))
		  (hr (propertize (concat "\n" (make-string (/ (window-total-width) 2) ?─) "\n") 'face 'font-lock-comment-face))
		  (dir (propertize (format "%s" (abbreviate-file-name (eshell/pwd))) 'face 'modus-themes-fg-yellow-warmer))
		  (git-info (eshell-git-info))
		  (time (propertize (format-time-string "%H:%M:%S") 'face 'font-lock-comment-face))
		  (sign (if (= (user-uid) 0)
					(propertize "\n#" 'face 'modus-themes-fg-blue-intense)
				  (propertize "\nλ" 'face 'modus-themes-fg-red-warmer))))
	  (concat hr dir separator git-info separator time sign " ")))

  (setf eshell-prompt-function 'eshell-prompt-multiline
		eshell-highlight-prompt nil)

  (defun thanos/eshell-clear ()
    "Interactive call for clear-scrollback."
    (interactive)
    (eshell/clear-scrollback))

  (defun thanos/eshell-preview-insert ()
	(interactive)
	(completion-preview-insert)
	(delete-char -1))

  :bind (("C-c t" . eshell)
		 :map eshell-mode-map
		 ("C-l" . 'thanos/eshell-clear))
  :hook ((eshell-mode . (lambda ()
                          (thanos/set-eshell-aliases thanos/aliases)
                          (display-line-numbers-mode -1)
                          (eshell-cmpl-mode -1)))))

;; (use-package corfu
;;   :ensure t
;;   :init
;;   (setq corfu-auto nil            ;; set to t for automatic popup
;;         corfu-cycle t             ;; cycle candidates
;;         corfu-echo-delay 0.25
;;         corfu-popupinfo-delay 0.25)
;;   (global-corfu-mode)
;;   (corfu-popupinfo-mode)
;;   :bind
;;   (:map corfu-map
;;         ("RET" . corfu-insert)
;;         ("TAB" . corfu-insert))
;;   :custom
;;   (corfu-min-width 20)
;;   (corfu-max-width 80)
;;   (corfu-separator ?\s))

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

;; (use-package lsp-mode
;;   :ensure t
;;   :after evil
;;   :commands (lsp lsp-deferred)
;;   :hook ((python-mode . lsp-deferred)
;;          (typescript-mode . lsp-deferred)
;;          (c-mode . lsp-deferred)
;;          (c++-mode . lsp-deferred)
;;          (css-mode . lsp-deferred)
;;          (html-mode . lsp-deferred)
;;          (js-mode . lsp-deferred)
;;          (lsp-mode . (lambda ()
;;                        (setq-local completion-at-point-functions
;;                                    (cons #'lsp-completion-at-point
;;                                          (remq #'lsp-completion-at-point completion-at-point-functions))))))
;;   :init
;;   (setq lsp-headerline-breadcrumb-enable nil)
;;   (setq lsp-keymap-prefix "M-l")
;;   (setq lsp-semantic-tokens-enable nil)
;;   (setq lsp-diagnostics-provider :flycheck)
;;   :custom
;;   (lsp-completion-enable t)
;;   (lsp-enable-snippet t)
;;   (lsp-completion-provider :capf)
;;   :config
;;   (lsp-enable-which-key-integration t))

;; (with-eval-after-load 'lsp-mode
;;   (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

;; (use-package lsp-ui
;;   :after lsp-mode
;;   :commands lsp-ui-mode
;;   :hook (lsp-mode . lsp-ui-mode)
;;   :custom
;;   (lsp-ui-sideline-enable nil)
;;   (lsp-ui-doc-enable t)
;;   (lsp-ui-doc-position 'at-point))

;; (with-eval-after-load 'evil
;;   (evil-define-key '(normal visual) 'global (kbd "K") #'lsp-ui-doc-glance)
;;   (evil-define-key '(normal visual) 'global (kbd "g r a") #'lsp-execute-code-action)
;;   (evil-define-key '(normal visual) 'global (kbd "g r i") #'lsp-find-implementation)
;;   (evil-define-key '(normal visual) 'global (kbd "g r n") #'lsp-rename)
;;   (evil-define-key '(normal visual) 'global (kbd "g r r") #'lsp-find-references)
;;   (evil-define-key '(normal visual) 'global (kbd "g r t") #'lsp-find-type-definition)
;;   (evil-define-key '(normal visual) 'global (kbd "g O") #'lsp-ui-imenu)
;;   (define-key evil-insert-state-map (kbd "C-s") #'lsp-signature-activate))

;; (use-package eglot
;;   :ensure t
;;   :hook ((python-mode python-ts-mode) . eglot-ensure)
;;   :config
;;   (add-to-list 'eglot-server-programs
;;                `((python-ts-mode python-mode) . ("pyrefly" "lsp")))

;;   (with-eval-after-load 'evil
;;     (evil-define-key '(normal visual) 'global (kbd "g r a") #'eglot-code-actions)
;;     (evil-define-key '(normal visual) 'global (kbd "g r i") #'eglot-find-implementation)
;;     (evil-define-key '(normal visual) 'global (kbd "g r n") #'eglot-rename)
;;     (evil-define-key '(normal visual) 'global (kbd "g r r") #'xref-find-references)
;;     (evil-define-key '(normal visual) 'global (kbd "g r t") #'eglot-find-typeDefinition)
;;     (evil-define-key '(normal visual) 'global (kbd "g O") #'lsp-ui-imenu)
;;     (define-key evil-insert-state-map (kbd "C-s") #'lsp-signature-activate)))

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
  :ensure t
  :config
  (global-lsp-bridge-mode)

  (setq acm-enable-codeium t)
  (setq acm-enable-tabnine t)
  (setq acm-enable-capf t)
  (setq acm-enable-doc t)

  (setq lsp-bridge-codeium-max-num-results 5)
  (setq lsp-bridge-codeium-enable-auto-complete t)

  (define-key acm-mode-map (kbd "M-<return>") 'acm-complete)

  (setq lsp-bridge-enable-candidate-doc t)
  (setq lsp-bridge-enable-signature-help t)
  (setq lsp-bridge-enable-completion-in-string t)
  (setq lsp-bridge-python-multi-lsp-server "ty_ruff")

  (with-eval-after-load 'evil
    (evil-define-key '(normal visual) 'global (kbd "K") #'lsp-bridge-popup-documentation)
    (evil-define-key '(normal visual) 'global (kbd "g r a") #'lsp-bridge-code-action)
    (evil-define-key '(normal visual) 'global (kbd "g r i") #'lsp-bridge-find-impl)
    (evil-define-key '(normal visual) 'global (kbd "g r n") #'lsp-bridge-rename)
    (evil-define-key '(normal visual) 'global (kbd "g r r") #'lsp-bridge-find-references)
    (evil-define-key '(normal visual) 'global (kbd "g r t") #'lsp-bridge-find-def)
    (evil-define-key '(normal visual) 'global (kbd "g O") #'acm-show-menu)
    (define-key evil-insert-state-map (kbd "C-s") #'lsp-bridge-signature-help)))

(use-package flycheck
  :hook (lsp-mode . flycheck-mode)
  :init (global-flycheck-mode))

(use-package apheleia
  :config
  (apheleia-global-mode +1))

(set-face-attribute 'flycheck-error nil :underline nil)
(set-face-attribute 'flycheck-warning nil :underline nil)

(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev :newest
            :branch "main"))

;; (add-hook 'prog-mode-hook 'copilot-mode)

(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("cbd85ab34afb47003fa7f814a462c24affb1de81ebf172b78cb4e65186ba59d2" default))
 '(package-selected-packages '(yasnippet))
 '(package-vc-selected-packages
   '((yasnippet :url "https://github.com/joaotavora/yasnippet" :branch "master")
     (lsp-bridge :url "https://github.com/manateelazycat/lsp-bridge" :branch
                 "master"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(line-number ((t (:foreground "#3a3f5a" :background nil))))
 '(line-number-current-line ((t (:foreground "#c5c9c5" :background nil :weight bold))))
 '(tab-bar ((t (:background "#000000" :foreground "#c0caf5" :box nil))))
 '(tab-bar-tab ((t (:background "#222222" :foreground "#c5c9c5" :weight bold))))
 '(tab-bar-tab-inactive ((t (:background nil :foreground "#565f89" :box nil)))))
