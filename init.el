;;; init.el --- Emacs init -*- lexical-binding: t; no-byte-compile: t; -*-

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
    :hook ((text-mode . (lambda () (display-line-numbers-mode -1)))
           (special-mode . (lambda () (display-line-numbers-mode -1))))
    :config
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
        `((".*" ,(expand-file-name "auto-save-list/" user-emacs-directory) t))
        image-auto-resize 'fit-window)

  (when (boundp 'native-comp-async-report-warnings-errors)
    (setq native-comp-async-report-warnings-errors nil))
  (when (boundp 'native-comp-deferred-compilation)
    (setq native-comp-deferred-compilation t))

  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq gc-cons-threshold (* 16 1024 1024)
                    gc-cons-percentage 0.1)))

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
        scroll-margin 0
        scroll-step 1
        scroll-conservatively 10000
        auto-window-vscroll nil
        fast-but-imprecise-scrolling t
        scroll-preserve-screen-position t
        mouse-wheel-scroll-amount '(1 ((shift) . 1))
        mouse-wheel-progressive-speed nil)

  (when (fboundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode 1))

  (put 'narrow-to-region 'disabled nil)
  (put 'narrow-to-page 'disabled nil)

  (setq isearch-lazy-count t)

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

  ;; Frame/workflow helpers.
  (defun my/project-or-cwd-name (&optional directory)
    "Return the `project.el' name for DIRECTORY, falling back to its cwd name."
    (let* ((directory (file-name-as-directory
                       (expand-file-name (or directory default-directory))))
           (default-directory directory)
           (project (and (fboundp 'project-current) (project-current nil)))
           (name (or (when project
                       (if (fboundp 'project-name)
                           (project-name project)
                         (file-name-nondirectory
                          (directory-file-name (project-root project)))))
                     (file-name-nondirectory
                      (directory-file-name directory)))))
      (if (string= name "") directory name)))

  (defun my/update-frame-name (&optional frame)
    "Name FRAME after its selected buffer's project, falling back to cwd."
    (let* ((frame (or frame (selected-frame)))
           (window (and (frame-live-p frame) (frame-selected-window frame))))
      ;; During desktop/frameset restoration Emacs can temporarily have live
      ;; frames without a valid selected window.  Do not call `window-buffer'
      ;; on nil there.
      (when (window-live-p window)
        (with-current-buffer (window-buffer window)
          (set-frame-parameter
           frame 'name
           (my/project-or-cwd-name
            (or (frame-parameter frame 'default-directory)
                default-directory)))))))

  (defun my/update-all-frame-names ()
    "Update names for all live frames."
    (dolist (frame (frame-list))
      (my/update-frame-name frame)))

  (defun my/update-frame-name-for-window (window)
    "Update WINDOW's frame name from WINDOW's buffer."
    (when (window-live-p window)
      (my/update-frame-name (window-frame window))))

  (defun my/update-frame-names-after-desktop-read ()
    "Update frame names after `desktop-read' restores frames and buffers."
    (run-at-time 0 nil #'my/update-all-frame-names))

  (add-hook 'window-buffer-change-functions #'my/update-frame-name-for-window)
  (add-hook 'after-change-major-mode-hook #'my/update-frame-name)
  (add-hook 'desktop-after-read-hook #'my/update-frame-names-after-desktop-read)
  (my/update-frame-name)

  (defun my/new-workflow-frame ()
    "Create a new workflow frame using current frame defaults."
    (interactive)
    (select-frame-set-input-focus (make-frame))
    (my/update-frame-name))

  (defun my/new-project-frame--tab-name (directory)
    "Return a tab name for project DIRECTORY."
    (let* ((directory (file-name-as-directory (expand-file-name directory)))
           (existing-tab-names (mapcar (lambda (tab) (alist-get 'name tab))
                                       (tab-bar-tabs))))
      (or (when (boundp 'tabspaces-project-tab-map)
            (cdr (assoc directory tabspaces-project-tab-map)))
          (when (fboundp 'tabspaces-generate-descriptive-tab-name)
            (tabspaces-generate-descriptive-tab-name directory existing-tab-names))
          (file-name-nondirectory (directory-file-name directory)))))

  (defun my/new-project-frame (directory)
    "Create a new frame rooted at project DIRECTORY."
    (interactive
     (list (if (fboundp 'project-prompt-project-dir)
               (project-prompt-project-dir)
             (read-directory-name "Project directory: "))))
    (let* ((project-directory (file-name-as-directory (expand-file-name directory)))
           (default-directory project-directory))
      (when (fboundp 'project--ensure-read-project-list)
        (project--ensure-read-project-list))
      (select-frame-set-input-focus (make-frame))
      ;; A newly-created frame can inherit the selected frame's tab-bar state.
      ;; Reset it to a single scratch tab, then turn that tab into the project
      ;; workspace directly.  Avoid `tabspaces-open-or-create-project-and-workspace'
      ;; here: it is designed for the current frame and may prompt via
      ;; `project-switch-project' instead of just creating the new frame.
      (when (fboundp 'tab-bar-tabs-set)
        (switch-to-buffer (get-buffer-create "*scratch*"))
        (tab-bar-tabs-set nil)
        (set-frame-parameter nil 'current-tab nil))
      (set-frame-name (my/project-or-cwd-name project-directory))
      (let ((tab-name (my/new-project-frame--tab-name project-directory)))
        (when (fboundp 'tab-bar-rename-tab)
          (tab-bar-rename-tab tab-name))
        (when (boundp 'tabspaces-project-tab-map)
          (setq tabspaces-project-tab-map
                (cons (cons project-directory tab-name)
                      (assoc-delete-all project-directory tabspaces-project-tab-map)))))
      (let ((default-directory project-directory))
        (when-let* ((project (and (fboundp 'project--find-in-directory)
                                  (project--find-in-directory project-directory))))
          (project-remember-project project))
        (if (and (fboundp 'project-dired) (project-current nil))
            (project-dired)
          (dired project-directory)))
      (when (fboundp 'tabspaces-reset-buffer-list)
        (tabspaces-reset-buffer-list))))

  (defun my/change-frame-default-directory (directory)
    "Change the default directory associated with the current frame to DIRECTORY."
    (interactive
     (list (read-directory-name
            "Frame default directory: "
            (or (frame-parameter nil 'default-directory) default-directory))))
    (let ((directory (file-name-as-directory (expand-file-name directory))))
      (set-frame-parameter nil 'default-directory directory)
      ;; `default-directory' is buffer-local; update the selected buffer so
      ;; commands run from it immediately use the new frame directory.
      (setq default-directory directory)
      (my/update-frame-name)
      (message "Frame default directory: %s" directory)))

  (defun my/rename-frame (name)
    "Rename the current frame to NAME."
    (interactive
     (list (read-string "Frame name: " (frame-parameter nil 'name))))
    (set-frame-name name))

  (defun my/switch-frame (frame)
    "Select a live frame by name."
    (interactive
     (let* ((frames (frame-list))
            (choices (mapcar (lambda (frame)
                               (cons (format "%s%s"
                                             (or (frame-parameter frame 'name) "<unnamed>")
                                             (if (eq frame (selected-frame)) "  (current)" ""))
                                     frame))
                             frames)))
       (list (cdr (assoc (completing-read "Switch to frame: " choices nil t)
                         choices)))))
    (select-frame-set-input-focus frame))

  (defun my/frame-buffer-list (&optional frame)
    "Return live buffers associated with all tabs in FRAME."
    (let* ((frame (or frame (selected-frame)))
           (tabs (frame-parameter frame 'tabs))
           buffers)
      (if (and (fboundp 'tabspaces--buffer-list) tabs)
          (let ((index 0))
            (dolist (_tab tabs)
              (setq buffers (append (tabspaces--buffer-list frame index) buffers))
              (setq index (1+ index))))
        (setq buffers (frame-parameter frame 'buffer-list)))
      (delete-dups (seq-filter #'buffer-live-p buffers))))

  (defun my/buffer-used-in-other-frame-p (buffer frame)
    "Return non-nil when BUFFER belongs to any live frame other than FRAME."
    (seq-some (lambda (other-frame)
                (and (frame-live-p other-frame)
                     (not (eq other-frame frame))
                     (memq buffer (my/frame-buffer-list other-frame))))
              (frame-list)))

  (defun my/frame-local-buffer-p (buffer frame)
    "Return non-nil when BUFFER should be removed when FRAME closes."
    (and (buffer-live-p buffer)
         (not (member (buffer-name buffer)
                      (and (boundp 'tabspaces-include-buffers)
                           tabspaces-include-buffers)))
         (not (my/buffer-used-in-other-frame-p buffer frame))))

  (defun my/kill-frame-local-buffers (&optional frame)
    "Kill buffers that are local to FRAME's tabspaces."
    (let ((frame (or frame (selected-frame))))
      (dolist (buffer (my/frame-buffer-list frame))
        (when (my/frame-local-buffer-p buffer frame)
          (kill-buffer buffer)))))

  (defconst my/auto-close-tab-name-regexp
    "\\`\\(?:eshell\\|magit\\|pi\\|pi-agent\\|vterm\\)\\(?:\\'\\|: \\)"
    "Regexp matching dedicated utility tabs to close before frame/desktop save/Emacs exit.")

  (defun my/auto-close-tab-p (tab)
    "Return non-nil when TAB is a dedicated utility tab."
    (when-let* ((name (alist-get 'name tab)))
      (string-match-p my/auto-close-tab-name-regexp name)))

  (defun my/close-utility-tabs-in-frame (&optional frame)
    "Close dedicated pi-agent, Eshell, Vterm and Magit tabs in FRAME.
If a utility tab is the only tab left, reset it to a scratch/main tab so it is
not persisted as a utility workspace when Emacs exits."
    (let ((frame (or frame (selected-frame))))
      (when (and (frame-live-p frame) (fboundp 'tab-bar-tabs))
        (with-selected-frame frame
          (dolist (name (delete-dups
                         (delq nil
                               (mapcar (lambda (tab)
                                         (when (my/auto-close-tab-p tab)
                                           (alist-get 'name tab)))
                                       (tab-bar-tabs)))))
            (if (> (length (tab-bar-tabs)) 1)
                (ignore-errors (tab-bar-close-tab-by-name name))
              (when (and (my/auto-close-tab-p (car (tab-bar-tabs)))
                         (get-buffer "*scratch*"))
                (switch-to-buffer (get-buffer-create "*scratch*"))
                (when (fboundp 'tab-bar-rename-tab)
                  (tab-bar-rename-tab "main"))
                (set-frame-parameter nil 'buffer-list (list (current-buffer))))))))))

  (defun my/close-utility-tabs-in-all-frames ()
    "Close dedicated utility tabs in all live frames."
    (dolist (frame (frame-list))
      (my/close-utility-tabs-in-frame frame)))

  (defun my/close-utility-tabs-before-desktop-save ()
    "Close transient utility tabs before `desktop' serializes frames/tabs."
    (my/close-utility-tabs-in-all-frames))

  (defun my/close-utility-tabs-before-delete-frame (frame)
    "Close dedicated utility tabs in FRAME before it is deleted."
    (my/close-utility-tabs-in-frame frame))

  (unless noninteractive
    (add-hook 'delete-frame-functions #'my/close-utility-tabs-before-delete-frame)
    (add-hook 'kill-emacs-hook #'my/close-utility-tabs-in-all-frames -100))

  (defun my/delete-frame-or-emacs (&optional kill-local-buffers)
    "Delete current frame, optionally killing frame-local buffers.
On the last frame, quit Emacs.  With prefix argument KILL-LOCAL-BUFFERS,
kill buffers that are local to the current frame before deleting it."
    (interactive "P")
    (if (cdr (frame-list))
        (progn
          (my/close-utility-tabs-in-frame (selected-frame))
          (when kill-local-buffers
            (my/kill-frame-local-buffers (selected-frame)))
          (delete-frame))
      (my/close-utility-tabs-in-all-frames)
      (save-buffers-kill-emacs)))

  (defun my/delete-frame-kill-local-buffers-command ()
    "Delete current frame after killing buffers local to it."
    (interactive)
    (my/delete-frame-or-emacs t))


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
   ("C-x f" . toggle-frame-fullscreen)
   ("C-x 5 n" . my/new-workflow-frame)
   ("C-x 5 p" . my/new-project-frame)
   ("C-x 5 d" . my/change-frame-default-directory)
   ("C-x 5 r" . my/rename-frame)
   ("C-x 5 s" . my/switch-frame)
   ("C-x 5 0" . my/delete-frame-or-emacs)
   ("C-x 5 K" . my/delete-frame-kill-local-buffers-command)
   ("C-x 5 F" . toggle-frame-fullscreen)
   ("C-x 5 M" . toggle-frame-maximized)
   ("C-x 5 x" . my/delete-frame-or-emacs)
   ("C-x C-b" . ibuffer)
   ("C-x C-S-b" . ibuffer)
   ("C-x b" . switch-to-buffer)
   ("C-x B" . switch-to-buffer-other-window)
   ("C-x k" . kill-current-buffer)
   :map minibuffer-local-map
   ("M-l" . my-minibuffer-insert-symbol-at-point)
   :map minibuffer-local-ns-map
   ("M-l" . my-minibuffer-insert-symbol-at-point)
   :map minibuffer-local-completion-map
   ("M-l" . my-minibuffer-insert-symbol-at-point)
   :map minibuffer-local-must-match-map
   ("M-l" . my-minibuffer-insert-symbol-at-point)))

;; Tabs
(use-package tab-bar
  :ensure nil
  :demand t
  :custom
  ;; Always show the tab bar, including on startup with a single tab.
  (tab-bar-show t)
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show nil)
  :config
  (defun my/tab-bar-switch-to-recent-or-prev-tab ()
    "Switch to the most recent tab, or the previous tab if unavailable."
    (interactive)
    (if (fboundp 'tab-bar-switch-to-recent-tab)
        (call-interactively #'tab-bar-switch-to-recent-tab)
      (tab-bar-switch-to-prev-tab)))

  (defun my/tab-bar-select-tab-by-number (number)
    "Switch to tab-bar tab NUMBER, where 1 is the first tab."
    (interactive "nTab number: ")
    (let ((tabs (tab-bar-tabs)))
      (unless (and (integerp number) (<= 1 number) (<= number (length tabs)))
        (user-error "No tab number %s" number))
      (tab-bar-select-tab number)))

  (defun my/define-tab-number-keys (map)
    "Bind C-c 1..C-c 9 in MAP to switch to tab-bar tabs 1..9."
    (dotimes (index 9)
      (let ((number (1+ index)))
        (define-key map (kbd (format "C-c %d" number))
                    (lambda ()
                      (interactive)
                      (my/tab-bar-select-tab-by-number number))))))

  (my/define-tab-number-keys my/override-map)

  (tab-bar-mode 1)
  (when (fboundp 'tab-bar-history-mode)
    (tab-bar-history-mode 1))
  :bind (:map my/override-map
              ("C-M-i" . tab-bar-switch-to-prev-tab)
              ("<backtab>" . tab-bar-switch-to-prev-tab)
              ("C-M-<tab>" . tab-bar-switch-to-prev-tab)
              ("C-M-o" . tab-bar-switch-to-next-tab)
              ("C-x t l" . my/tab-bar-switch-to-recent-or-prev-tab)))

(use-package tabspaces
  :ensure t
  :after tab-bar
  :demand t
  :preface
  (defun my/tabspaces--local-buffer-name-list (&optional frame)
    "Return local buffer names for FRAME's current tabspace."
    (mapcar #'buffer-name (tabspaces--buffer-list frame)))

  (defun my/tabspaces--read-local-buffer (prompt &optional frame)
    "Read a buffer from FRAME's current tabspace."
    (let ((buffers (my/tabspaces--local-buffer-name-list frame)))
      (read-buffer prompt buffers nil
                   (lambda (b)
                     (member (if (stringp b) b (car b)) buffers)))))

  (defun my/tabspaces--other-live-frame ()
    "Return another live frame, or nil when this is the only frame."
    (seq-find (lambda (frame)
                (and (frame-live-p frame)
                     (not (eq frame (selected-frame)))
                     (not (window-minibuffer-p (frame-selected-window frame)))))
              (frame-list)))

  (defun my/tabspaces-switch-to-buffer-other-window (buffer)
    "Switch to a local tabspace BUFFER in another window."
    (interactive
     (list (my/tabspaces--read-local-buffer
            "Switch to local buffer other window: ")))
    (switch-to-buffer-other-window buffer))

  (defun my/tabspaces-switch-to-buffer-other-frame (buffer)
    "Switch to BUFFER in another frame.
Unlike plain tab-local switching, this offers all live buffers so `C-x 5 b'
can be used like native Emacs to put any buffer into another frame's current
tabspace."
    (interactive
     (list (read-buffer "Switch to buffer other frame: " nil t)))
    (switch-to-buffer-other-frame buffer))

  (defun my/tabspaces-remove-current-buffer-dwim (&optional kill)
    "Remove current buffer from this tabspace without killing it globally.
With prefix argument KILL, really kill the current buffer."
    (interactive "P")
    (if kill
        (kill-current-buffer)
      (let ((buffer (current-buffer)))
        (if (and (bound-and-true-p tabspaces-mode)
                 (fboundp 'tabspaces-remove-current-buffer))
            (progn
              (tabspaces-remove-current-buffer)
              ;; Be strict: remove from this frame/tab list, but leave the
              ;; live buffer available to any other tab or frame using it.
              (when (buffer-live-p buffer)
                (set-frame-parameter
                 nil 'buffer-list
                 (delq buffer (copy-sequence (frame-parameter nil 'buffer-list))))
                (set-frame-parameter
                 nil 'buried-buffer-list
                 (delq buffer (copy-sequence (frame-parameter nil 'buried-buffer-list))))))
          (bury-buffer)))))

  (defun my/tabspaces-add-buffer (buffer)
    "Add an existing global BUFFER to the current tabspace."
    (interactive
     (list (read-buffer "Add buffer to current tabspace: "
                        nil t
                        (lambda (b)
                          (not (member (if (stringp b) b (car b))
                                       (my/tabspaces--local-buffer-name-list)))))))
    (switch-to-buffer buffer))
  :custom
  ;; Make normal buffer switching tab-local.
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-keymap-prefix "C-x t")
  (tabspaces-default-tab "main")
  (tabspaces-remove-to-default t)
  ;; Always keep these utility buffers visible from every tab.
  (tabspaces-include-buffers '("*scratch*" "*Messages*"))
  :config
  ;; Native command remaps: commands that normally act globally now act on the
  ;; current tabspace.  Use `C-u C-x k' when you really want to kill a buffer.
  (define-key tabspaces-mode-map (kbd "C-x t") tabspaces-command-map)
  (define-key tabspaces-mode-map [remap switch-to-buffer] #'tabspaces-switch-to-buffer)
  (define-key tabspaces-mode-map [remap switch-to-buffer-other-window]
              #'my/tabspaces-switch-to-buffer-other-window)
  (define-key tabspaces-mode-map [remap switch-to-buffer-other-frame]
              #'my/tabspaces-switch-to-buffer-other-frame)
  (define-key tabspaces-mode-map [remap kill-buffer] #'my/tabspaces-remove-current-buffer-dwim)
  (define-key tabspaces-mode-map [remap kill-current-buffer] #'my/tabspaces-remove-current-buffer-dwim)
  (unless noninteractive
    (tabspaces-mode 1))
  :bind (:map my/override-map
              ("C-x b" . tabspaces-switch-to-buffer)
              ("C-x B" . my/tabspaces-switch-to-buffer-other-frame)
              ("C-x 5 b" . my/tabspaces-switch-to-buffer-other-frame)
              ("C-x k" . my/tabspaces-remove-current-buffer-dwim)
              ("C-x t TAB" . tabspaces-switch-buffer-and-tab)
              ("C-x t n" . tabspaces-switch-or-create-workspace)
              ("C-x t p" . tabspaces-open-or-create-project-and-workspace)
              ("C-x t a" . my/tabspaces-add-buffer)
              ("C-x t r" . tabspaces-remove-current-buffer)
              ("C-x t k" . tabspaces-close-workspace)))

(defun my/project-tab-name (prefix)
  "Return PREFIX plus the current project/directory name."
  (format "%s: %s"
          prefix
          (or (when-let* ((project (project-current nil)))
                (file-name-nondirectory
                 (directory-file-name (project-root project))))
              (file-name-nondirectory
               (directory-file-name default-directory)))))

(defun my/current-tab-name ()
  "Return the current tab-bar tab name."
  (alist-get 'name (tab-bar--current-tab)))

(defun my/switch-to-named-tab (name)
  "Switch to tab-bar tab NAME, creating it when needed."
  (let ((directory default-directory))
    (if (member name (mapcar (lambda (tab) (alist-get 'name tab)) (tab-bar-tabs)))
        (tab-bar-switch-to-tab name)
      ;; Do not let new tabspaces inherit the current buffer from the
      ;; previous tab; start them on *scratch*, but keep the caller's cwd so
      ;; commands run immediately after tab creation, such as Magit and Pi,
      ;; still see the originating project directory.
      (let ((tab-bar-new-tab-choice
             (lambda ()
               (let ((buffer (get-buffer-create "*scratch*")))
                 (with-current-buffer buffer
                   (setq default-directory directory))
                 buffer))))
        (tab-bar-new-tab))
      (tab-bar-rename-tab name))))

(use-package crux
  :ensure t
  :preface
  (declare-function crux-indent-region-region-or-buffer "crux")
  (declare-function crux-untabify-region-or-buffer "crux")
  (declare-function crux-comment-or-uncomment-region-region-or-line "crux")
  (declare-function crux-kill-region-region-or-sexp-or-line "crux")
  (declare-function crux-kill-ring-save-region-or-point-to-eol "crux")
  :config
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
  (when (display-graphic-p)
    (fontaine-mode t)
    (fontaine-set-preset 'maple-mono)))

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

(defun my/mode-line-tab-name ()
  "Return the current tab-bar tab name for the mode line."
  (when (fboundp 'tab-bar--current-tab)
    (when-let* ((name (alist-get 'name (tab-bar--current-tab))))
      (format "[%s] " name))))

(defun my/mode-line-vc ()
  "Return compact VC branch info."
  (when vc-mode
    (format " %s" (string-trim vc-mode))))

(setq-default
 mode-line-format
 '("%e "
   (:eval (my/mode-line-tab-name))
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
  :init (global-corfu-mode)
  :bind
  (:map my/override-map
        ("M-/" . completion-at-point)))

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
  :hook (ibuffer-mode . my/ibuffer-setup-frame-groups)
  :config
  (require 'ibuf-ext)

  (define-ibuffer-filter frame-name
      "Limit current view to buffers associated with frames named QUALIFIER."
    (:description "frame name"
                  :reader (completing-read "Frame name: "
                                           (my/ibuffer-frame-names) nil t))
    (if (eq qualifier 'my/ibuffer-unframed)
        (not (my/ibuffer-buffer-frame-names buf))
      (member qualifier (my/ibuffer-buffer-frame-names buf))))

  (defun my/ibuffer-frame-name (frame)
    "Return FRAME's display name."
    (or (frame-parameter frame 'name) "<unnamed>"))

  (defun my/ibuffer-frame-names ()
    "Return the names of all live frames."
    (delete-dups (mapcar #'my/ibuffer-frame-name (frame-list))))

  (defun my/ibuffer-buffer-frame-names (buffer)
    "Return names of live frames whose tabspaces include BUFFER."
    (delq nil
          (mapcar (lambda (frame)
                    (when (memq buffer (my/frame-buffer-list frame))
                      (my/ibuffer-frame-name frame)))
                  (frame-list))))

  (defun my/ibuffer-refresh-frame-filter-groups ()
    "Regenerate `ibuffer' filter groups from current frame names."
    (setq ibuffer-saved-filter-groups
          `(("default"
             ,@(mapcar (lambda (name)
                         (list (format "Frame: %s" name)
                               `(frame-name . ,name)))
                       (my/ibuffer-frame-names))
             ("Unframed" (frame-name . my/ibuffer-unframed))))))

  (defun my/ibuffer-setup-frame-groups ()
    "Group `ibuffer' buffers by frame name."
    (ibuffer-auto-mode 1)
    (my/ibuffer-refresh-frame-filter-groups)
    (ibuffer-switch-to-saved-filter-groups "default"))

  (defun my/ibuffer-update-frame-groups ()
    "Refresh frame-name groups, then update `ibuffer'."
    (interactive)
    (my/ibuffer-refresh-frame-filter-groups)
    (ibuffer-switch-to-saved-filter-groups "default")
    (ibuffer-update nil t))

  (setq ibuffer-show-empty-filter-groups nil
        ibuffer-always-show-last-buffer nil
        ibuffer-default-sorting-mode 'recency
        ibuffer-human-readable-size t)

  :bind (("C-x C-M-b" . ibuffer)
         :map ibuffer-mode-map
         ("g" . my/ibuffer-update-frame-groups)
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
  :preface
  (require 'project)
  (require 'seq)

  (defvar my/vterm-return-tab nil
    "Tab to return to when toggling away from the vterm tab.")

  (defvar-local my/vterm-tab-name nil
    "Dedicated tab name for this vterm buffer.")

  (defun my/vterm-tab-name ()
    "Return the dedicated vterm tab name for this frame."
    "vterm")

  (defun my/vterm--project-dir ()
    "Return project root if available, else current `default-directory`."
    (if-let* ((pr (project-current nil)))
        (expand-file-name (project-root pr))
      (expand-file-name default-directory)))

  (defun my/vterm--buffer-p (b)
    (with-current-buffer b
      (derived-mode-p 'vterm-mode)))

  (defun my/vterm--buf-dir (b)
    "Return normalized `default-directory` for vterm buffer B."
    (with-current-buffer b
      (expand-file-name default-directory)))

  (defun my/vterm--find-by-dir (dir)
    "Find an existing vterm buffer whose `default-directory` matches DIR."
    (let ((target (expand-file-name dir)))
      (seq-find (lambda (b)
                  (and (my/vterm--buffer-p b)
                       (string= (my/vterm--buf-dir b) target)))
                (buffer-list))))

  (defun my/vterm--new-in-dir (dir)
    "Create a new vterm buffer and start it in DIR."
    (let ((default-directory dir))
      (vterm (generate-new-buffer-name
              (format "*vterm: %s*"
                      (file-name-nondirectory
                       (directory-file-name dir)))))))

  (defun my/vterm-in-tab (&optional new)
    "Open/toggle vterm in a dedicated project tab.
With prefix argument NEW, always create a new vterm buffer."
    (interactive "P")
    (let* ((dir (my/vterm--project-dir))
           (vterm-tab (my/vterm-tab-name))
           (current-tab (my/current-tab-name))
           (tab-names (mapcar (lambda (tab) (alist-get 'name tab)) (tab-bar-tabs)))
           (tab-exists (member vterm-tab tab-names)))
      (if (and (not new)
               (string= current-tab vterm-tab)
               my/vterm-return-tab
               (member my/vterm-return-tab tab-names))
          (tab-bar-switch-to-tab my/vterm-return-tab)
        (unless (string= current-tab vterm-tab)
          (setq my/vterm-return-tab current-tab))
        (my/switch-to-named-tab vterm-tab)
        (unless tab-exists
          (delete-other-windows)
          (set-frame-parameter nil 'buffer-list nil))
        (let ((buf (and (not new) (my/vterm--find-by-dir dir))))
          (if buf
              (switch-to-buffer buf)
            (my/vterm--new-in-dir dir)))
        (setq-local my/vterm-tab-name vterm-tab)
        (unless tab-exists
          (set-frame-parameter nil 'buffer-list (list (current-buffer)))))))
  :hook (vterm-mode . (lambda () (display-line-numbers-mode -1)))
  :bind ("C-c t" . my/vterm-in-tab))

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

  (defvar my/eshell-return-tab nil
    "Tab to return to when toggling away from the eshell tab.")

  (defvar-local my/eshell-tab-name nil
    "Dedicated tab name for this eshell buffer.")

  (defun my/eshell-tab-name ()
    "Return the dedicated eshell tab name for this frame."
    "eshell")

  (defun my/eshell--open-in-tab (dir &optional new)
    "Open/toggle eshell for DIR in a dedicated project tab.
With NEW, always create a new eshell buffer."
    (let* ((eshell-tab (my/eshell-tab-name))
           (current-tab (my/current-tab-name))
           (tab-names (mapcar (lambda (tab) (alist-get 'name tab)) (tab-bar-tabs)))
           (tab-exists (member eshell-tab tab-names)))
      (if (and (not new)
               (string= current-tab eshell-tab)
               my/eshell-return-tab
               (member my/eshell-return-tab tab-names))
          (tab-bar-switch-to-tab my/eshell-return-tab)
        (unless (string= current-tab eshell-tab)
          (setq my/eshell-return-tab current-tab))
        (my/switch-to-named-tab eshell-tab)
        (unless tab-exists
          (delete-other-windows)
          (set-frame-parameter nil 'buffer-list nil))
        (let ((buf (and (not new) (my/eshell--find-by-dir dir))))
          (if buf
              (switch-to-buffer buf)
            (my/eshell--new-in-dir dir)))
        (setq-local my/eshell-tab-name eshell-tab)
        (unless tab-exists
          (set-frame-parameter nil 'buffer-list (list (current-buffer)))))))

  (defun my/toggle-eshell-here (&optional new)
    "Open/toggle eshell for the current directory in a dedicated tab.
With prefix argument NEW, always create a new eshell buffer."
    (interactive "P")
    (my/eshell--open-in-tab (my/eshell--current-dir) new))

  (defun my/eshell--project-dir ()
    "Return project root if available, else current `default-directory`."
    (if-let* ((pr (project-current nil)))
        (expand-file-name (project-root pr))
      (my/eshell--current-dir)))

  (defun my/toggle-eshell-project (&optional new)
    "Open/toggle eshell for the project root in a dedicated tab.
With prefix argument NEW, always create a new eshell buffer."
    (interactive "P")
    (my/eshell--open-in-tab (my/eshell--project-dir) new))

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
    :hook (eshell-mode . eat-eshell-mode)
    :config
    ;; Eat's Eshell keymaps can take precedence and pass C-c digits through to
    ;; the terminal.  Bind tab switching there explicitly too.
    (when (fboundp 'my/define-tab-number-keys)
      (my/define-tab-number-keys eat-eshell-emacs-mode-map)
      (my/define-tab-number-keys eat-eshell-semi-char-mode-map)))

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

  (defun my/eshell-setup-keymaps ()
    "Set up Eshell key bindings without global tab-switch overrides."
    ;; `my/override-map' has higher priority than `eshell-mode-map'.  Keep the
    ;; rest of the override bindings, but let C-M-i/C-M-<tab> complete in Eshell.
    (let ((map (copy-keymap my/override-map)))
      (define-key map (kbd "C-M-i") nil)
      (define-key map (kbd "C-M-<tab>") nil)
      (when (fboundp 'my/define-tab-number-keys)
        (my/define-tab-number-keys map))
      (setq-local minor-mode-overriding-map-alist
                  (cons (cons 'my/override-mode map)
                        (assq-delete-all 'my/override-mode
                                         minor-mode-overriding-map-alist))))
    (when (fboundp 'my/define-tab-number-keys)
      (my/define-tab-number-keys eshell-mode-map))
    (keymap-set eshell-mode-map "C-M-i" #'completion-at-point)
    (keymap-set eshell-mode-map "C-M-<tab>" #'completion-at-point)
    (keymap-set eshell-mode-map "C-r" #'my/eshell-history-fuzzy)
    (keymap-set eshell-mode-map "C-l" #'eshell/clear))

  (add-hook 'eshell-mode-hook #'my/eshell-setup-keymaps))

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
  :preface
  (defvar my/magit-return-tab nil
    "Tab to return to when toggling away from the magit tab.")

  (defvar-local my/magit-tab-name nil
    "Dedicated tab name for this Magit buffer.")

  (defun my/magit-tab-name ()
    "Return the dedicated Magit tab name for the current project."
    (my/project-tab-name "magit"))

  (defun my/magit-close-tab (&optional tab-name)
    "Close the dedicated Magit TAB-NAME."
    (let ((name (or tab-name my/magit-tab-name (my/magit-tab-name))))
      (when (and name
                 (> (length (tab-bar-tabs)) 1)
                 (member name (mapcar (lambda (tab) (alist-get 'name tab))
                                      (tab-bar-tabs))))
        (tab-bar-close-tab-by-name name))))

  (defun my/magit-status-in-tab ()
    "Open/toggle `magit-status' in a dedicated project tab."
    (interactive)
    (let ((magit-tab (my/magit-tab-name))
          (current-tab (my/current-tab-name)))
      (if (and (string= current-tab magit-tab)
               (derived-mode-p 'magit-mode)
               my/magit-return-tab
               (member my/magit-return-tab
                       (mapcar (lambda (tab) (alist-get 'name tab)) (tab-bar-tabs))))
          (tab-bar-switch-to-tab my/magit-return-tab)
        (setq my/magit-return-tab current-tab)
        (my/switch-to-named-tab magit-tab)
        (call-interactively #'magit-status)
        (setq-local my/magit-tab-name magit-tab))))

  (defun my/magit-quit-and-close-tab ()
    "Quit Magit and close its dedicated tab."
    (interactive)
    (let ((tab-name (or my/magit-tab-name (my/magit-tab-name))))
      (quit-window t)
      (my/magit-close-tab tab-name)))

  (defun my/magit-close-tab-after-kill ()
    "Close this buffer's dedicated Magit tab after the buffer is killed."
    (when my/magit-tab-name
      (run-at-time 0 nil #'my/magit-close-tab my/magit-tab-name)))
  :bind (("C-x v d" . my/magit-status-in-tab)
         ("C-x v v" . magit-commit-create)
         ("C-x v l" . magit-log-buffer-file)
         ("C-x v L" . magit-log)
         ("C-x v b" . magit-blame-addition)
         ("C-x v u" . magit-revert)
         ("C-x v P" . magit-push-current)
         ("C-x v p" . magit-pull-branch)
         ("C-x v !" . magit-dispatch)
         :map magit-status-mode-map
         ("q" . my/magit-quit-and-close-tab)
         ("C-x v d" . my/magit-status-in-tab)
         :map magit-mode-map
         ("C-x v d" . my/magit-status-in-tab))
  :custom
  (magit-process-connection-type nil)
  (vc-handled-backends '(Git))
  :config
  (add-hook 'magit-status-mode-hook
            (lambda ()
              (add-hook 'kill-buffer-hook #'my/magit-close-tab-after-kill nil t)))
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

(defun my/install-missing-treesit-grammars ()
  "Install Tree-sitter grammars used by this configuration when missing."
  (interactive)
  (unless (fboundp 'treesit-install-language-grammar)
    (user-error "Tree-sitter grammar installation is not available"))
  (dolist (language '(svelte))
    (unless (treesit-language-available-p language)
      (treesit-install-language-grammar language))))

(use-package treesit-auto
  :ensure t
  :defer 1
  :custom
  (treesit-auto-install 'prompt)
  :config
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
  :commands (mc/mark-next-like-this
             mc/mark-previous-like-this
             mc/skip-to-next-like-this
             mc/skip-to-previous-like-this
             mc/mark-all-like-this
             mc/edit-lines)
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
  :preface
  (require 'cl-lib)

  (defvar my/pi-coding-agent-return-tab nil
    "Tab to return to when toggling away from the pi tab.")

  (defun my/pi-coding-agent-in-tab ()
    "Open `pi-coding-agent' in a dedicated project tab, or return from it.

When invoked from the pi tab, switch back to the tab from which pi was
opened without hiding or toggling the pi buffer."
    (interactive)
    (let ((pi-tab (my/project-tab-name "pi"))
          (current-tab (my/current-tab-name)))
      (if (and (string= current-tab pi-tab)
               my/pi-coding-agent-return-tab
               (member my/pi-coding-agent-return-tab
                       (mapcar (lambda (tab) (alist-get 'name tab)) (tab-bar-tabs))))
          (tab-bar-switch-to-tab my/pi-coding-agent-return-tab)
        (unless (string= current-tab pi-tab)
          (setq my/pi-coding-agent-return-tab current-tab))
        (my/switch-to-named-tab pi-tab)
        (call-interactively #'pi-coding-agent))))

  (defun my/pi-coding-agent-return-tab ()
    "Switch back from the pi tab without hiding the pi buffer."
    (interactive)
    (if (and my/pi-coding-agent-return-tab
             (member my/pi-coding-agent-return-tab
                     (mapcar (lambda (tab) (alist-get 'name tab)) (tab-bar-tabs))))
        (tab-bar-switch-to-tab my/pi-coding-agent-return-tab)
      (tab-bar-switch-to-prev-tab)))

  (defun my/pi-coding-agent--sort-session-entries-by-time (entries)
    "Return pi session ENTRIES sorted newest first."
    (sort (copy-sequence entries)
          (lambda (a b)
            (time-less-p
             (plist-get (plist-get b :metadata) :modified-time)
             (plist-get (plist-get a :metadata) :modified-time)))))

  (defun my/pi-coding-agent--preserve-resume-order (orig-fun &rest args)
    "Call ORIG-FUN without minibuffer completion re-sorting its candidates."
    (let ((completions-sort nil)
          (orig-completing-read (symbol-function 'completing-read)))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (prompt collection &rest read-args)
                   (let ((collection
                          (if (and (string= prompt "Resume session: ")
                                   (functionp collection))
                              (lambda (string pred action)
                                (if (eq action 'metadata)
                                    '(metadata
                                      (display-sort-function . identity)
                                      (cycle-sort-function . identity))
                                  (funcall collection string pred action)))
                            collection)))
                     (apply orig-completing-read prompt collection read-args)))))
        (apply orig-fun args))))
  :config
  (unless (advice-member-p #'my/pi-coding-agent--sort-session-entries-by-time
                           'pi-coding-agent--list-session-entries)
    (advice-add 'pi-coding-agent--list-session-entries
                :filter-return #'my/pi-coding-agent--sort-session-entries-by-time))
  (unless (advice-member-p #'my/pi-coding-agent--preserve-resume-order
                           'pi-coding-agent--resume-session-from-directory)
    (advice-add 'pi-coding-agent--resume-session-from-directory
                :around #'my/pi-coding-agent--preserve-resume-order))
  :bind
  ("C-c a" . my/pi-coding-agent-in-tab)
  (:map pi-coding-agent-chat-mode-map
        ("C-c a" . my/pi-coding-agent-return-tab))
  (:map pi-coding-agent-input-mode-map
        ("C-c a" . my/pi-coding-agent-return-tab)))

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
;; (use-package sublimity
;;   :ensure t
;;   :config
;;   (require 'sublimity-attractive)
;;   (sublimity-mode)
;;   :bind (("C-c z" . sublimity-mode)))

(use-package olivetti
  :ensure t
  :demand t
  :bind
  ("C-c z" . olivetti-mode)
  :custom
  (olivetti-body-width 120)
  (olivetti-minimum-body-width 80)
  :hook
  (olivetti-mode . (lambda () (visual-line-mode 1)))
  :config
  (defun my/turn-on-olivetti ()
    (unless (or (minibufferp)
                (derived-mode-p 'special-mode 'dired-mode))
      (olivetti-mode 1)))

  (define-globalized-minor-mode global-olivetti-mode
    olivetti-mode
    my/turn-on-olivetti)

  (global-olivetti-mode 1))

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
  (delete-by-moving-to-trash t)
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

  (defun my/save-clipboard-image (file)
    "Save a PNG image from the Wayland clipboard to FILE."
    (interactive
     (list
      (read-file-name
       "Save clipboard image to: "
       (if (derived-mode-p 'dired-mode)
           (dired-current-directory)
         default-directory)
       nil nil
       (format-time-string "screenshot-%Y%m%d-%H%M%S.png"))))
    (unless (executable-find "wl-paste")
      (user-error "wl-paste is not installed"))
    (let ((file (expand-file-name file)))
      (when (file-directory-p file)
        (setq file (expand-file-name
                    (format-time-string "screenshot-%Y%m%d-%H%M%S.png")
                    file)))
      (make-directory (file-name-directory file) t)
      (unless (zerop (call-process-shell-command
                      (format "wl-paste --type image/png > %s"
                              (shell-quote-argument file))))
        (when (file-exists-p file)
          (delete-file file))
        (user-error "Clipboard does not contain a PNG image"))
      (when (derived-mode-p 'dired-mode)
        (revert-buffer))
      (message "Saved clipboard image to %s" file)))
  :bind (:map dired-mode-map
              ("h" . dired-up-directory)
              ("l" . dired-find-file)
              ("C-u i" . dired-kill-subdir)
              ("C-c C-y" . my/save-clipboard-image)))

(use-package dired-clipboard
  :vc (:url "https://github.com/kn66/dired-clipboard.el"
            :rev :newest
            :branch "main")
  :hook (dired-mode . dired-clipboard-mode))

(use-package ready-player
  :ensure t
  :defer 2
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

;; Desktop session persistence

(use-package desktop
  :ensure nil
  :custom
  (desktop-path (list user-emacs-directory))
  (desktop-dirname user-emacs-directory)
  (desktop-base-file-name "desktop.el")
  (desktop-save t)
  (desktop-load-locked-desktop t)
  (desktop-restore-eager 5)
  (desktop-auto-save-timeout 300)
  :config
  ;; Keep frame restoration enabled.  `my/update-frame-name' is defensive while
  ;; frameset is rebuilding frames/windows during desktop restore.
  (when (boundp 'desktop-restore-frames)
    (setq desktop-restore-frames t))
  ;; Do not persist transient utility workspaces
  (unless noninteractive
    (add-hook 'desktop-save-hook #'my/close-utility-tabs-before-desktop-save))
  (setq desktop-buffers-not-to-save
        (concat "\\`\\*\\(?:vterm\\|eshell\\|magit\\|pi-coding-agent\\|pi-agent\\).*\\*\\(?:<[0-9]+>\\)?\\'"
                (when desktop-buffers-not-to-save
                  (concat "\\|" desktop-buffers-not-to-save))))
  (unless noninteractive
    (desktop-save-mode 1)))

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

(use-package visual-fill-column
  :ensure t
  :custom
  (visual-fill-column-width 80)
  (visual-fill-column-center-text t))

(defvar-local my/org-presentation--mode-line-local-p nil)
(defvar-local my/org-presentation--mode-line-format nil)
(defvar-local my/org-presentation--tab-line-local-p nil)
(defvar-local my/org-presentation--tab-line-format nil)
(defvar-local my/org-presentation--frame nil)
(defvar-local my/org-presentation--tab-bar-lines nil)

(defun my/org-presentation-center-slide (&rest _)
  "Center the current org-present slide in the window."
  (setq-local visual-fill-column-width 80)
  (setq-local visual-fill-column-center-text t)
  (when (bound-and-true-p visual-fill-column-mode)
    (visual-fill-column-adjust))
  (recenter))

(defun my/org-presentation-start ()
  "Presentation display tweaks."
  (setq-local my/org-presentation--mode-line-local-p
              (local-variable-p 'mode-line-format))
  (setq-local my/org-presentation--mode-line-format mode-line-format)
  (setq-local my/org-presentation--tab-line-local-p
              (local-variable-p 'tab-line-format))
  (setq-local my/org-presentation--tab-line-format tab-line-format)
  (setq-local mode-line-format nil)
  (setq-local tab-line-format nil)
  (setq-local my/org-presentation--frame (selected-frame))
  (setq-local my/org-presentation--tab-bar-lines
              (frame-parameter nil 'tab-bar-lines))
  (modify-frame-parameters nil '((tab-bar-lines . 0)))
  (force-mode-line-update)
  (text-scale-set 3)
  (visual-line-mode 1)
  (visual-fill-column-mode 1)
  (my/org-presentation-center-slide)
  (org-display-inline-images)
  (when (fboundp 'org-latex-preview)
    (org-latex-preview '(16))))

(defun my/org-presentation-end ()
  "Undo presentation display tweaks."
  (if my/org-presentation--mode-line-local-p
      (setq-local mode-line-format my/org-presentation--mode-line-format)
    (kill-local-variable 'mode-line-format))
  (if my/org-presentation--tab-line-local-p
      (setq-local tab-line-format my/org-presentation--tab-line-format)
    (kill-local-variable 'tab-line-format))
  (when (frame-live-p my/org-presentation--frame)
    (modify-frame-parameters
     my/org-presentation--frame
     `((tab-bar-lines . ,my/org-presentation--tab-bar-lines))))
  (force-mode-line-update)
  (text-scale-set 0)
  (visual-fill-column-mode -1)
  (org-remove-inline-images)
  (when (fboundp 'org-clear-latex-preview)
    (org-clear-latex-preview)))

(use-package org-present
  :ensure t
  :after org
  :hook ((org-present-mode . my/org-presentation-start)
         (org-present-mode-quit . my/org-presentation-end))
  :config
  (add-hook 'org-present-after-navigate-functions
            #'my/org-presentation-center-slide)
  :bind (:map org-mode-map
              ("C-c p" . org-present)))

(use-package ob-mermaid
  :ensure t)

(use-package org
  :ensure nil
  :preface
  (require 'subr-x)
  (require 'cl-lib)
  (require 'seq)
  (define-prefix-command 'my/org-prefix-map)

  (defun my/org-notes-tab-name ()
    "Return the dedicated notes tab name."
    "notes")

  (defun my/org-read-note-file ()
    "Read an Org note file from `org-directory'."
    (let ((default-directory (file-name-as-directory org-directory)))
      (read-file-name "Find note: " default-directory)))

  (defun my/org-find-note ()
    "Open `find-file' from `org-directory' to search or create notes."
    (interactive)
    (find-file (my/org-read-note-file)))

  (defun my/org-find-note-in-tab ()
    "Read a note file, then open it in a dedicated notes tab.
Do not create or switch tabs until after the file has been selected."
    (interactive)
    (let ((file (my/org-read-note-file))
          (notes-tab (my/org-notes-tab-name)))
      (let ((tab-exists (member notes-tab
                                (mapcar (lambda (tab) (alist-get 'name tab))
                                        (tab-bar-tabs)))))
        (my/switch-to-named-tab notes-tab)
        (unless tab-exists
          (when (fboundp 'tabspaces-reset-buffer-list)
            (tabspaces-reset-buffer-list))))
      (find-file file)))

  (defun my/org-todo-file ()
    "Return the Org todos file."
    (expand-file-name "todo.org" org-directory))

  (defun my/org-ensure-todo-file ()
    "Ensure `my/org-todo-file' exists, then return it."
    (let ((file (my/org-todo-file)))
      (make-directory (file-name-directory file) t)
      (unless (file-exists-p file)
        (with-temp-buffer (write-file file)))
      file))

  (defvar my/org-todo-default-heading "Inbox"
    "Default top-level heading for todo captures when the todo file is empty.")

  (defun my/org-normalize-heading-title (heading)
    "Return a safe one-line Org heading title from HEADING."
    (let ((title (replace-regexp-in-string
                  "[[:space:]\n\r]+" " " (string-trim (or heading "")))))
      (if (string-empty-p title)
          (user-error "Heading cannot be empty")
        title)))

  (defun my/org-ensure-heading (heading)
    "Ensure a top-level HEADING exists in the current Org buffer.
Leave point at the beginning of that heading."
    (let ((heading (my/org-normalize-heading-title heading))
          found)
      (goto-char (point-min))
      (while (and (not found) (re-search-forward org-heading-regexp nil t))
        (when (and (= (org-current-level) 1)
                   (string= (org-get-heading t t t t) heading))
          (setq found t)
          (beginning-of-line)))
      (unless found
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert "* " heading "\n")
        (forward-line -1)))
    (point))

  (defun my/org-todo-title-headings ()
    "Return valid top-level todo title headings from `my/org-todo-file'."
    (with-current-buffer (find-file-noselect (my/org-ensure-todo-file))
      (org-with-wide-buffer
       (goto-char (point-min))
       (let (headings)
         (while (re-search-forward org-heading-regexp nil t)
           (when (= (org-current-level) 1)
             (let ((heading (org-get-heading t t t t)))
               (unless (or (org-get-todo-state)
                           (member heading headings))
                 (push heading headings)))))
         (nreverse headings)))))

  (defun my/org-capture-todo-under-heading (heading)
    "Position `org-capture' on top-level HEADING for a child todo.
For `file+function' capture targets, Org expects the function to leave
point on the parent heading; Org then inserts the entry as a child."
    (set-buffer (find-file-noselect (my/org-ensure-todo-file)))
    (save-restriction
      (widen)
      (my/org-ensure-heading heading)
      (point)))

  (defun my/org-capture-todo ()
    "Position `org-capture' under a chosen todo title in the todos file."
    (let* ((titles (my/org-todo-title-headings))
           (default (or (car titles) my/org-todo-default-heading))
           (title (my/org-normalize-heading-title
                   (completing-read
                    (format "Todo title (default %s): " default)
                    titles nil nil nil nil default))))
      (my/org-capture-todo-under-heading title)))

  (defun my/org-confirm-babel-evaluate (lang _body)
    "Return non-nil when Org should confirm evaluating a LANG block."
    (not (member lang '("emacs-lisp"))))

  (defvar my/org-mermaid-preview-cache-directory
    (expand-file-name "org-mermaid-preview/" user-emacs-directory)
    "Directory for cached Mermaid preview images.")

  (defun my/org-mermaid-preview-clear (&optional beg end)
    "Remove Mermaid preview overlays between BEG and END."
    (interactive)
    (remove-overlays (or beg (point-min)) (or end (point-max))
                     'my/org-mermaid-preview t))

  (defun my/org-mermaid-preview--params-with-file (params file)
    "Return Mermaid PARAMS for preview output FILE."
    (cons (cons :file file)
          (seq-remove (lambda (param)
                        (memq (car-safe param) '(:file :results :exports)))
                      params)))

  (defun my/org-mermaid-preview--output-file (body params)
    "Return cache file name for Mermaid BODY and PARAMS."
    (let* ((source-file (cdr (assq :file params)))
           (extension (or (and source-file (file-name-extension source-file)) "svg"))
           (hash (secure-hash 'sha1 (prin1-to-string (list body params)))))
      (expand-file-name (concat hash "." extension)
                        my/org-mermaid-preview-cache-directory)))

  (defun my/org-mermaid-preview--image (file)
    "Return an Org-style image descriptor for FILE."
    (if (fboundp 'org--create-inline-image)
        (org--create-inline-image file nil)
      (create-image file)))

  (defun my/org-mermaid-preview--block-at-point ()
    "Return current Mermaid source block element, or nil."
    (let ((element (org-element-at-point)))
      (when (and (eq (org-element-type element) 'src-block)
                 (string= (org-element-property :language element) "mermaid"))
        element)))

  (defun my/org-mermaid-preview--display-end (element)
    "Return end position for previewing ELEMENT without hiding following blank lines."
    (- (org-element-property :end element)
       (or (org-element-property :post-blank element) 0)))

  (defun my/org-mermaid-preview--render-block (element)
    "Render Mermaid source block ELEMENT as an overlay replacing the block."
    (require 'ob-mermaid)
    (let* ((begin (org-element-property :begin element))
           (end (my/org-mermaid-preview--display-end element))
           (info (save-excursion
                   (goto-char begin)
                   (org-babel-get-src-block-info)))
           (body (nth 1 info))
           (params (nth 2 info))
           (file (my/org-mermaid-preview--output-file body params)))
      (make-directory my/org-mermaid-preview-cache-directory t)
      (unless (file-exists-p file)
        (org-babel-execute:mermaid
         body
         (my/org-mermaid-preview--params-with-file params file)))
      (my/org-mermaid-preview-clear begin end)
      (let ((overlay (make-overlay begin end nil t nil)))
        (overlay-put overlay 'my/org-mermaid-preview t)
        (overlay-put overlay 'display (my/org-mermaid-preview--image file))
        (overlay-put overlay 'help-echo "Mermaid preview. C-c C-x C-m toggles source.")
        overlay)))

  (defun my/org-mermaid-preview-at-point ()
    "Toggle Mermaid source block preview at point."
    (interactive)
    (let* ((element (my/org-mermaid-preview--block-at-point))
           (begin (and element (org-element-property :begin element)))
           (end (and element (my/org-mermaid-preview--display-end element))))
      (unless element
        (user-error "Point is not on a Mermaid source block"))
      (if (cl-some (lambda (overlay)
                     (overlay-get overlay 'my/org-mermaid-preview))
                   (overlays-in begin end))
          (my/org-mermaid-preview-clear begin end)
        (my/org-mermaid-preview--render-block element))))

  (defun my/org-mermaid-preview-available-p ()
    "Return non-nil when Mermaid previews can be rendered."
    (or (and (boundp 'ob-mermaid-cli-path) ob-mermaid-cli-path)
        (executable-find "mmdc")))

  (defun my/org-mermaid-preview-buffer ()
    "Render all Mermaid source blocks in the current Org buffer as previews."
    (interactive)
    (unless (my/org-mermaid-preview-available-p)
      (user-error "mmdc not found; set `ob-mermaid-cli-path' or install mermaid-cli"))
    (my/org-mermaid-preview-clear)
    (org-with-wide-buffer
     (org-element-map (org-element-parse-buffer) 'src-block
       (lambda (element)
         (when (string= (org-element-property :language element) "mermaid")
           (my/org-mermaid-preview--render-block element))))))

  (defvar-local my/org-mermaid-preview-timer nil
    "Idle timer used to debounce automatic Mermaid preview rendering.")

  (defun my/org-mermaid-preview-buffer-later ()
    "Render Mermaid previews in the current Org buffer after Emacs becomes idle."
    (when (timerp my/org-mermaid-preview-timer)
      (cancel-timer my/org-mermaid-preview-timer))
    (let ((buffer (current-buffer)))
      (setq my/org-mermaid-preview-timer
            (run-with-idle-timer
             0.5 nil
             (lambda ()
               (when (and (buffer-live-p buffer)
                          (my/org-mermaid-preview-available-p))
                 (with-current-buffer buffer
                   (when (derived-mode-p 'org-mode)
                     (ignore-errors
                       (my/org-mermaid-preview-buffer))))))))))

  (defun my/org-mermaid-preview-setup-auto-render ()
    "Automatically render Mermaid previews when opening or saving Org buffers."
    (my/org-mermaid-preview-buffer-later)
    (add-hook 'after-save-hook #'my/org-mermaid-preview-buffer-later nil t))
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
   (org-mode . org-indent-mode)
   (org-mode . my/org-mermaid-preview-setup-auto-render))
  :custom
  ;; Editing
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-insert-heading-respect-content t)
  (org-M-RET-may-split-line '((default . nil)))
  (org-image-actual-width nil)

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
  (org-refile-targets `((org-agenda-files :maxlevel . 3)
                        (,(my/org-todo-file) :maxlevel . 3)))
  (org-outline-path-complete-in-steps nil)
  (org-refile-use-outline-path 'file)
  (org-capture-templates
   `(("t" "Todo" entry
      (file+function ,(my/org-todo-file) my/org-capture-todo)
      "* TODO %?\n %i\n")
     ("i" "Inbox task" entry
      (file "inbox.org")
      "* TODO %?\n")))

                                        ; (org-preview-latex-default-process 'dvipng)
  (org-preview-latex-default-process 'dvisvgm)
  (org-format-latex-options
   (plist-put org-format-latex-options :scale 2.0))
  (ob-mermaid-cli-path "mmdc")

  :config
  (setq org-babel-default-header-args:mermaid
        '((:results . "file graphics silent") (:exports . "none")))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (python . t)
     (latex . t)
     (mermaid . t)))
  (add-hook 'org-babel-after-execute-hook #'org-display-inline-images)
  (add-hook 'org-babel-after-execute-hook
            (lambda ()
              (when (derived-mode-p 'org-mode)
                (my/org-mermaid-preview-buffer-later))))
  :bind
  (:map my/org-prefix-map
        ("a" . org-agenda)
        ("c" . org-capture)
        ("n" . my/org-find-note-in-tab)
        ("T" . org-todo-list)
        :map org-mode-map
        ("C-c C-d" . org-deadline)
        ("C-c C-x C-m" . my/org-mermaid-preview-at-point)
        ("C-c C-x M" . my/org-mermaid-preview-buffer)))

(use-package org-embed
  :vc (:url "https://github.com/yibie/org-embed"
            :rev :newest
            :branch "main")
  :after org
  :demand t
  :preface
  (define-prefix-command 'my/org-embed-prefix-map)

  (defun my/org-embed-disable-conflicting-modes (&rest _)
    "Disable Org display modes that can conflict with xwidget embeds."
    (when (bound-and-true-p org-modern-mode)
      (org-modern-mode -1)))
  :custom
  (org-embed-video-width 800)
  (org-embed-video-height 450)
  (org-embed-webpage-width 1024)
  (org-embed-webpage-height 768)
  (org-embed-pdf-width 800)
  (org-embed-pdf-height 1000)
  :config
  (unless (fboundp 'xwidget-webkit-new-session)
    (message "org-embed requires Emacs with xwidget-webkit support"))
  ;; org-embed's xwidgets may not display correctly with org-modern enabled.
  (dolist (fn '(org-embed-video
                org-embed-local-video
                org-embed-webpage
                org-embed-pdf
                org-embed-follow-link))
    (advice-add fn :before #'my/org-embed-disable-conflicting-modes))
  :bind
  (:map my/org-prefix-map
        ("e" . my/org-embed-prefix-map)
        :map my/org-embed-prefix-map
        ("v" . org-embed-video)
        ("V" . org-embed-local-video)
        ("w" . org-embed-webpage)
        ("p" . org-embed-pdf)
        ("c" . org-embed-clean-at-point)
        ("C" . org-embed-clean-all)
        :map org-mode-map
        ("C-c e" . my/org-embed-prefix-map)))

;; LSP

(use-package mason
  :ensure t
  :defer 2
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
           ("vtsls" "--stdio")
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
  :bind ("C-c F" . elfeed)
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
  :defer t)
