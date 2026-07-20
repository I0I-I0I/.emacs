;;; paste-image.el --- Paste clipboard images into Emacs -*- lexical-binding: t; -*-

;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: multimedia, convenience

;;; Commentary:
;; Save an image from the system clipboard into:
;;
;;   PROJECT-ROOT/pics/TIMESTAMP-RANDOM.png
;;
;; When the current file is not in a project, use the directory
;; containing the current file as the root.  For an unsaved buffer,
;; use `default-directory'.
;;
;; Inserted links are relative to the current document, not the project
;; root, so links also work from files in project subdirectories.
;;
;; `paste-image-directory' must be a relative path contained inside the
;; selected root.  Remote directories are intentionally unsupported.
;;
;; External programs:
;; - macOS: pngpaste
;; - Wayland: wl-paste
;; - X11: xclip
;; - Windows: PowerShell

;;; Code:

(require 'project nil t)
(require 'subr-x)

(defgroup paste-image nil
  "Paste images from the system clipboard."
  :group 'convenience
  :prefix "paste-image-")

(defcustom paste-image-directory "pics"
  "Directory in which clipboard images are saved.

The value must be a non-empty relative path contained inside the
project root or current file directory.  Absolute paths, home-relative
paths, and parent-directory components are rejected."
  :type 'string
  :group 'paste-image)

(defcustom paste-image-timestamp-format "%Y%m%d-%H%M%S"
  "Timestamp format used as the prefix of image filenames.

A random suffix is added to reserve each filename atomically."
  :type 'string
  :group 'paste-image)

(defun paste-image--project-root (project)
  "Return the root directory of PROJECT, or nil when unavailable.

Support both the current `project-root' API and the older
`project-roots' API."
  (or (and (fboundp 'project-root)
           (ignore-errors
             (project-root project)))
      (and (fboundp 'project-roots)
           (ignore-errors
             (car (project-roots project))))))

(defun paste-image--root ()
  "Return the root directory for the pasted image.

Use the project root when available.  Otherwise, use the directory
containing the current file.  For an unsaved buffer, use
`default-directory'."
  (let* ((project
          (and (fboundp 'project-current)
               (project-current nil)))
         (project-root
          (and project
               (paste-image--project-root project))))
    (file-name-as-directory
     (cond
      (project-root
       project-root)
      (buffer-file-name
       (file-name-directory buffer-file-name))
      (t
       default-directory)))))

(defun paste-image--link-base ()
  "Return the directory relative to which an inserted link is resolved."
  (file-name-as-directory
   (if buffer-file-name
       (file-name-directory buffer-file-name)
     default-directory)))

(defun paste-image--image-directory (root)
  "Return the validated image directory inside ROOT.

Signal `user-error' when `paste-image-directory' is not a safe relative
path contained inside ROOT."
  (let ((name paste-image-directory))
    (unless (and (stringp name)
                 (not (string-empty-p name)))
      (user-error "`paste-image-directory' must be a non-empty string"))
    (when (or (file-name-absolute-p name)
              (string-prefix-p "~" name)
              (member ".." (split-string name "[/\\\\]+" t)))
      (user-error
       "`paste-image-directory' must be relative and may not contain `..'"))
    (let ((directory
           (file-name-as-directory
            (expand-file-name name root))))
      (unless (or (equal directory root)
                  (file-in-directory-p directory root))
        (user-error
         "`paste-image-directory' must remain inside the selected root"))
      directory)))

(defun paste-image--missing-directories (directory root)
  "Return missing directories from DIRECTORY upward toward ROOT.

The returned list is ordered from the deepest directory to the
shallowest.  ROOT itself is never included."
  (let ((current (directory-file-name directory))
        (root (directory-file-name root))
        missing)
    (while (and (not (equal current root))
                (not (file-directory-p current)))
      (push current missing)
      (let ((parent
             (directory-file-name
              (file-name-directory current))))
        (if (equal parent current)
            (setq current root)
          (setq current parent))))
    (nreverse missing)))

(defun paste-image--delete-empty-directories (directories)
  "Delete each empty directory in DIRECTORIES, ignoring failures."
  (dolist (directory directories)
    (ignore-errors
      (delete-directory directory))))

(defun paste-image--read-text-file (file)
  "Return the contents of FILE as trimmed text."
  (if (and file (file-readable-p file))
      (with-temp-buffer
        (insert-file-contents file)
        (string-trim (buffer-string)))
    ""))

(defun paste-image--diagnostic-text (text)
  "Normalize and truncate diagnostic TEXT for an error message."
  (when (and text (not (string-empty-p text)))
    (truncate-string-to-width
     (replace-regexp-in-string
      "[[:space:]\n\r]+" " " (string-trim text))
     300 nil nil "...")))

(defun paste-image--run-process (program arguments)
  "Run PROGRAM with ARGUMENTS and return a result property list.

Standard output is captured as text.  Standard error is captured
separately."
  (let ((stderr-file (make-temp-file "paste-image-stderr-"))
        result)
    (unwind-protect
        (setq result
              (condition-case err
                  (with-temp-buffer
                    (let ((coding-system-for-read 'utf-8-unix)
                          (coding-system-for-write 'utf-8-unix)
                          (process-connection-type nil))
                      (let ((status
                             (apply #'call-process
                                    program
                                    nil
                                    (list (current-buffer) stderr-file)
                                    nil
                                    arguments)))
                        (list :status status
                              :stderr
                              (paste-image--read-text-file stderr-file)
                              :output (buffer-string)))))
                (error
                 (list :error (error-message-string err)))))
      (ignore-errors
        (delete-file stderr-file)))
    result))

(defun paste-image--call-process-to-file (program arguments file)
  "Run PROGRAM with ARGUMENTS and write its binary output to FILE.

Return a process result property list containing `:ok' on success.
The process output remains in one unibyte buffer and is written directly
from that buffer, avoiding extra full-size string copies."
  (let ((stderr-file (make-temp-file "paste-image-stderr-"))
        result)
    (unwind-protect
        (setq result
              (condition-case err
                  (with-temp-buffer
                    (set-buffer-multibyte nil)
                    (let ((coding-system-for-read 'binary)
                          (coding-system-for-write 'utf-8-unix)
                          (process-connection-type nil))
                      (let* ((status
                              (apply #'call-process
                                     program
                                     nil
                                     (list (current-buffer) stderr-file)
                                     nil
                                     arguments))
                             (result
                              (list :status status
                                    :stderr
                                    (paste-image--read-text-file stderr-file)
                                    :binary-output t)))
                        (cond
                         ((not (and (integerp status)
                                    (zerop status)))
                          result)
                         ((zerop (buffer-size))
                          (plist-put result :reason "produced no output"))
                         (t
                          (let ((coding-system-for-write 'binary))
                            (write-region
                             (point-min) (point-max) file nil 'silent))
                          (plist-put result :ok t))))))
                (error
                 (list :error (error-message-string err)
                       :binary-output t))))
      (ignore-errors
        (delete-file stderr-file)))
    result))

(defun paste-image--call-process-writing-file (program arguments)
  "Run PROGRAM with ARGUMENTS when it writes the image itself.

Return a process result property list containing `:ok' on success."
  (let* ((result
          (paste-image--run-process program arguments))
         (status (plist-get result :status)))
    (if (and (integerp status) (zerop status))
        (plist-put result :ok t)
      result)))

(defun paste-image--png-signature-p (file)
  "Return non-nil when FILE starts with the PNG signature."
  (condition-case nil
      (let ((attributes (file-attributes file)))
        (and attributes
             (file-regular-p file)
             (>= (file-attribute-size attributes) 8)
             (with-temp-buffer
               (set-buffer-multibyte nil)
               (insert-file-contents-literally file nil 0 8)
               (equal (buffer-string)
                      (string-make-unibyte
                       "\x89PNG\r\n\x1a\n")))))
    (file-error nil)))

(defun paste-image--powershell-quote (string)
  "Quote STRING as a PowerShell single-quoted string."
  (concat
   "'"
   (replace-regexp-in-string "'" "''" string t t)
   "'"))

(defun paste-image--powershell-script (file)
  "Return a PowerShell script that saves the clipboard image to FILE."
  (format
   (concat
    "Add-Type -AssemblyName System.Windows.Forms; "
    "Add-Type -AssemblyName System.Drawing; "
    "if (-not [System.Windows.Forms.Clipboard]::ContainsImage()) "
    "{ exit 1 }; "
    "$image = [System.Windows.Forms.Clipboard]::GetImage(); "
    "try { "
    "$image.Save(%s, [System.Drawing.Imaging.ImageFormat]::Png) "
    "} finally { "
    "$image.Dispose() "
    "}")
   (paste-image--powershell-quote file)))

(defun paste-image--backends (file)
  "Return available clipboard backend specifications for FILE."
  (let ((pngpaste
         (and (eq system-type 'darwin)
              (executable-find "pngpaste")))
        (wl-paste
         (and (getenv "WAYLAND_DISPLAY")
              (executable-find "wl-paste")))
        (xclip
         (and (getenv "DISPLAY")
              (executable-find "xclip")))
        (powershell
         (and (eq system-type 'windows-nt)
              (or (executable-find "pwsh.exe")
                  (executable-find "powershell.exe")
                  (executable-find "pwsh")
                  (executable-find "powershell")))))
    (delq
     nil
     (list
      (when pngpaste
        (list :name "pngpaste"
              :kind 'direct
              :program pngpaste
              :arguments (list file)))
      (when wl-paste
        (list :name "wl-paste"
              :kind 'stdout
              :program wl-paste
              :arguments '("--no-newline" "--type" "image/png")))
      (when xclip
        (list :name "xclip"
              :kind 'stdout
              :program xclip
              :arguments '("-selection" "clipboard"
                           "-t" "image/png"
                           "-o")))
      (when powershell
        (list :name "PowerShell"
              :kind 'direct
              :program powershell
              :arguments
              (list "-NoProfile"
                    "-NonInteractive"
                    "-STA"
                    "-Command"
                    (paste-image--powershell-script file))))))))

(defun paste-image--run-backend (backend file)
  "Run BACKEND to save a clipboard image to FILE.

Return a result property list.  A successful result contains `:ok'."
  (let* ((kind (plist-get backend :kind))
         (program (plist-get backend :program))
         (arguments (plist-get backend :arguments))
         (result
          (pcase kind
            ('stdout
             (paste-image--call-process-to-file
              program arguments file))
            ('direct
             (paste-image--call-process-writing-file
              program arguments))
            (_
             (list :error
                   (format "Unsupported backend kind: %S" kind))))))
    (when (plist-get result :ok)
      (unless (paste-image--png-signature-p file)
        (setq result
              (plist-put
               result :reason
               "did not produce data with a valid PNG signature"))
        (setq result (plist-put result :ok nil))))
    result))

(defun paste-image--describe-backend-failure (backend result)
  "Return a readable failure description for BACKEND and RESULT."
  (let* ((name (plist-get backend :name))
         (status (plist-get result :status))
         (error-text
          (paste-image--diagnostic-text
           (plist-get result :error)))
         (reason
          (paste-image--diagnostic-text
           (plist-get result :reason)))
         (stderr
          (paste-image--diagnostic-text
           (plist-get result :stderr)))
         (stdout
          (unless (plist-get result :binary-output)
            (paste-image--diagnostic-text
             (plist-get result :output))))
         (parts
          (delq
           nil
           (list
            error-text
            reason
            (cond
             ((integerp status)
              (unless (zerop status)
                (format "exited with status %d" status)))
             ((stringp status)
              (format "terminated: %s" status)))
            (when stderr
              (format "stderr: %s" stderr))
            (when stdout
              (format "stdout: %s" stdout))))))
    (format "%s: %s"
            name
            (if parts
                (string-join parts "; ")
              "unknown failure"))))

(defun paste-image--missing-backend-message ()
  "Return a platform-appropriate missing-backend error message."
  (pcase system-type
    ('darwin
     "No supported clipboard image backend is available; install pngpaste")
    ('windows-nt
     (concat
      "No supported clipboard image backend is available; "
      "install or enable PowerShell"))
    (_
     (concat
      "No supported clipboard image backend is available for this session; "
      "install wl-clipboard for Wayland or xclip for X11, and ensure "
      "WAYLAND_DISPLAY or DISPLAY is set"))))

(defun paste-image--save-clipboard (file)
  "Save the clipboard image as PNG to the reserved path FILE.

Try each usable backend in order.  Each backend writes inside a unique
staging directory.  On success, atomically replace the reserved FILE
with the staged image.  Signal a detailed `user-error' if none succeeds."
  (unless (paste-image--backends file)
    (user-error "%s" (paste-image--missing-backend-message)))
  (let* ((staging-directory
          (make-temp-file
           (expand-file-name
            ".paste-image-"
            (file-name-directory file))
           t))
         (staging-file
          (expand-file-name "image.png" staging-directory))
         (backends (paste-image--backends staging-file))
         failures)
    (unwind-protect
        (catch 'saved
          (dolist (backend backends)
            (when (file-exists-p staging-file)
              (delete-file staging-file))
            (let ((result
                   (paste-image--run-backend backend staging-file)))
              (if (plist-get result :ok)
                  (progn
                    (rename-file staging-file file t)
                    (throw 'saved t))
                (push
                 (paste-image--describe-backend-failure backend result)
                 failures))))
          (user-error
           "Could not save a PNG from the clipboard. Backend attempts: %s"
           (string-join (nreverse failures) " | ")))
      (ignore-errors
        (delete-directory staging-directory t)))))

(defun paste-image--reserve-filename (directory)
  "Atomically reserve a timestamped PNG filename inside DIRECTORY."
  (make-temp-file
   (expand-file-name
    (concat
     (format-time-string paste-image-timestamp-format)
     "-")
    directory)
   nil
   ".png"))

(defun paste-image--org-link-path (path)
  "Escape PATH for use inside an Org file link."
  (let ((result
         (replace-regexp-in-string "%" "%25" path t t)))
    (setq result
          (replace-regexp-in-string
           (regexp-quote "[") "%5B" result t t))
    (replace-regexp-in-string
     (regexp-quote "]") "%5D" result t t)))

(defun paste-image--markdown-link-path (path)
  "Escape PATH for a Markdown angle-bracket link destination."
  (let ((result
         (replace-regexp-in-string
          (regexp-quote "\\") "\\\\" path t t)))
    (setq result
          (replace-regexp-in-string
           (regexp-quote "<") "\\<" result t t))
    (replace-regexp-in-string
     (regexp-quote ">") "\\>" result t t)))

(defun paste-image--insert-link (file base-directory)
  "Insert a link to FILE relative to BASE-DIRECTORY."
  (let ((relative-file
         (file-relative-name file base-directory)))
    (cond
     ((derived-mode-p 'org-mode)
      (insert
       (format
        "[[file:%s]]"
        (paste-image--org-link-path relative-file))))
     ((derived-mode-p 'markdown-mode 'gfm-mode)
      (insert
       (format
        "![](<%s>)"
        (paste-image--markdown-link-path relative-file))))
     (t
      (insert relative-file)))))

;;;###autoload
(defun paste-image (&optional save-only)
  "Save the clipboard image as a timestamped PNG.

Images are saved under `paste-image-directory' inside the current
project root.

When there is no project, use the directory containing the current
file.  For an unsaved buffer, use `default-directory'.

Insert an Org link, Markdown image, or relative filename at point.  The
link is relative to the current document directory.

With prefix argument SAVE-ONLY, save the image without inserting a
link.  Remote directories are not supported."
  (interactive "P")
  (unless save-only
    (barf-if-buffer-read-only))
  (let* ((root (paste-image--root))
         (link-base (paste-image--link-base))
         directory
         created-directories
         file
         completed)
    (when (or (file-remote-p root)
              (and (not save-only)
                   (file-remote-p link-base)))
      (user-error
       "Saving clipboard images to remote directories is unsupported"))
    (setq directory (paste-image--image-directory root))
    (setq created-directories
          (paste-image--missing-directories directory root))
    (unwind-protect
        (progn
          (make-directory directory t)
          (setq file (paste-image--reserve-filename directory))
          (paste-image--save-clipboard file)
          (unless save-only
            (atomic-change-group
              (paste-image--insert-link file link-base)))
          (setq completed t)
          (message
           "Saved clipboard image: %s"
           (file-relative-name file root)))
      (unless completed
        (when (and file (file-exists-p file))
          (ignore-errors
            (delete-file file)))
        (paste-image--delete-empty-directories created-directories)))))

(provide 'paste-image)

;;; paste-image.el ends here
