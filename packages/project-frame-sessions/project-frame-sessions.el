;;; project-frame-sessions.el --- Persistent frame workspaces -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; Author: project-frame-sessions contributors
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, frames, sessions

;;; Commentary:

;; Save one explicitly enrolled graphical frame as a named session.  Session
;; identity is independent of `project.el'; project discovery is optional and is
;; used to suggest a root and name.  Restore completion also appends remembered
;; local projects that do not yet have a saved session; selecting one opens its
;; root in Dired and enrolls that frame transactionally.  Desktop saves buffers,
;; and Frameset saves the complete tab-bar workspace.  Tabspaces' frame-local
;; buffer lists are represented by the built-in tab parameters and are therefore
;; preserved without copying Tabspaces globals.  Frame buffers are preserved on
;; close by default; optional cleanup only happens after a successful save and
;; deletion and never kills buffers shared with another frame.
;;
;; While the global mode is enabled, package snapshots can also restart Eshell
;; buffers in their saved local or Tramp directories and resume the exact JSONL
;; session used by pi-coding-agent chat/input buffers.  Pi and Eat are optional.
;; Eshell restoration starts a fresh process and runs normal hooks, including a
;; user's Eat hook; process state and terminal transcript are not restored.  Utility
;; tabs and buffers must not be excluded by user tab/Desktop predicates.
;;
;; `project-frame-sessions-directory' must be local and trusted: Desktop files
;; are executable Lisp when restored.  Metadata is parsed with `read-eval' nil.

;;; Code:

(require 'cl-lib)
(require 'desktop)
(require 'frameset)
(require 'subr-x)
(require 'tab-bar)

(defvar read-eval)
(defvar warning-minimum-level)
(defvar window-state-change-functions)
(defvar tab-bar-tab-post-open-functions)
(defvar tab-bar-tab-post-select-functions)
(defvar write-region-inhibit-fsync)
(defvar eshell-buffer-name)
(defvar eshell-mode-hook)
(defvar pi-coding-agent--chat-buffer)
(defvar pi-coding-agent--input-buffer)
(defvar pi-coding-agent--state)
(defvar pi-coding-agent--canonical-session-directory)

(declare-function project-root "project" (project))
(declare-function project-known-project-roots "project" ())
(declare-function project-name "project" (project))
(declare-function project-current "project" (&optional maybe-prompt directory))
(declare-function dired "dired" (dirname &optional switches))
(declare-function kill-buffer--possibly-save "simple" (buffer))
(declare-function tabspaces--buffer-list "tabspaces" (&optional frame tabnum))
(declare-function pi-coding-agent-open-session-file "pi-coding-agent" (session-file))
(declare-function pi-coding-agent--setup-session "pi-coding-agent" (dir &optional session))
(declare-function eshell "esh-mode" (&optional arg))

(defgroup project-frame-sessions nil
  "Persistent named frame sessions."
  :group 'frames)

(defcustom project-frame-sessions-directory
  (expand-file-name "project-frame-sessions/" user-emacs-directory)
  "Trusted local directory containing frame sessions."
  :type 'directory)

(defcustom project-frame-sessions-discovery-function
  #'project-frame-sessions--default-discovery
  "Function called in a frame to discover an optional local root.
It may return a directory name or nil.  The default uses `project.el' only when
that library is available."
  :type 'function)

(defcustom project-frame-sessions-frame-buffer-function
  (lambda (frame) (frame-parameter frame 'buffer-list))
  "Function returning buffers associated with FRAME.
Buffers associated with non-current tabs are added automatically."
  :type 'function)

(defcustom project-frame-sessions-ignored-buffer-name-regexp nil
  "Regexp matching buffer names that should not be persisted.
Nil means that no buffer is ignored by this package.  Desktop's own buffer
exclusion options continue to apply independently."
  :type '(choice (const :tag "None" nil) regexp))

(defcustom project-frame-sessions-kill-buffers-on-switch nil
  "When non-nil, kill outgoing buffers after a current-frame switch.
Only buffers belonging to the outgoing workspace and not associated with
another live frame are killed.  Modified buffers require confirmation.  When
nil, outgoing buffers remain live but are excluded from the destination
session until explicitly displayed or associated with that frame again.
This option does not control cleanup when a frame is closed; see
`project-frame-sessions-preserve-buffers-after-frame-close'."
  :type 'boolean)

(defcustom project-frame-sessions-preserve-buffers-after-frame-close t
  "When non-nil, preserve session buffers after an enrolled frame closes.
When nil, buffers belonging only to the closed frame are killed after its
session was saved and deletion was confirmed.  Buffers used by another live
frame are always preserved.  This option does not affect current-frame session
switches, which are controlled by
`project-frame-sessions-kill-buffers-on-switch'."
  :type 'boolean)

(defcustom project-frame-sessions-ignored-tab-name-regexp nil
  "Regexp matching names of tabs that should not be persisted.
Nil means that no tab is ignored by name."
  :type '(choice (const :tag "None" nil) regexp))

(defcustom project-frame-sessions-tab-omit-function nil
  "Optional additional predicate for tabs that should not be persisted.
The function receives one tab alist.  A tab is omitted when either this
predicate returns non-nil or its name matches
`project-frame-sessions-ignored-tab-name-regexp'."
  :type '(choice (const nil) function))

(defcustom project-frame-sessions-post-restore-function nil
  "Optional function called with FRAME after a successful restore."
  :type '(choice (const nil) function))

(defcustom project-frame-sessions-autosave-interval 300
  "Idle fallback interval in seconds, or nil to disable it.
Only dirty enrolled sessions are saved.  An unmanaged frame with a discoverable
project may be enrolled by this fallback; an ordinary frame is never enrolled."
  :type '(choice (const :tag "Disabled" nil)
                 (number :tag "Positive seconds")))

(defcustom project-frame-sessions-debounce-delay 2
  "Nonnegative seconds to debounce high-frequency workspace changes."
  :type 'number)

(defcustom project-frame-sessions-maximum-dirty-age 60
  "Nonnegative maximum seconds a dirty session may wait before a save attempt."
  :type 'number)

(defcustom project-frame-sessions-retry-delays '(5 15 45 120 300 600)
  "Autosave retry delays in seconds, or nil to disable automatic retry.
Every delay must be nonnegative.  The final value is reused after all preceding
values have been used."
  :type '(repeat number))

(defcustom project-frame-sessions-ownerless-lock-stale-seconds 5
  "Nonnegative seconds before an ownerless transaction lock is considered stale.
This grace period prevents another Emacs process from reclaiming the lock in
between its atomic directory creation and owner metadata publication."
  :type 'number)

(defcustom project-frame-sessions-warn-function #'display-warning
  "Function used to report visible warnings."
  :type 'function)

(defcustom project-frame-sessions-show-status nil
  "When non-nil, manual commands include concise session status messages."
  :type 'boolean)

(defconst project-frame-sessions--index-version 1)
(defconst project-frame-sessions--desktop-name "desktop.el")
(defconst project-frame-sessions--id-parameter 'project-frame-sessions-id)
(defconst project-frame-sessions--deleted-parameter 'project-frame-sessions-deleted)
(defconst project-frame-sessions--excluded-buffers-parameter
  'project-frame-sessions-excluded-buffers)
(defconst project-frame-sessions--generic-frame-names
  '("" "Emacs" "GNU Emacs" "emacs" "*scratch*"))
(defconst project-frame-sessions--desktop-state-variables
  '(desktop-dirname desktop-path desktop-base-file-name
    desktop-file-name-format desktop-restore-frames desktop-restore-eager
    desktop-missing-file-warning desktop-after-read-hook
    desktop-before-save-hook desktop-save-hook desktop-saved-frameset
    desktop-globals-to-save desktop-buffers-not-to-save
    desktop-buffers-not-to-save-function desktop-io-file-version
    desktop-file-modtime desktop-file-checksum desktop-save-buffer
    desktop-buffer-mode-handlers desktop-locals-to-save))

(cl-defstruct (project-frame-sessions--runtime
               (:constructor project-frame-sessions--make-runtime))
  (dirty 0) (saved 0) dirty-since saving (failures 0) next-retry timer
  last-success last-error last-warning)

(defvar project-frame-sessions-mode nil)
(defvar project-frame-sessions--runtimes (make-hash-table :test #'equal))
(defvar project-frame-sessions--pending (make-hash-table :test #'eq)
  "First-save entries keyed by frame until their initial commit succeeds.")
(defvar project-frame-sessions--idle-timer nil)
(defvar project-frame-sessions--shutdown nil)
(defvar project-frame-sessions--preflight-complete nil)
(defvar project-frame-sessions--delete-suppressed nil)
(defvar project-frame-sessions--pending-frame-closes (make-hash-table :test #'eq)
  "Frames whose delete hook has already saved and scheduled finalization.")
(defvar project-frame-sessions--recovery-warning-shown nil)
(defvar project-frame-sessions--inhibit-dirty nil)
(defvar project-frame-sessions--save-buffer-exclusions nil
  "Buffers dynamically excluded from the save currently in progress.")
(defvar project-frame-sessions--desktop-restored-buffers nil
  "Buffers restored by the dynamically active Desktop read.")
(defvar project-frame-sessions--utility-restore-cache nil
  "Dynamically bound utility-session cache for one Desktop read.")
(defvar project-frame-sessions--desktop-handler-registrations nil
  "Desktop handler alist cells installed by this package.")
(defvar-local project-frame-sessions--utility-identity nil
  "Stable identity serialized for this utility buffer.")
(defvar-local project-frame-sessions--prior-desktop-save-buffer nil
  "Value of `desktop-save-buffer' before package enrollment.")
(defvar-local project-frame-sessions--prior-desktop-save-buffer-local-p nil
  "Whether `desktop-save-buffer' was local before package enrollment.")
(defvar-local project-frame-sessions--desktop-save-buffer-owned nil
  "Non-nil when this package installed `desktop-save-buffer' here.")
(defvar project-frame-sessions--failpoint-function nil
  "Dynamically bound test callback called with transaction boundary symbols.")

(defun project-frame-sessions--failpoint (name)
  "Invoke the dynamically bound test failpoint NAME, when any."
  (when project-frame-sessions--failpoint-function
    (funcall project-frame-sessions--failpoint-function name)))

(defun project-frame-sessions--nonnegative-number-p (value)
  "Return non-nil when VALUE is a finite nonnegative number."
  (and (numberp value) (>= value 0)))

(defun project-frame-sessions--validate-configuration ()
  "Reject customization values that could create unsafe timer behavior."
  (unless (or (null project-frame-sessions-autosave-interval)
              (and (numberp project-frame-sessions-autosave-interval)
                   (> project-frame-sessions-autosave-interval 0)))
    (error "Project-frame-sessions-autosave-interval must be nil or positive, not %S"
           project-frame-sessions-autosave-interval))
  (dolist (pair `((project-frame-sessions-debounce-delay
                   . ,project-frame-sessions-debounce-delay)
                  (project-frame-sessions-maximum-dirty-age
                   . ,project-frame-sessions-maximum-dirty-age)
                  (project-frame-sessions-ownerless-lock-stale-seconds
                   . ,project-frame-sessions-ownerless-lock-stale-seconds)))
    (unless (project-frame-sessions--nonnegative-number-p (cdr pair))
      (error "Value of %s must be a nonnegative number, not %S"
             (car pair) (cdr pair))))
  (unless (or (null project-frame-sessions-retry-delays)
              (and (consp project-frame-sessions-retry-delays)
                   (cl-every #'project-frame-sessions--nonnegative-number-p
                             project-frame-sessions-retry-delays)))
    (error "Project-frame-sessions-retry-delays must be nil or a nonempty list of nonnegative numbers, not %S"
           project-frame-sessions-retry-delays))
  t)

(defun project-frame-sessions--warn (format-string &rest args)
  "Issue a package warning made from FORMAT-STRING and ARGS."
  (funcall project-frame-sessions-warn-function
           'project-frame-sessions (apply #'format format-string args) :warning))

(defun project-frame-sessions--canonical-root (root)
  "Return canonical local ROOT, or nil."
  (when root
    (unless (and (stringp root) (file-name-absolute-p root))
      (user-error "Session root must be an absolute directory: %S" root))
    (when (file-remote-p root)
      (user-error "Remote session roots are not supported: %s" root))
    (file-name-as-directory
     (condition-case nil (file-truename root) (error (expand-file-name root))))))

(defun project-frame-sessions--default-discovery ()
  "Discover a project root without making `project.el' mandatory."
  (when (require 'project nil t)
    (when-let* ((project (project-current nil)))
      (project-root project))))

(defun project-frame-sessions--discover-root (&optional frame)
  "Discover FRAME's optional local root."
  (let ((frame (or frame (selected-frame))))
    (when-let* ((root (with-selected-frame frame
                       (funcall project-frame-sessions-discovery-function))))
      (project-frame-sessions--canonical-root root))))

(defun project-frame-sessions--graphical-frame-p (frame)
  "Return non-nil if FRAME is a savable top-level graphical frame.
Frames intentionally excluded from Desktop and child frames, such as completion
popups, cannot own persistent frame sessions."
  (and (frame-live-p frame) (display-graphic-p frame)
       (not (eq frame terminal-frame))
       (not (frame-parameter frame 'desktop-dont-save))
       (not (frame-parent frame))))

(defun project-frame-sessions--frame-id (&optional frame)
  "Return the stable session ID attached to FRAME."
  (frame-parameter (or frame (selected-frame))
                   project-frame-sessions--id-parameter))

(defun project-frame-sessions--valid-id-p (id)
  "Return non-nil when ID is a package session ID."
  (and (stringp id) (string-match-p "\\`[0-9a-f]\\{64\\}\\'" id)))

(defun project-frame-sessions--new-id ()
  "Generate a stable, unpredictable session ID."
  (secure-hash
   'sha256
   (format "%s:%s:%s:%s:%s" (float-time) (emacs-pid) (system-name)
           (random most-positive-fixnum) (make-temp-name "pfs"))))

(defun project-frame-sessions--valid-name-p (name)
  "Return non-nil when NAME is a usable session name."
  (and (stringp name) (not (string-empty-p (string-trim name)))))

(defun project-frame-sessions--index-file ()
  "Return the package index file."
  (expand-file-name "index.eld" project-frame-sessions-directory))
(defun project-frame-sessions--sessions-directory ()
  "Return the active session storage directory."
  (expand-file-name "sessions/" project-frame-sessions-directory))
(defun project-frame-sessions--trash-directory ()
  "Return the recoverable trash directory."
  (expand-file-name "trash/" project-frame-sessions-directory))
(defun project-frame-sessions--recovery-directory ()
  "Return the interrupted-transaction recovery directory."
  (expand-file-name "recovery/" project-frame-sessions-directory))
(defun project-frame-sessions--lock-directory ()
  "Return the transaction lock directory."
  (expand-file-name ".transaction-lock/" project-frame-sessions-directory))

(defun project-frame-sessions--assert-local-path (path)
  "Return expanded PATH after requiring an absolute local file name."
  (unless (and (stringp path) (file-name-absolute-p path))
    (error "Managed path must be absolute: %S" path))
  (when (file-remote-p path)
    (error "Managed path must be local: %s" path))
  (expand-file-name path))

(defun project-frame-sessions--assert-safe-target-ancestry (path &optional stop)
  "Reject symlinks in existing components of PATH through STOP.
STOP defaults to the filesystem root.  Dangling symlinks are rejected too."
  (let ((cursor (directory-file-name
                 (project-frame-sessions--assert-local-path path)))
        (stop (and stop (directory-file-name (expand-file-name stop))))
        done)
    (while (not done)
      (when (file-symlink-p cursor)
        (error "Managed path component is a symlink: %s" cursor))
      (setq done (or (and stop (equal cursor stop))
                     (equal cursor (directory-file-name
                                    (file-name-directory cursor)))))
      (unless done
        (setq cursor (directory-file-name (file-name-directory cursor)))))
    (when (and stop (not (equal cursor stop)))
      (error "Managed path is outside expected ancestry: %s" path))
    path))

(defun project-frame-sessions--validate-store ()
  "Validate and return the configured local package store."
  (let ((store (file-name-as-directory
                (project-frame-sessions--assert-local-path
                 project-frame-sessions-directory))))
    (project-frame-sessions--assert-safe-target-ancestry store)
    (when (and (file-exists-p store) (not (file-directory-p store)))
      (error "Session store is not a directory: %s" store))
    store))

(defun project-frame-sessions--assert-contained-path (path parent)
  "Return expanded PATH after proving it is contained by PARENT."
  (let* ((path (project-frame-sessions--assert-local-path path))
         (parent (file-name-as-directory
                  (project-frame-sessions--assert-local-path parent)))
         (relative (file-relative-name path parent)))
    (when (or (file-name-absolute-p relative)
              (equal relative "..")
              (string-prefix-p "../" relative))
      (error "Managed path %s escapes %s" path parent))
    (project-frame-sessions--assert-safe-target-ancestry path parent)
    path))

(defun project-frame-sessions--assert-real-directory (path &optional parent)
  "Require existing PATH to be a real directory contained by PARENT."
  (when parent (project-frame-sessions--assert-contained-path path parent))
  (project-frame-sessions--assert-safe-target-ancestry path parent)
  (unless (and (file-directory-p path) (not (file-symlink-p path)))
    (error "Managed path is not a real directory: %s" path))
  path)

(defun project-frame-sessions--assert-safe-managed-root (path)
  "Validate managed root PATH beneath the configured store when it exists."
  (let ((store (project-frame-sessions--validate-store)))
    (project-frame-sessions--assert-contained-path path store)
    (when (or (file-exists-p path) (file-symlink-p path))
      (project-frame-sessions--assert-real-directory path store))
    path))

(defun project-frame-sessions--assert-safe-managed-file (file)
  "Validate FILE ancestry and reject an existing non-regular or symlink file."
  (let ((store (project-frame-sessions--validate-store)))
    (project-frame-sessions--assert-contained-path file store)
    (when (or (file-exists-p file) (file-symlink-p file))
      (unless (and (file-regular-p file) (not (file-symlink-p file)))
        (error "Managed file is not a real regular file: %s" file)))
    file))

(defun project-frame-sessions--validate-managed-roots ()
  "Validate all existing package-owned roots."
  (project-frame-sessions--validate-store)
  (dolist (root (list (project-frame-sessions--sessions-directory)
                      (project-frame-sessions--trash-directory)
                      (project-frame-sessions--recovery-directory)
                      (project-frame-sessions--lock-directory)))
    (project-frame-sessions--assert-safe-managed-root root))
  t)

(defun project-frame-sessions--read-object (file)
  "Read exactly one Lisp object from safe regular FILE without evaluation."
  (project-frame-sessions--assert-safe-managed-file file)
  (with-temp-buffer
    (insert-file-contents file)
    (let ((read-eval nil) (object (read (current-buffer))))
      (skip-chars-forward " \t\r\n")
      (unless (eobp) (error "Trailing data in %s" file))
      object)))

(defun project-frame-sessions--atomic-write (file object)
  "Atomically write OBJECT to safe managed FILE with mode 0600."
  (project-frame-sessions--assert-safe-managed-file file)
  (project-frame-sessions--assert-safe-target-ancestry
   (file-name-directory file) (project-frame-sessions--validate-store))
  (make-directory (file-name-directory file) t)
  (project-frame-sessions--assert-safe-managed-file file)
  (let ((temporary (make-temp-file
                    (expand-file-name ".write-" (file-name-directory file)))))
    (unwind-protect
        (progn
          (set-file-modes temporary #o600)
          (let ((write-region-inhibit-fsync nil))
            (with-temp-file temporary
              (let ((print-length nil) (print-level nil))
                (prin1 object (current-buffer))
                (insert "\n"))))
          (rename-file temporary file t)
          (set-file-modes file #o600))
      (when (file-exists-p temporary) (delete-file temporary)))))

(defun project-frame-sessions--entry-snapshot-file (entry)
  "Return the absolute snapshot file for ENTRY."
  (expand-file-name (plist-get entry :snapshot)
                    project-frame-sessions-directory))

(defun project-frame-sessions--valid-snapshot-p (snapshot id)
  "Return non-nil when SNAPSHOT is a safe relative path for ID."
  (and (stringp snapshot)
       (string-match-p
        (format "\\`sessions/%s/desktop-[0-9a-f]\\{16\\}\\.el\\'"
                (regexp-quote id))
        snapshot)))

(defun project-frame-sessions--valid-entry-p (entry)
  "Return non-nil when ENTRY conforms to the current schema."
  (let ((id (plist-get entry :id))
        (name (plist-get entry :name))
        (root (plist-get entry :root))
        (snapshot (plist-get entry :snapshot)))
    (and (listp entry)
         (project-frame-sessions--valid-id-p id)
         (project-frame-sessions--valid-name-p name)
         (or (null root)
             (and (stringp root) (file-name-absolute-p root)
                  (not (file-remote-p root))
                  (equal root (project-frame-sessions--canonical-root root))))
         (project-frame-sessions--valid-snapshot-p snapshot id)
         (numberp (plist-get entry :saved-at))
         (eq (plist-get entry :state) 'active))))

(defun project-frame-sessions--read-index-data ()
  "Read and strictly validate the current index.
A corrupt index is never treated as an empty index."
  (project-frame-sessions--validate-managed-roots)
  (let ((file (project-frame-sessions--index-file)))
    (if (not (file-exists-p file))
        (list :version project-frame-sessions--index-version :entries nil)
      (condition-case err
          (let* ((data (project-frame-sessions--read-object file))
                 (version (plist-get data :version))
                 (entries (plist-get data :entries))
                 ids names)
            (unless (and (= version project-frame-sessions--index-version)
                         (listp entries))
              (error "Invalid index schema"))
            (dolist (entry entries)
              (unless (project-frame-sessions--valid-entry-p entry)
                (error "Invalid session entry"))
              (let ((id (plist-get entry :id))
                    (name (plist-get entry :name)))
                (when (or (member id ids) (member name names))
                  (error "Duplicate session ID or name"))
                (push id ids) (push name names)))
            data)
        (error
         (error "Refusing to overwrite corrupt session index %s: %s"
                file (error-message-string err)))))))

(defun project-frame-sessions--read-index ()
  "Return validated active index entries."
  (plist-get (project-frame-sessions--read-index-data) :entries))

(defun project-frame-sessions--write-index (entries)
  "Write validated active ENTRIES."
  (project-frame-sessions--validate-managed-roots)
  (let ((data (list :version project-frame-sessions--index-version
                    :entries entries)))
    ;; Validate the in-memory object by the same essential invariants first.
    (let (ids names)
      (dolist (entry entries)
        (unless (project-frame-sessions--valid-entry-p entry)
          (error "Refusing to write invalid session entry"))
        (when (or (member (plist-get entry :id) ids)
                  (member (plist-get entry :name) names))
          (error "Session names and IDs must be unique"))
        (push (plist-get entry :id) ids)
        (push (plist-get entry :name) names)))
    (project-frame-sessions--atomic-write
     (project-frame-sessions--index-file) data)))

(defun project-frame-sessions--find-entry (id &optional entries)
  "Find session ID in ENTRIES or the active index."
  (cl-find id (or entries (project-frame-sessions--read-index))
           :key (lambda (entry) (plist-get entry :id)) :test #'equal))

(defun project-frame-sessions--find-entry-by-name (name &optional entries)
  "Find session NAME in ENTRIES or the active index."
  (cl-find name (or entries (project-frame-sessions--read-index))
           :key (lambda (entry) (plist-get entry :name)) :test #'equal))

(defun project-frame-sessions--frame-entry (&optional frame)
  "Return the active entry attached to FRAME, if any."
  (when-let* ((id (project-frame-sessions--frame-id frame)))
    (project-frame-sessions--find-entry id)))

(defun project-frame-sessions--entry-name (id)
  "Return the display name for ID."
  (or (when-let* ((entry (project-frame-sessions--find-entry id)))
        (plist-get entry :name))
      id))

(defun project-frame-sessions--lock-owner-file (&optional lock)
  "Return the owner metadata file for LOCK or the published lock."
  (expand-file-name "owner.eld" (or lock (project-frame-sessions--lock-directory))))

(defun project-frame-sessions--owner-live-p (owner)
  "Return non-nil if lock OWNER should be considered live."
  (let ((pid (plist-get owner :pid)) (host (plist-get owner :host)))
    (and (integerp pid) (stringp host)
         (if (equal host (system-name)) (process-attributes pid) t))))

(defun project-frame-sessions--ownerless-lock-stale-p (lock)
  "Return non-nil when ownerless LOCK is old enough to reclaim safely."
  (project-frame-sessions--validate-configuration)
  (project-frame-sessions--assert-safe-managed-root lock)
  (when-let* ((attributes (file-attributes lock 'integer))
              (modified (file-attribute-modification-time attributes)))
    (>= (- (float-time) (float-time modified))
        project-frame-sessions-ownerless-lock-stale-seconds)))

(defun project-frame-sessions--acquire-lock ()
  "Acquire the package transaction lock and return its directory."
  (project-frame-sessions--validate-configuration)
  (project-frame-sessions--validate-managed-roots)
  (make-directory project-frame-sessions-directory t)
  (project-frame-sessions--validate-managed-roots)
  (let ((lock (project-frame-sessions--lock-directory)))
    (condition-case nil
        (make-directory lock)
      (file-already-exists
       (let ((owner (ignore-errors
                      (project-frame-sessions--read-object
                       (project-frame-sessions--lock-owner-file lock)))))
         (cond
          ((and owner (project-frame-sessions--owner-live-p owner))
           (error "Session transaction is locked by PID %s on %s"
                  (plist-get owner :pid) (plist-get owner :host)))
          ((or owner (project-frame-sessions--ownerless-lock-stale-p lock))
           (when (file-symlink-p lock)
             (error "Refusing symlink transaction lock"))
           (delete-directory lock t)
           (make-directory lock))
          (t
           (error "Session transaction lock is being initialized"))))))
    (project-frame-sessions--assert-safe-managed-root lock)
    (project-frame-sessions--atomic-write
     (project-frame-sessions--lock-owner-file lock)
     (list :pid (emacs-pid) :host (system-name) :at (float-time)))
    lock))

(defmacro project-frame-sessions--with-lock (&rest body)
  "Execute BODY while holding the package transaction lock."
  (declare (indent 0) (debug t))
  `(let ((lock (project-frame-sessions--acquire-lock)))
     (unwind-protect
         (progn
           (project-frame-sessions--validate-managed-roots)
           (project-frame-sessions--failpoint 'after-lock-acquisition)
           ,@body)
       (when (and (file-directory-p lock) (not (file-symlink-p lock)))
         (project-frame-sessions--assert-safe-managed-root lock)
         (delete-directory lock t)))))

(defun project-frame-sessions--safe-direct-child-p (path parent regexp)
  "Return non-nil when PATH is a safe direct child of PARENT matching REGEXP."
  (condition-case nil
      (let* ((parent (file-name-as-directory
                      (project-frame-sessions--assert-local-path parent)))
             (path (directory-file-name
                    (project-frame-sessions--assert-local-path path))))
        (and (equal (file-name-directory path) parent)
             (string-match-p regexp (file-name-nondirectory path))
             (not (file-symlink-p path))
             (progn (project-frame-sessions--assert-contained-path path parent) t)))
    (error nil)))

(defun project-frame-sessions--with-desktop-state (function)
  "Call FUNCTION with Desktop singleton state dynamically isolated."
  (let* ((variables (cl-remove-if-not
                     #'boundp project-frame-sessions--desktop-state-variables))
         (values (mapcar #'symbol-value variables)))
    (cl-progv variables values (funcall function))))

(defun project-frame-sessions--ignored-tab-p (tab)
  "Return non-nil when TAB is configured to be omitted from saves."
  (or (and project-frame-sessions-ignored-tab-name-regexp
           (when-let* ((name (alist-get 'name tab)))
             (string-match-p project-frame-sessions-ignored-tab-name-regexp
                             name)))
      (and project-frame-sessions-tab-omit-function
           (funcall project-frame-sessions-tab-omit-function tab))))

(defun project-frame-sessions--filter-tabs (current filtered parameters saving)
  "Sanitize CURRENT tabs and optionally omit tabs while SAVING.
FILTERED and PARAMETERS are passed through to `frameset-filter-tabs'."
  (let ((parameter (frameset-filter-tabs current filtered parameters saving)))
    (if (not (and saving
                  (or project-frame-sessions-ignored-tab-name-regexp
                      project-frame-sessions-tab-omit-function)))
        parameter
      ;; Frameset filters receive the complete (tabs . VALUE) parameter, not
      ;; VALUE alone.  Never pass its `tabs' key to an omission predicate.
      (let* ((kept (cl-remove-if #'project-frame-sessions--ignored-tab-p
                                 (cdr parameter)))
             (current-tab (cl-find-if
                           (lambda (tab)
                             (eq (car-safe tab) 'current-tab))
                           kept)))
        (when (and kept (not current-tab))
          (setcar kept (cons 'current-tab (cdar kept))))
        (cons (car parameter) kept)))))

(defun project-frame-sessions--tab-parameter-buffers (frame)
  "Return live buffers stored in FRAME's tab parameters."
  (let (buffers)
    (dolist (tab (frame-parameter frame 'tabs))
      (dolist (buffer (append (cdr (assq 'wc-bl tab))
                              (cdr (assq 'wc-bbl tab))))
        (when (buffer-live-p buffer) (cl-pushnew buffer buffers)))
      (when-let* ((workspace (assq 'ws tab))
                  (association (assq #'tabspaces--buffer-list workspace)))
        (dolist (item (car (cdr association)))
          (let ((buffer (if (bufferp item) item (get-buffer item))))
            (when (buffer-live-p buffer) (cl-pushnew buffer buffers))))))
    buffers))

(defun project-frame-sessions--active-workspace-buffers (frame)
  "Return buffers actively displayed or tab-associated with FRAME."
  (let ((buffers (project-frame-sessions--tab-parameter-buffers frame)))
    (dolist (window (window-list frame 'no-minibuffer))
      (when-let* ((buffer (window-buffer window)))
        (when (buffer-live-p buffer) (cl-pushnew buffer buffers))))
    buffers))

(defun project-frame-sessions--excluded-buffers (frame)
  "Return FRAME's live buffers excluded from session saves."
  (cl-delete-if-not
   #'buffer-live-p
   (copy-sequence
    (or (frame-parameter frame
                         project-frame-sessions--excluded-buffers-parameter)
        nil))))

(defun project-frame-sessions--set-excluded-buffers (frame buffers)
  "Set FRAME's save exclusions to live BUFFERS."
  (set-frame-parameter
   frame project-frame-sessions--excluded-buffers-parameter
   (cl-delete-if-not #'buffer-live-p
                     (cl-remove-duplicates buffers :test #'eq))))

(defun project-frame-sessions--tab-buffers (frame)
  "Return live buffers belonging to FRAME's session workspace.
Buffers explicitly displayed or associated after a switch are removed from the
frame's exclusion list.  Hidden tab and Tabspaces associations are included."
  (let* ((active (project-frame-sessions--active-workspace-buffers frame))
         (excluded (cl-set-difference
                    (project-frame-sessions--excluded-buffers frame)
                    active :test #'eq))
         (buffers (copy-sequence
                   (funcall project-frame-sessions-frame-buffer-function frame))))
    (project-frame-sessions--set-excluded-buffers frame excluded)
    (dolist (buffer (project-frame-sessions--tab-parameter-buffers frame))
      (cl-pushnew buffer buffers))
    (cl-set-difference
     (cl-delete-if-not #'buffer-live-p buffers)
     (append excluded project-frame-sessions--save-buffer-exclusions)
     :test #'eq)))

(defun project-frame-sessions--other-frame-buffers (frame)
  "Return live session buffers associated with frames other than FRAME."
  (let (buffers)
    (dolist (candidate (frame-list))
      (when (and (frame-live-p candidate) (not (eq candidate frame)))
        (dolist (buffer (project-frame-sessions--tab-buffers candidate))
          (cl-pushnew buffer buffers))))
    buffers))

(defun project-frame-sessions--frame-close-candidates (frame)
  "Return FRAME session buffers not currently used by another live frame."
  (cl-set-difference (project-frame-sessions--tab-buffers frame)
                     (project-frame-sessions--other-frame-buffers frame)
                     :test #'eq))

(defun project-frame-sessions--kill-outgoing-buffer (buffer)
  "Kill BUFFER, asking for confirmation first when it is modified."
  (when (and
         (buffer-live-p buffer)
         (or (not (buffer-modified-p buffer))
             (with-current-buffer buffer
               (if (fboundp 'kill-buffer--possibly-save)
                   (kill-buffer--possibly-save buffer)
                 (yes-or-no-p
                  (format "Buffer %s modified; kill anyway? "
                          (buffer-name buffer)))))))
    (kill-buffer buffer)))

(defun project-frame-sessions--dispose-closed-frame-buffers (buffers)
  "Kill safe BUFFERS after a frame close, rechecking live-frame sharing.
Failure or refusal to kill one buffer does not prevent processing the rest."
  (unless project-frame-sessions-preserve-buffers-after-frame-close
    (dolist (buffer buffers)
      (when (and (buffer-live-p buffer)
                 (not (memq buffer
                            (project-frame-sessions--other-frame-buffers nil))))
        (condition-case err
            (project-frame-sessions--kill-outgoing-buffer buffer)
          (quit
           (project-frame-sessions--warn
            "Did not kill frame buffer %s" (buffer-name buffer)))
          (error
           (project-frame-sessions--warn
            "Could not kill frame buffer %s: %s"
            (buffer-name buffer) (error-message-string err))))))))

(defun project-frame-sessions--finalize-frame-close (frame buffers)
  "Finish FRAME's pending close and dispose BUFFERS if it was deleted."
  (remhash frame project-frame-sessions--pending-frame-closes)
  (unless (frame-live-p frame)
    (project-frame-sessions--dispose-closed-frame-buffers buffers)))

(defun project-frame-sessions--complete-current-frame-switch
    (frame outgoing prior-excluded destination)
  "Finish FRAME's switch from OUTGOING buffers to DESTINATION buffers.
PRIOR-EXCLUDED is retained.  Destination buffers and buffers belonging to
other frames are never killed.  Buffers that remain live stay excluded from
future saves."
  (let* ((destination (cl-delete-if-not #'buffer-live-p destination))
         (excluded (cl-set-difference
                    (cl-remove-duplicates
                     (append prior-excluded outgoing) :test #'eq)
                    destination :test #'eq))
         (candidates (cl-set-difference outgoing destination :test #'eq)))
    ;; Install exclusions before asking questions, so quitting a confirmation
    ;; cannot accidentally enroll an outgoing buffer in the destination.
    (project-frame-sessions--set-excluded-buffers frame excluded)
    (when project-frame-sessions-kill-buffers-on-switch
      (let ((shared (project-frame-sessions--other-frame-buffers frame))
            aborted)
        (dolist (buffer candidates)
          (unless (or aborted (memq buffer shared))
            (condition-case err
                (project-frame-sessions--kill-outgoing-buffer buffer)
              (quit (setq aborted t))
              (error
               (project-frame-sessions--warn
                "Could not kill outgoing buffer %s: %s"
                (buffer-name buffer) (error-message-string err))))))))
    (project-frame-sessions--set-excluded-buffers frame excluded)))

(defun project-frame-sessions--buffer-save-predicate (buffers prior)
  "Return a Desktop predicate restricted to BUFFERS and honoring PRIOR.
Buffers matching `project-frame-sessions-ignored-buffer-name-regexp' are also
rejected."
  (lambda (filename buffer-name mode &rest args)
    (let ((buffer (get-buffer buffer-name)))
      (and (buffer-live-p buffer) (memq buffer buffers)
           (not (and project-frame-sessions-ignored-buffer-name-regexp
                     (string-match-p
                      project-frame-sessions-ignored-buffer-name-regexp
                      buffer-name)))
           (or (null prior)
               (apply prior filename buffer-name mode args))))))

(defun project-frame-sessions--utility-chat-buffer ()
  "Return the Pi chat buffer associated with the current utility buffer."
  (pcase major-mode
    ('pi-coding-agent-chat-mode (current-buffer))
    ('pi-coding-agent-input-mode
     (and (boundp 'pi-coding-agent--chat-buffer)
          (buffer-live-p pi-coding-agent--chat-buffer)
          pi-coding-agent--chat-buffer))))

(defun project-frame-sessions--pi-save-data (_desktop-directory)
  "Return Desktop metadata for the current Pi buffer, or nil."
  (let* ((role (pcase major-mode
                 ('pi-coding-agent-chat-mode 'chat)
                 ('pi-coding-agent-input-mode 'input)))
         (chat (project-frame-sessions--utility-chat-buffer))
         (state (and chat (buffer-local-value 'pi-coding-agent--state chat)))
         (session-file (and (listp state) (plist-get state :session-file)))
         (directory (and chat
                         (or (and (boundp 'pi-coding-agent--canonical-session-directory)
                                  (buffer-local-value
                                   'pi-coding-agent--canonical-session-directory
                                   chat))
                             (buffer-local-value 'default-directory chat)))))
    (if (and role chat (stringp session-file)
             (not (string-empty-p session-file)))
        (list :project-frame-sessions 1 :kind 'pi :role role
              :session-file session-file :directory directory
              :identity session-file)
      (project-frame-sessions--warn
       "Pi buffer %s has no session file and cannot be restored"
       (buffer-name))
      nil)))

(defun project-frame-sessions--eshell-save-data (_desktop-directory)
  "Return Desktop metadata for the current Eshell buffer."
  (unless project-frame-sessions--utility-identity
    (setq project-frame-sessions--utility-identity
          (project-frame-sessions--new-id)))
  (let ((directory (and (stringp default-directory)
                        (file-name-absolute-p default-directory)
                        (file-name-as-directory
                         (expand-file-name default-directory)))))
    (if directory
        (list :project-frame-sessions 1 :kind 'eshell
              :directory directory :buffer-name (buffer-name)
              :identity project-frame-sessions--utility-identity)
      (project-frame-sessions--warn
       "Eshell buffer %s has no absolute directory and cannot be restored"
       (buffer-name))
      nil)))

(defun project-frame-sessions--utility-save-data (desktop-directory)
  "Return utility metadata for Desktop in DESKTOP-DIRECTORY."
  (pcase major-mode
    ((or 'pi-coding-agent-chat-mode 'pi-coding-agent-input-mode)
     (project-frame-sessions--pi-save-data desktop-directory))
    ('eshell-mode
     (project-frame-sessions--eshell-save-data desktop-directory))))

(defun project-frame-sessions--valid-utility-data-p (data kind)
  "Return non-nil when DATA has this package's current KIND schema."
  (and (listp data)
       (equal (plist-get data :project-frame-sessions) 1)
       (eq (plist-get data :kind) kind)))

(defun project-frame-sessions--open-isolated-pi-session (session-file)
  "Open SESSION-FILE in a Pi buffer unique to that exact file.
Pi's public file-opening command currently selects its backing buffers by
working directory.  Temporarily supply a stable named-session identity so two
session files with the same working directory cannot alias or retarget each
other.  Signal an error when the installed Pi version lacks this isolation
point rather than risking mutation of another live session."
  (unless (fboundp 'pi-coding-agent--setup-session)
    (error "Installed Pi version cannot isolate restored sessions"))
  (let ((setup (symbol-function 'pi-coding-agent--setup-session))
        (identity (format "project-frame-sessions-%s"
                          (substring (secure-hash 'sha256 session-file) 0 16))))
    (cl-letf (((symbol-function 'pi-coding-agent--setup-session)
               (lambda (directory &optional _session)
                 (funcall setup directory identity))))
      (pi-coding-agent-open-session-file session-file))))

(defun project-frame-sessions--restore-pi-buffer (_file _name data)
  "Restore a Pi buffer from Desktop auxiliary DATA."
  (condition-case err
      (let ((role (plist-get data :role))
            (session-file (plist-get data :session-file)))
        (unless (and (project-frame-sessions--valid-utility-data-p data 'pi)
                     (memq role '(chat input))
                     (stringp session-file)
                     (file-name-absolute-p session-file)
                     (not (file-remote-p session-file))
                     (file-regular-p session-file)
                     (file-readable-p session-file))
          (error "Invalid or unavailable Pi session file"))
        (unless (require 'pi-coding-agent nil t)
          (error "Pi-coding-agent is unavailable"))
        (let* ((cache (or project-frame-sessions--utility-restore-cache
                          (setq project-frame-sessions--utility-restore-cache
                                (make-hash-table :test #'equal))))
               (chat (gethash session-file cache)))
          (unless (buffer-live-p chat)
            (setq chat
                  (project-frame-sessions--open-isolated-pi-session session-file))
            (unless (buffer-live-p chat)
              (error "Pi did not return a live chat buffer"))
            (puthash session-file chat cache))
          (if (eq role 'chat)
              chat
            (let ((input (buffer-local-value 'pi-coding-agent--input-buffer chat)))
              (unless (buffer-live-p input)
                (error "Pi did not create an input buffer"))
              input))))
    (error
     (project-frame-sessions--warn
      "Could not restore Pi buffer: %s" (error-message-string err))
     nil)))

(defun project-frame-sessions--safe-buffer-name-p (name)
  "Return non-nil when NAME is suitable for a restored utility buffer."
  (and (stringp name) (not (string-empty-p name))
       (not (string-prefix-p " " name))))

(defun project-frame-sessions--restore-eshell-buffer (_file name data)
  "Restore a fresh Eshell buffer from Desktop auxiliary DATA and NAME."
  (condition-case err
      (let ((directory (plist-get data :directory))
            (saved-name (plist-get data :buffer-name)))
        (unless (and (project-frame-sessions--valid-utility-data-p data 'eshell)
                     (stringp (plist-get data :identity))
                     (stringp directory) (file-name-absolute-p directory)
                     ;; Do not contact a Tramp host merely to read a session.
                     ;; Eshell will establish the connection when it needs it.
                     (or (file-remote-p directory)
                         (file-directory-p directory)))
          (error "Invalid or unavailable Eshell directory"))
        (unless (require 'eshell nil t) (error "Eshell is unavailable"))
        (let* ((default-directory (file-name-as-directory
                                   (expand-file-name directory)))
               (eshell-buffer-name
                (if (project-frame-sessions--safe-buffer-name-p saved-name)
                    saved-name
                  (if (project-frame-sessions--safe-buffer-name-p name)
                      name
                    "*eshell*")))
               (buffer (eshell t)))
          (unless (buffer-live-p buffer)
            (error "Eshell did not return a live buffer"))
          buffer))
    (error
     (project-frame-sessions--warn
      "Could not restore Eshell buffer: %s" (error-message-string err))
     nil)))

(defun project-frame-sessions--desktop-handler-owned-p (mode)
  "Return non-nil when this package owns the Desktop handler for MODE."
  (memq (assq mode desktop-buffer-mode-handlers)
        project-frame-sessions--desktop-handler-registrations))

(defun project-frame-sessions--enroll-utility-buffer ()
  "Enroll the current supported utility buffer in package Desktop saves."
  (when (and project-frame-sessions-mode
             (memq major-mode '(eshell-mode pi-coding-agent-chat-mode
                                pi-coding-agent-input-mode))
             (project-frame-sessions--desktop-handler-owned-p major-mode)
             (not project-frame-sessions--desktop-save-buffer-owned))
    (setq project-frame-sessions--prior-desktop-save-buffer-local-p
          (local-variable-p 'desktop-save-buffer)
          project-frame-sessions--prior-desktop-save-buffer desktop-save-buffer
          project-frame-sessions--desktop-save-buffer-owned t
          desktop-save-buffer #'project-frame-sessions--utility-save-data)))

(defun project-frame-sessions--unenroll-utility-buffer ()
  "Remove this package's Desktop enrollment from the current buffer."
  (when project-frame-sessions--desktop-save-buffer-owned
    (if project-frame-sessions--prior-desktop-save-buffer-local-p
        (setq desktop-save-buffer
              project-frame-sessions--prior-desktop-save-buffer)
      (kill-local-variable 'desktop-save-buffer))
    (setq project-frame-sessions--desktop-save-buffer-owned nil
          project-frame-sessions--prior-desktop-save-buffer nil
          project-frame-sessions--prior-desktop-save-buffer-local-p nil)))

(defun project-frame-sessions--register-desktop-handler (mode function)
  "Register FUNCTION for Desktop MODE unless another handler already exists."
  (let ((existing (assq mode desktop-buffer-mode-handlers)))
    (cond
     ((null existing)
      (let ((registration (cons mode function)))
        (push registration desktop-buffer-mode-handlers)
        (push registration project-frame-sessions--desktop-handler-registrations)))
     ((eq (cdr existing) function)
      ;; Do not claim an indistinguishable pre-existing registration.
      nil)
     (t
      (project-frame-sessions--warn
       "Desktop handler for %s already exists; utility restore integration skipped"
       mode)))))

(defun project-frame-sessions--register-desktop-integrations ()
  "Install optional utility Desktop integrations idempotently."
  (project-frame-sessions--register-desktop-handler
   'eshell-mode #'project-frame-sessions--restore-eshell-buffer)
  (project-frame-sessions--register-desktop-handler
   'pi-coding-agent-chat-mode #'project-frame-sessions--restore-pi-buffer)
  (project-frame-sessions--register-desktop-handler
   'pi-coding-agent-input-mode #'project-frame-sessions--restore-pi-buffer)
  (dolist (hook '(eshell-mode-hook pi-coding-agent-chat-mode-hook
                                  pi-coding-agent-input-mode-hook))
    (add-hook hook #'project-frame-sessions--enroll-utility-buffer))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (project-frame-sessions--enroll-utility-buffer))))

(defun project-frame-sessions--remove-desktop-integrations ()
  "Remove only Desktop integrations installed by this package."
  (dolist (hook '(eshell-mode-hook pi-coding-agent-chat-mode-hook
                                  pi-coding-agent-input-mode-hook))
    (remove-hook hook #'project-frame-sessions--enroll-utility-buffer))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (project-frame-sessions--unenroll-utility-buffer)))
  (dolist (registration project-frame-sessions--desktop-handler-registrations)
    (setq desktop-buffer-mode-handlers
          (delq registration desktop-buffer-mode-handlers)))
  (setq project-frame-sessions--desktop-handler-registrations nil))

(defun project-frame-sessions--desktop-frameset-form (form)
  "Extract a quoted Desktop frameset value from FORM, or nil."
  (when (and (listp form) (eq (car form) 'setq))
    (let ((tail (cdr form)) value found)
      (while tail
        (let ((variable (pop tail)) (expression (pop tail)))
          (when (eq variable 'desktop-saved-frameset)
            (setq value (if (and (consp expression)
                                 (eq (car expression) 'quote))
                            (cadr expression)
                          expression)
                  found t))))
      (and found value))))

(defun project-frame-sessions--validate-desktop (file)
  "Validate that FILE is readable Desktop data containing exactly one frame.
The file is parsed with `read-eval' disabled and is not evaluated."
  (unless (and (file-regular-p file) (not (file-symlink-p file)))
    (error "Desktop did not produce a regular snapshot"))
  (with-temp-buffer
    (insert-file-contents file)
    (let ((read-eval nil) frameset done)
      (while (not done)
        (condition-case nil
            (let* ((form (read (current-buffer)))
                   (candidate (project-frame-sessions--desktop-frameset-form form)))
              (when candidate (setq frameset candidate)))
          (end-of-file (setq done t))))
      (unless (and (frameset-p frameset)
                   (= (length (frameset-states frameset)) 1))
        (error "Snapshot must contain exactly one saved frame"))
      t)))

(defun project-frame-sessions--name-used-p (name &optional except-id)
  "Return non-nil if active NAME is used by a session other than EXCEPT-ID."
  (cl-some (lambda (entry)
             (and (equal name (plist-get entry :name))
                  (not (equal except-id (plist-get entry :id)))))
           (project-frame-sessions--read-index)))

(defun project-frame-sessions--generic-frame-name-p (name)
  "Return non-nil when NAME is unsuitable as a default session name."
  (or (not (project-frame-sessions--valid-name-p name))
      (member (string-trim name) project-frame-sessions--generic-frame-names)
      (string-match-p "\\`Emacs@" (string-trim name))))

(defun project-frame-sessions--directory-default-name (frame root)
  "Return a useful naming default for FRAME and optional ROOT."
  (let* ((directory (or root (frame-parameter frame 'default-directory)
                        (with-selected-frame frame default-directory)))
         (local (and directory (not (file-remote-p directory)) directory)))
    (if local
        (file-name-nondirectory (directory-file-name (expand-file-name local)))
      "Session")))

(defun project-frame-sessions--read-unique-name (prompt default &optional except-id)
  "Read a nonempty unique name with PROMPT and DEFAULT.
EXCEPT-ID is ignored during uniqueness checking."
  (let ((reason prompt) name)
    (while (not name)
      (let ((candidate (string-trim (read-string reason nil nil default))))
        (cond
         ((string-empty-p candidate)
          (setq reason "Session name cannot be empty; choose another: "))
         ((project-frame-sessions--name-used-p candidate except-id)
          (setq reason (format "Session name %S is already in use; choose another: "
                               candidate)))
         (t (setq name candidate)))))
    name))

(defun project-frame-sessions--first-save-metadata (frame &optional explicit)
  "Return (:name NAME :root ROOT) for FRAME's first save.
When EXPLICIT is non-nil, its root and proposed name are authoritative."
  (let* ((root (if explicit (plist-get explicit :root)
                 (ignore-errors (project-frame-sessions--discover-root frame))))
         (explicit-name (and explicit (plist-get explicit :name)))
         (project-name (if explicit explicit-name
                         (and root
                              (file-name-nondirectory
                               (directory-file-name root)))))
         (frame-name (format "%s" (or (frame-parameter frame 'name) "")))
         (suggestion (cond
                      ((and (project-frame-sessions--valid-name-p project-name)
                            (not (project-frame-sessions--name-used-p project-name)))
                       (string-trim project-name))
                      ((and (not explicit)
                            (not (project-frame-sessions--generic-frame-name-p frame-name))
                            (not (project-frame-sessions--name-used-p frame-name)))
                       frame-name))))
    (list :name (or suggestion
                    (project-frame-sessions--read-unique-name
                     (if (or (project-frame-sessions--name-used-p project-name)
                             (project-frame-sessions--name-used-p frame-name))
                         "Suggested name is already in use; choose a session name: "
                       "Name this frame session: ")
                     (project-frame-sessions--directory-default-name frame root)))
          :root root)))

(defun project-frame-sessions--runtime (id)
  "Return, creating if needed, runtime state for session ID."
  (or (gethash id project-frame-sessions--runtimes)
      (puthash id (project-frame-sessions--make-runtime)
               project-frame-sessions--runtimes)))

(defun project-frame-sessions--cancel-runtime-timer (runtime)
  "Cancel RUNTIME's pending timer."
  (when (timerp (project-frame-sessions--runtime-timer runtime))
    (cancel-timer (project-frame-sessions--runtime-timer runtime)))
  (setf (project-frame-sessions--runtime-timer runtime) nil))

(defun project-frame-sessions--retry-delay (failures)
  "Return retry delay for FAILURES consecutive failures."
  (project-frame-sessions--validate-configuration)
  (let ((delays project-frame-sessions-retry-delays))
    (when delays (nth (min (1- failures) (1- (length delays))) delays))))

(defun project-frame-sessions--find-live-frame (id &optional except)
  "Find a live frame attached to ID, excluding EXCEPT."
  (cl-find-if (lambda (frame)
                (and (frame-live-p frame) (not (eq frame except))
                     (equal id (project-frame-sessions--frame-id frame))))
              (frame-list)))

(defun project-frame-sessions--find-pending-frame (id)
  "Find the live first-save frame whose pending entry has ID."
  (let (found)
    (maphash (lambda (frame entry)
               (when (and (not found) (frame-live-p frame)
                          (equal id (plist-get entry :id)))
                 (setq found frame)))
             project-frame-sessions--pending)
    found))

(defun project-frame-sessions--schedule (id &optional delay)
  "Schedule dirty session ID after DELAY, respecting retry state."
  (project-frame-sessions--validate-configuration)
  (when (and delay (not (project-frame-sessions--nonnegative-number-p delay)))
    (error "Timer delay must be a nonnegative number, not %S" delay))
  (let* ((runtime (project-frame-sessions--runtime id))
         (now (float-time))
         (dirty-since (project-frame-sessions--runtime-dirty-since runtime))
         (forced (and dirty-since
                      (>= (- now dirty-since)
                          project-frame-sessions-maximum-dirty-age)))
         (retry (project-frame-sessions--runtime-next-retry runtime))
         (seconds (if forced 0 (or delay project-frame-sessions-debounce-delay))))
    (when (and retry (> retry now))
      (setq seconds (max seconds (- retry now))))
    (project-frame-sessions--cancel-runtime-timer runtime)
    (setf (project-frame-sessions--runtime-timer runtime)
          (run-at-time seconds nil #'project-frame-sessions--autosave-id id))))

(defun project-frame-sessions--mark-frame-dirty (&optional frame)
  "Mark enrolled FRAME dirty and debounce an autosave."
  (unless project-frame-sessions--inhibit-dirty
    (let* ((frame (or frame (selected-frame)))
           (id (and (project-frame-sessions--graphical-frame-p frame)
                    (project-frame-sessions--frame-id frame))))
      (when id
        (let ((runtime (project-frame-sessions--runtime id)))
          (cl-incf (project-frame-sessions--runtime-dirty runtime))
          (unless (project-frame-sessions--runtime-dirty-since runtime)
            (setf (project-frame-sessions--runtime-dirty-since runtime)
                  (float-time)))
          (project-frame-sessions--schedule id))))))

(defun project-frame-sessions--event-dirty (&optional frame &rest _)
  "Mark the affected enrolled graphical FRAME dirty.
Hooks that do not supply a frame, including `buffer-list-update-hook' and tab
hooks,
fall back to the selected frame."
  (when project-frame-sessions-mode
    (project-frame-sessions--mark-frame-dirty
     (if (framep frame) frame (selected-frame)))))

(defun project-frame-sessions--save-desktop-to-stage (frame stage)
  "Save FRAME's Desktop snapshot into STAGE."
  (let* ((buffers (project-frame-sessions--tab-buffers frame))
         (prior desktop-buffers-not-to-save-function)
         (old-parameters
          (mapcar (lambda (candidate)
                    (cons candidate
                          (frame-parameter candidate 'desktop-dont-save)))
                  (frame-list))))
    (unwind-protect
        (progn
          (dolist (candidate (frame-list))
            (unless (eq candidate frame)
              (set-frame-parameter candidate 'desktop-dont-save t)))
          (project-frame-sessions--with-desktop-state
           (lambda ()
             (let ((project-frame-sessions--inhibit-dirty t)
                   (write-region-inhibit-fsync nil)
                   (desktop-buffers-not-to-save-function
                    (project-frame-sessions--buffer-save-predicate buffers prior))
                   (desktop-restore-frames t)
                   (desktop-globals-to-save nil)
                   (desktop-file-name-format 'absolute)
                   (desktop-base-file-name project-frame-sessions--desktop-name)
                   (frameset-filter-alist
                    (append
                     `((,project-frame-sessions--excluded-buffers-parameter
                        . :never)
                       (tabs . project-frame-sessions--filter-tabs))
                     frameset-filter-alist)))
               (desktop-save stage t)))))
      (dolist (pair old-parameters)
        (when (frame-live-p (car pair))
          (set-frame-parameter (car pair) 'desktop-dont-save (cdr pair)))))))

(defun project-frame-sessions--stage-directory (id token)
  "Return the recovery stage directory for session ID and TOKEN."
  (expand-file-name (format ".stage-%s-%s/" id token)
                    (project-frame-sessions--recovery-directory)))

(defun project-frame-sessions--replace-entry (entry entries)
  "Return ENTRIES with ENTRY replacing the same ID."
  (cons entry (cl-remove (plist-get entry :id) entries
                         :key (lambda (item) (plist-get item :id))
                         :test #'equal)))

(defun project-frame-sessions--cleanup-session-snapshots (entry)
  "Delete superseded regular snapshot files belonging to ENTRY."
  (let ((directory (file-name-directory
                    (project-frame-sessions--entry-snapshot-file entry)))
        (keep (project-frame-sessions--entry-snapshot-file entry)))
    (when (and (file-directory-p directory) (not (file-symlink-p directory)))
      (dolist (file (directory-files directory t
                                     "\\`desktop-[0-9a-f]\\{16\\}\\.el\\'"))
        (when (and (not (equal file keep)) (file-regular-p file)
                   (not (file-symlink-p file)))
          (delete-file file))))))

(defun project-frame-sessions--commit-save (frame entry stage token)
  "Commit FRAME's staged ENTRY from STAGE using TOKEN.
Return the committed entry.  Caller holds the package lock."
  (let* ((id (plist-get entry :id))
         (relative (format "sessions/%s/desktop-%s.el" id token))
         (target (expand-file-name relative project-frame-sessions-directory))
         (staged (expand-file-name project-frame-sessions--desktop-name stage))
         (metadata-file (expand-file-name "metadata.eld" stage))
         (old-entries (project-frame-sessions--read-index))
         (old-entry (project-frame-sessions--find-entry id old-entries))
         (committed (copy-sequence entry)))
    (setq committed (plist-put committed :snapshot relative))
    (setq committed (plist-put committed :saved-at (float-time)))
    (setq committed (plist-put committed :state 'active))
    (when-let* ((collision (project-frame-sessions--find-entry-by-name
                           (plist-get committed :name) old-entries)))
      (unless (equal id (plist-get collision :id))
        (error "Session name %S is already in use" (plist-get committed :name))))
    (project-frame-sessions--atomic-write
     metadata-file (list :kind 'save :entry committed :target relative))
    (project-frame-sessions--failpoint 'after-save-metadata-write)
    (let ((directory (file-name-directory target)))
      (when (file-symlink-p (directory-file-name directory))
        (error "Refusing symlink session directory for %s"
               (plist-get committed :name)))
      (make-directory directory t)
      (unless (project-frame-sessions--safe-direct-child-p
               directory (project-frame-sessions--sessions-directory)
               (format "\\`%s\\'" (regexp-quote id)))
        (error "Unsafe session directory for %s" (plist-get committed :name))))
    (project-frame-sessions--assert-safe-managed-file staged)
    (project-frame-sessions--assert-safe-managed-file target)
    (rename-file staged target)
    (set-file-modes target #o600)
    (project-frame-sessions--failpoint 'after-snapshot-promotion)
    (condition-case err
        (progn
          (project-frame-sessions--failpoint 'before-index-write)
          (project-frame-sessions--write-index
           (project-frame-sessions--replace-entry committed old-entries))
          (project-frame-sessions--failpoint 'after-index-write))
      (error
       ;; The promoted snapshot remains recoverable with its stage metadata.
       (signal (car err) (cdr err))))
    (project-frame-sessions--failpoint 'before-old-snapshot-cleanup)
    (when old-entry
      (let ((old-file (project-frame-sessions--entry-snapshot-file old-entry)))
        (when (and (not (equal old-file target)) (file-regular-p old-file)
                   (not (file-symlink-p old-file)))
          (ignore-errors (delete-file old-file)))))
    (project-frame-sessions--failpoint 'before-stage-cleanup)
    (ignore-errors (delete-directory stage t))
    (project-frame-sessions--cleanup-session-snapshots committed)
    (set-frame-parameter frame project-frame-sessions--id-parameter id)
    (set-frame-parameter frame project-frame-sessions--deleted-parameter nil)
    committed))

(cl-defun project-frame-sessions--save-frame-internal
    (frame manual &optional first-save-metadata)
  "Save FRAME transactionally.  MANUAL bypasses retry backoff.
FIRST-SAVE-METADATA, when non-nil, supplies authoritative project metadata."
  (unless (project-frame-sessions--graphical-frame-p frame)
    (user-error "Frame sessions require a savable top-level graphical frame"))
  (let* ((existing-id (project-frame-sessions--frame-id frame))
         (existing (and existing-id (project-frame-sessions--find-entry existing-id)))
         (pending (gethash frame project-frame-sessions--pending)))
    (when (and existing-id (not existing))
      (error "Frame refers to missing session %s" existing-id))
    (let* ((metadata (unless (or existing pending)
                       (if first-save-metadata
                           (project-frame-sessions--first-save-metadata
                            frame first-save-metadata)
                         (project-frame-sessions--first-save-metadata frame))))
           (id (or existing-id (plist-get pending :id)
                   (project-frame-sessions--new-id)))
           (entry (or (and existing (copy-sequence existing))
                      (and pending (copy-sequence pending))
                      (list :id id :name (plist-get metadata :name)
                            :root (plist-get metadata :root))))
           (runtime (project-frame-sessions--runtime id))
           start-generation
           (token (substring (secure-hash 'sha256
                                          (format "%s:%s:%s" id (float-time)
                                                  (random most-positive-fixnum)))
                             0 16))
           (stage (project-frame-sessions--stage-directory id token))
           committed)
      (unless existing
        (puthash frame entry project-frame-sessions--pending)
        (when (= (project-frame-sessions--runtime-dirty runtime)
                 (project-frame-sessions--runtime-saved runtime))
          (cl-incf (project-frame-sessions--runtime-dirty runtime))
          (setf (project-frame-sessions--runtime-dirty-since runtime)
                (float-time))))
      (setq start-generation (project-frame-sessions--runtime-dirty runtime))
      (when (project-frame-sessions--runtime-saving runtime)
        (if manual (user-error "Session %s is already being saved"
                               (plist-get entry :name))
          (cl-return-from project-frame-sessions--save-frame-internal nil)))
      (when (and (not manual)
                 (project-frame-sessions--runtime-next-retry runtime)
                 (> (project-frame-sessions--runtime-next-retry runtime)
                    (float-time)))
        (project-frame-sessions--schedule id)
        (cl-return-from project-frame-sessions--save-frame-internal nil))
      (setf (project-frame-sessions--runtime-saving runtime) t)
      (project-frame-sessions--cancel-runtime-timer runtime)
      (condition-case err
          (unwind-protect
              (project-frame-sessions--with-lock
                (project-frame-sessions--assert-contained-path
                 stage (project-frame-sessions--recovery-directory))
                (make-directory stage t)
                (project-frame-sessions--assert-real-directory
                 stage (project-frame-sessions--recovery-directory))
                (project-frame-sessions--failpoint 'after-stage-creation)
                (project-frame-sessions--save-desktop-to-stage frame stage)
                (project-frame-sessions--failpoint 'after-desktop-snapshot-creation)
                (let ((staged (expand-file-name
                               project-frame-sessions--desktop-name stage)))
                  (unless (file-exists-p staged)
                    (error "Desktop did not create %s"
                           project-frame-sessions--desktop-name))
                  (set-file-modes staged #o600)
                  (project-frame-sessions--validate-desktop staged)
                  (project-frame-sessions--failpoint 'after-staged-snapshot-validation))
                (setq committed
                      (project-frame-sessions--commit-save
                       frame entry stage token)))
            (setf (project-frame-sessions--runtime-saving runtime) nil))
        (error
         (setf (project-frame-sessions--runtime-saving runtime) nil)
         (cl-incf (project-frame-sessions--runtime-failures runtime))
         (let* ((failures (project-frame-sessions--runtime-failures runtime))
                (delay (project-frame-sessions--retry-delay failures))
                (name (plist-get entry :name))
                (reason (error-message-string err)))
           (setf (project-frame-sessions--runtime-last-error runtime) reason
                 (project-frame-sessions--runtime-next-retry runtime)
                 (and delay (+ (float-time) delay)))
           (unless (project-frame-sessions--runtime-dirty-since runtime)
             (setf (project-frame-sessions--runtime-dirty-since runtime)
                   (float-time)))
           (project-frame-sessions--warn
            "Session %s save failed: %s. The last committed snapshot is safe.%s Run M-x project-frame-sessions-save to retry now."
            name reason (if delay (format " Automatic retry in %ss." delay) ""))
           (when delay (project-frame-sessions--schedule id delay)))
         (signal (car err) (cdr err))))
      (when committed
        (remhash frame project-frame-sessions--pending)
        (setf (project-frame-sessions--runtime-saved runtime) start-generation
              (project-frame-sessions--runtime-failures runtime) 0
              (project-frame-sessions--runtime-next-retry runtime) nil
              (project-frame-sessions--runtime-last-error runtime) nil
              (project-frame-sessions--runtime-last-success runtime) (float-time))
        (if (> (project-frame-sessions--runtime-dirty runtime) start-generation)
            (project-frame-sessions--schedule id)
          (setf (project-frame-sessions--runtime-dirty-since runtime) nil))
        committed))))

(defun project-frame-sessions--handle-deleted-frame (frame)
  "Resolve FRAME's detached soft-deleted session before saving."
  (when-let* ((deleted (frame-parameter frame
                                        project-frame-sessions--deleted-parameter)))
    (pcase (read-char-choice
            (format "Session %s was deleted: [r]ecover it or create [n]ew? "
                    (plist-get deleted :name))
            '(?r ?n))
      (?r
       (project-frame-sessions--recover-trash-entry
        (plist-get deleted :trash) frame))
      (?n
       (set-frame-parameter frame project-frame-sessions--deleted-parameter nil)))))

;;;###autoload
(defun project-frame-sessions-save (&optional frame)
  "Enroll if necessary, then save FRAME transactionally.
Interactively FRAME is the selected frame.  A manual first save works without
`project.el' and prompts only when no acceptable unique name can be inferred."
  (interactive)
  (let ((frame (or frame (selected-frame))))
    (project-frame-sessions--handle-deleted-frame frame)
    (let ((entry (project-frame-sessions--save-frame-internal frame t)))
      (when entry
        (message "Saved frame session %s" (plist-get entry :name)))
      entry)))

(defun project-frame-sessions--autosave-id (id)
  "Autosave dirty session ID and retain failures for retry."
  (when project-frame-sessions-mode
    (when-let* ((frame (or (project-frame-sessions--find-live-frame id)
                           (project-frame-sessions--find-pending-frame id)))
                (runtime (project-frame-sessions--runtime id)))
      (when (> (project-frame-sessions--runtime-dirty runtime)
               (project-frame-sessions--runtime-saved runtime))
        (condition-case nil
            (project-frame-sessions--save-frame-internal frame nil)
          (error nil))))))

(defun project-frame-sessions--idle-save ()
  "Idle fallback for dirty sessions and project-assisted enrollment."
  (when project-frame-sessions-mode
    (dolist (frame (frame-list))
      (when (project-frame-sessions--graphical-frame-p frame)
        (if-let* ((id (project-frame-sessions--frame-id frame)))
            (project-frame-sessions--autosave-id id)
          ;; Ordinary frames remain untouched.  A detected project may perform
          ;; its first save without assigning identity until commit succeeds.
          (when (ignore-errors (project-frame-sessions--discover-root frame))
            (condition-case err
                (project-frame-sessions--save-frame-internal frame nil)
              (error
               (project-frame-sessions--warn
                "Automatic project enrollment failed: %s"
                (error-message-string err))))))))))

(defun project-frame-sessions-status (&optional frame)
  "Return a concise status symbol for FRAME's session."
  (let* ((frame (or frame (selected-frame)))
         (deleted (frame-parameter frame project-frame-sessions--deleted-parameter))
         (id (project-frame-sessions--frame-id frame))
         (runtime (and id (project-frame-sessions--runtime id))))
    (cond
     (deleted 'soft-deleted)
     ((not id) 'unmanaged)
     ((project-frame-sessions--runtime-saving runtime) 'saving)
     ((project-frame-sessions--runtime-last-error runtime)
      (if (project-frame-sessions--runtime-next-retry runtime)
          'retry-scheduled 'save-failed))
     ((> (project-frame-sessions--runtime-dirty runtime)
         (project-frame-sessions--runtime-saved runtime)) 'dirty)
     (t 'saved))))

(defun project-frame-sessions--candidate-label (entry &optional state)
  "Return concise selection label for ENTRY and STATE."
  (format "%s%s" (plist-get entry :name)
          (pcase state
            ('deleted " (deleted)")
            ('recovery " (recovery)")
            (_ (if (project-frame-sessions--find-live-frame
                    (plist-get entry :id)) " (open)" "")))))

(defun project-frame-sessions--known-project-roots ()
  "Return project.el's remembered roots, or nil when unavailable."
  (condition-case nil
      (when (and (require 'project nil t)
                 (fboundp 'project-known-project-roots))
        (project-known-project-roots))
    (error nil)))

(defun project-frame-sessions--project-name-at-root (root)
  "Return project.el's project name for canonical ROOT, or nil."
  (condition-case nil
      (when (and (fboundp 'project-current) (fboundp 'project-name))
        (when-let* ((project (project-current nil root))
                    (name (project-name project)))
          (and (project-frame-sessions--valid-name-p name)
               (string-trim name))))
    (error nil)))

(defun project-frame-sessions--saved-roots (entries)
  "Return canonical roots represented by active ENTRIES."
  (delete-dups
   (delq nil
         (mapcar (lambda (entry)
                   (ignore-errors
                     (project-frame-sessions--canonical-root
                      (plist-get entry :root))))
                 entries))))

(defun project-frame-sessions--unsaved-projects (entries)
  "Return tagged remembered projects not represented by ENTRIES."
  (let ((saved (project-frame-sessions--saved-roots entries)) seen projects)
    (dolist (root (project-frame-sessions--known-project-roots))
      (condition-case nil
          (when (and (stringp root) (file-name-absolute-p root)
                     (not (file-remote-p root)) (file-directory-p root))
            (let ((canonical (project-frame-sessions--canonical-root root)))
              (unless (or (member canonical saved) (member canonical seen))
                (push canonical seen)
                (push (list :type 'project :root canonical
                            :name (project-frame-sessions--project-name-at-root
                                   canonical))
                      projects))))
        (error nil)))
    (nreverse projects)))

(defun project-frame-sessions--unique-project-label (project occupied projects)
  "Return a unique completion label for PROJECT.
OCCUPIED contains saved-session labels and PROJECTS is the full project list."
  (let* ((root (plist-get project :root))
         (name (or (plist-get project :name)
                   (file-name-nondirectory (directory-file-name root))))
         (same-name (cl-count name projects :test #'equal
                              :key (lambda (item)
                                     (or (plist-get item :name)
                                         (file-name-nondirectory
                                          (directory-file-name
                                           (plist-get item :root))))))))
    (if (and (= same-name 1) (not (member name occupied)))
        name
      (format "%s — %s" name (abbreviate-file-name root)))))

(defun project-frame-sessions--active-choices ()
  "Return active restore completion choices."
  (let (choices)
    (dolist (entry (sort (copy-sequence (project-frame-sessions--read-index))
                         (lambda (a b) (> (plist-get a :saved-at)
                                          (plist-get b :saved-at)))))
      (if (file-exists-p (project-frame-sessions--entry-snapshot-file entry))
          (push (cons (project-frame-sessions--candidate-label entry) entry) choices)
        (project-frame-sessions--warn "Session %s has a missing snapshot"
                                      (plist-get entry :name))))
    (nreverse choices)))

(defun project-frame-sessions--restore-choices ()
  "Return tagged saved-session and remembered-project restore choices."
  (let* ((entries (project-frame-sessions--read-index))
         (session-choices (project-frame-sessions--active-choices))
         (projects (project-frame-sessions--unsaved-projects entries))
         (occupied (mapcar #'car session-choices))
         (choices
          (mapcar (lambda (choice)
                    (cons (car choice)
                          (list :type 'session :entry (cdr choice))))
                  session-choices)))
    (dolist (project projects choices)
      (let ((label (project-frame-sessions--unique-project-label
                    project occupied projects)))
        (push label occupied)
        (setq choices (append choices (list (cons label project))))))))

(defun project-frame-sessions--select-restore-candidate (prompt)
  "Select a tagged restore candidate using PROMPT."
  (project-frame-sessions--scan-recovery)
  (let ((choices (project-frame-sessions--restore-choices)))
    (unless choices (user-error "No saved frame sessions"))
    (let* ((selection (completing-read prompt choices nil t))
           (candidate (cdr (assoc-string selection choices))))
      (unless candidate (user-error "No session selected"))
      candidate)))

(defun project-frame-sessions--select-active (prompt)
  "Select an active session using PROMPT."
  (project-frame-sessions--scan-recovery)
  (let ((choices (project-frame-sessions--active-choices)))
    (unless choices (user-error "No saved frame sessions"))
    (let* ((selection (completing-read prompt choices nil t))
           (entry (cdr (assoc selection choices))))
      (unless entry (user-error "No session selected"))
      entry)))

(defun project-frame-sessions--read-desktop-frameset (entry frame)
  "Read ENTRY's Desktop buffers in FRAME and return its single Frameset.
Desktop clears `desktop-saved-frameset' before `desktop-read' returns on
recent Emacs versions, so capture it when Desktop offers to restore it."
  (let* ((file (project-frame-sessions--entry-snapshot-file entry))
         (directory (file-name-directory file))
         (base (file-name-nondirectory file)))
    (unless (and (project-frame-sessions--safe-direct-child-p
                  directory (project-frame-sessions--sessions-directory)
                  (format "\\`%s\\'" (regexp-quote (plist-get entry :id))))
                 (file-regular-p file)
                 (not (file-symlink-p file)))
      (error "Session %s snapshot is missing or unsafe"
             (plist-get entry :name)))
    (let ((owner-before (desktop-owner directory)))
      (when (and owner-before (not (equal owner-before (emacs-pid))))
      (error "Session %s snapshot is locked by PID %s"
             (plist-get entry :name) owner-before))
    (project-frame-sessions--with-desktop-state
     (lambda ()
       (let ((desktop-dirname directory) (desktop-restore-frames nil)
             (desktop-restore-eager t) (desktop-missing-file-warning nil)
             (desktop-after-read-hook nil) (desktop-saved-frameset nil)
             (desktop-base-file-name base)
             (project-frame-sessions--utility-restore-cache
              (make-hash-table :test #'equal))
             captured-frameset)
         (unwind-protect
             (progn
               (let ((create-buffer (symbol-function 'desktop-create-buffer)))
                 (cl-letf (((symbol-function 'desktop-restore-frameset)
                            (lambda ()
                              (setq captured-frameset desktop-saved-frameset)))
                           ((symbol-function 'desktop-create-buffer)
                            (lambda (&rest args)
                              (let ((buffer (apply create-buffer args)))
                                (when (buffer-live-p buffer)
                                  (cl-pushnew
                                   buffer
                                   project-frame-sessions--desktop-restored-buffers))
                                buffer))))
                   (with-selected-frame frame
                     (desktop-read directory))))
               (unless (and (frameset-p captured-frameset)
                            (= (length (frameset-states captured-frameset)) 1))
                 (error "Session %s snapshot does not contain exactly one frame"
                        (plist-get entry :name)))
               captured-frameset)
           (when (and (null owner-before)
                      (equal (desktop-owner directory) (emacs-pid)))
             (desktop-release-lock directory)))))))))

(defun project-frame-sessions--prepare-restore-target (frame)
  "Clear FRAME's tabs and layout without killing buffers."
  (with-selected-frame frame
    (switch-to-buffer (get-buffer-create "*scratch*"))
    (when (fboundp 'tab-bar-tabs-set)
      (tab-bar-tabs-set nil)
      (set-frame-parameter frame 'current-tab nil))
    (delete-other-windows)))

(defun project-frame-sessions--restore-frameset (frameset frame)
  "Restore FRAMESET into exactly FRAME."
  (with-selected-frame frame
    (let ((warning-minimum-level :error))
      (frameset-restore
       frameset :filters frameset-filter-alist
       :reuse-frames (lambda (candidate) (eq candidate frame))
       :cleanup-frames nil :force-display t
       :force-onscreen (and desktop-restore-forces-onscreen
                            (display-graphic-p frame))))))

(defun project-frame-sessions--delete-new-frames (before)
  "Delete frames not present in BEFORE, returning cleanup errors."
  (let (errors)
    (dolist (frame (frame-list))
      (when (and (frame-live-p frame) (not (memq frame before)))
        (condition-case err (delete-frame frame t)
          (error (push err errors)))))
    errors))

(defun project-frame-sessions--restore-entry (entry frame save-current)
  "Restore ENTRY into FRAME.  SAVE-CURRENT offers to save its current session."
  (catch 'live
    (when-let* ((owner (project-frame-sessions--find-live-frame
                        (plist-get entry :id) frame)))
      (select-frame-set-input-focus owner)
      (throw 'live owner))
    (let* ((before-frames (frame-list))
           (outgoing-buffers
            (and save-current (project-frame-sessions--tab-buffers frame)))
           (prior-excluded
            (and save-current (project-frame-sessions--excluded-buffers frame)))
           (rollback (frameset-save (list frame) :filters frameset-filter-alist))
           (old-id (project-frame-sessions--frame-id frame))
           (old-deleted (frame-parameter frame
                                         project-frame-sessions--deleted-parameter))
           (old-root (frame-parameter frame 'project-frame-sessions-root))
           (old-directory (with-selected-frame frame default-directory))
           (project-frame-sessions--desktop-restored-buffers nil)
           restore-error)
      (condition-case err
          (progn
            (when (and save-current old-id
                       (yes-or-no-p
                        (format "Save current session %s before restoring %s? "
                                (project-frame-sessions--entry-name old-id)
                                (plist-get entry :name))))
              (project-frame-sessions--save-frame-internal frame t))
            (let ((project-frame-sessions--inhibit-dirty t))
              (let ((frameset
                     (project-frame-sessions--read-desktop-frameset entry frame)))
                (project-frame-sessions--prepare-restore-target frame)
                (project-frame-sessions--restore-frameset frameset frame)))
            (when (cl-set-difference (frame-list) before-frames)
              (project-frame-sessions--delete-new-frames before-frames)
              (error "Session %s unexpectedly created another frame"
                     (plist-get entry :name)))
            (set-frame-parameter frame project-frame-sessions--id-parameter
                                 (plist-get entry :id))
            (set-frame-parameter frame project-frame-sessions--deleted-parameter nil)
            (set-frame-parameter frame 'project-frame-sessions-root
                                 (plist-get entry :root))
            (when (plist-get entry :root)
              (with-selected-frame frame
                (setq default-directory (plist-get entry :root))))
            (when save-current
              (project-frame-sessions--complete-current-frame-switch
               frame outgoing-buffers prior-excluded
               (append project-frame-sessions--desktop-restored-buffers
                       (project-frame-sessions--active-workspace-buffers frame))))
            (select-frame-set-input-focus frame)
            (when project-frame-sessions-post-restore-function
              (run-at-time 0 nil project-frame-sessions-post-restore-function frame))
            frame)
        (error (setq restore-error err)))
      (when restore-error
        (let ((rollback-errors
               (project-frame-sessions--delete-new-frames before-frames)))
          (condition-case err
              (let ((project-frame-sessions--inhibit-dirty t))
                (project-frame-sessions--prepare-restore-target frame)
                (project-frame-sessions--restore-frameset rollback frame)
                (set-frame-parameter frame project-frame-sessions--id-parameter old-id)
                (set-frame-parameter frame project-frame-sessions--deleted-parameter
                                     old-deleted)
                (set-frame-parameter frame 'project-frame-sessions-root old-root)
                (with-selected-frame frame
                  (setq default-directory old-directory))
                (select-frame-set-input-focus frame))
            (error (push err rollback-errors)))
          (when save-current
            ;; Desktop may have displayed newly loaded buffers before the
            ;; failure.  Keep them alive, but do not let the frame's
            ;; historical buffer list enroll them in the rolled-back session.
            (project-frame-sessions--set-excluded-buffers
             frame
             (cl-remove-duplicates
              (append
               prior-excluded
               (cl-set-difference
                project-frame-sessions--desktop-restored-buffers
                outgoing-buffers :test #'eq))
              :test #'eq)))
          (when rollback-errors
            (project-frame-sessions--warn
             "Session %s restore rollback also failed: %s"
             (plist-get entry :name)
             (mapconcat #'error-message-string rollback-errors "; ")))
          (project-frame-sessions--warn
           "Session %s restore failed; newly loaded buffers were left alive"
           (plist-get entry :name))
          (signal (car restore-error) (cdr restore-error))))
      frame)))

(defun project-frame-sessions--enroll-project (frame project &optional switch-current)
  "Open tagged PROJECT in FRAME and transactionally enroll the frame.
When SWITCH-CURRENT is non-nil, detach the outgoing session and apply the
configured buffer cleanup only after the new project's first save succeeds."
  (let* ((root (plist-get project :root))
         (outgoing-buffers
          (and switch-current (project-frame-sessions--tab-buffers frame)))
         (prior-excluded
          (and switch-current (project-frame-sessions--excluded-buffers frame))))
    (set-frame-parameter frame 'project-frame-sessions-root root)
    (when switch-current
      (set-frame-parameter frame project-frame-sessions--id-parameter nil)
      (set-frame-parameter frame project-frame-sessions--deleted-parameter nil))
    (with-selected-frame frame
      (setq default-directory root)
      (require 'dired)
      (dired root)
      ;; Save only after Dired has installed its buffer and window state.
      (let* ((destination
              (project-frame-sessions--active-workspace-buffers frame))
             (save-exclusions
              (and switch-current
                   (cl-set-difference
                    (cl-remove-duplicates
                     (append prior-excluded outgoing-buffers) :test #'eq)
                    destination :test #'eq))))
        (let ((project-frame-sessions--save-buffer-exclusions save-exclusions))
          (project-frame-sessions--save-frame-internal
           frame t (list :root root :name (plist-get project :name))))
        (when switch-current
          (project-frame-sessions--complete-current-frame-switch
           frame outgoing-buffers prior-excluded destination))))
    (select-frame-set-input-focus frame)
    frame))

(defun project-frame-sessions--route-restore (new-frame-p)
  "Select and route a restore candidate, honoring NEW-FRAME-P."
  (let ((candidate
         (project-frame-sessions--select-restore-candidate
          "Restore frame session or project: ")))
    (pcase (plist-get candidate :type)
      ('project
       ;; Project failures intentionally leave this frame available for retry.
       (project-frame-sessions--enroll-project
        (if new-frame-p (make-frame) (selected-frame)) candidate
        (not new-frame-p)))
      ('session
       (let* ((entry (plist-get candidate :entry))
              (owner (project-frame-sessions--find-live-frame
                      (plist-get entry :id))))
         (if owner
             (progn (select-frame-set-input-focus owner) owner)
           (if new-frame-p
               (let ((frame (make-frame)))
                 (condition-case err
                     (project-frame-sessions--restore-entry entry frame nil)
                   (error
                    (when (frame-live-p frame)
                      (let ((project-frame-sessions--delete-suppressed frame))
                        (delete-frame frame t)))
                    (signal (car err) (cdr err)))))
             (project-frame-sessions--restore-entry entry (selected-frame) t)))))
      (_ (error "Unknown restore candidate type: %S"
                (plist-get candidate :type))))))

;;;###autoload
(defun project-frame-sessions-restore ()
  "Select a session or remembered project and open it in a new frame.
A live session is focused instead.  A project opens in Dired and is enrolled as
a session.  Canceling selection creates no frame."
  (interactive)
  (project-frame-sessions--route-restore t))

;;;###autoload
(defun project-frame-sessions-restore-current-frame ()
  "Select a session or remembered project for the selected frame.
Session restoration rolls back on failure.  Outgoing buffers are handled by
`project-frame-sessions-kill-buffers-on-switch'.  A project opens in Dired and
is then enrolled; project open or save failures intentionally leave the frame
at the project for manual recovery."
  (interactive)
  (project-frame-sessions--route-restore nil))

(defun project-frame-sessions--select-entry-for-management (prompt)
  "Use current frame's session or select one with PROMPT."
  (or (project-frame-sessions--frame-entry)
      (project-frame-sessions--select-active prompt)))

;;;###autoload
(defun project-frame-sessions-rename (&optional new-name)
  "Rename the current or selected session to unique NEW-NAME.
The stable ID and snapshot path do not change."
  (interactive)
  (let* ((entry (project-frame-sessions--select-entry-for-management
                 "Rename frame session: "))
         (id (plist-get entry :id))
         (name (or new-name
                   (project-frame-sessions--read-unique-name
                    (format "Rename session %s to: " (plist-get entry :name))
                    (plist-get entry :name) id))))
    (setq name (string-trim name))
    (unless (project-frame-sessions--valid-name-p name)
      (user-error "Session name cannot be empty"))
    (when (project-frame-sessions--name-used-p name id)
      (user-error "Session name %S is already in use" name))
    (project-frame-sessions--with-lock
      (let* ((entries (project-frame-sessions--read-index))
             (current (project-frame-sessions--find-entry id entries))
             (renamed (copy-sequence current)))
        (unless current (error "Session disappeared while renaming"))
        (setq renamed (plist-put renamed :name name))
        (project-frame-sessions--write-index
         (project-frame-sessions--replace-entry renamed entries))))
    (message "Renamed frame session to %s" name)
    name))

(defun project-frame-sessions--trash-metadata-file (trash)
  "Return the metadata file in TRASH."
  (expand-file-name "metadata.eld" trash))

(defun project-frame-sessions--trash-entries ()
  "Return validated, fully soft-deleted entries with :trash paths.
A trash copy whose ID is still active is an interrupted pre-commit deletion;
the active session remains authoritative and that copy is not exposed."
  (let ((parent (project-frame-sessions--trash-directory))
        (active (project-frame-sessions--read-index)) result)
    (when (file-directory-p parent)
      (dolist (directory (directory-files parent t directory-files-no-dot-files-regexp))
        (when (project-frame-sessions--safe-direct-child-p
               directory parent "\\`[0-9a-f]\\{64\\}-[0-9a-f]\\{16\\}\\'")
          (condition-case nil
              (let* ((data (project-frame-sessions--read-object
                            (project-frame-sessions--trash-metadata-file directory)))
                     (entry (plist-get data :entry)))
                (when (and (eq (plist-get data :kind) 'deleted)
                           (project-frame-sessions--valid-entry-p entry)
                           (not (project-frame-sessions--find-entry
                                 (plist-get entry :id) active)))
                  (setq entry (copy-sequence entry))
                  (setq entry (plist-put entry :trash directory))
                  (push entry result)))
            (error nil)))))
    result))

(defun project-frame-sessions--soft-delete-entry (entry)
  "Soft-delete active ENTRY and return its trash path.
The snapshot is copied to trash before the index switch, so every crash boundary
leaves either the active source authoritative or a complete trash copy."
  (project-frame-sessions--with-lock
    (let* ((entries (project-frame-sessions--read-index))
           (id (plist-get entry :id))
           (current (project-frame-sessions--find-entry id entries))
           (source (expand-file-name id (project-frame-sessions--sessions-directory)))
           (source-file (and current
                             (project-frame-sessions--entry-snapshot-file current)))
           (token (substring (secure-hash 'sha256
                                          (format "%s:%s:%s" id (float-time)
                                                  (random most-positive-fixnum)))
                             0 16))
           (trash (expand-file-name (format "%s-%s" id token)
                                    (project-frame-sessions--trash-directory)))
           (trash-file (and source-file
                            (expand-file-name (file-name-nondirectory source-file)
                                              trash)))
           index-committed)
      (unless current (error "Session %s is no longer active" (plist-get entry :name)))
      (unless (and (project-frame-sessions--safe-direct-child-p
                    source (project-frame-sessions--sessions-directory)
                    "\\`[0-9a-f]\\{64\\}\\'")
                   (file-regular-p source-file) (not (file-symlink-p source-file)))
        (error "Session %s storage is missing or unsafe" (plist-get entry :name)))
      (project-frame-sessions--assert-contained-path
       trash (project-frame-sessions--trash-directory))
      (make-directory trash t)
      (project-frame-sessions--assert-real-directory
       trash (project-frame-sessions--trash-directory))
      (project-frame-sessions--failpoint 'after-trash-creation)
      (condition-case err
          (progn
            (let ((write-region-inhibit-fsync nil))
              (copy-file source-file trash-file t t))
            (set-file-modes trash-file #o600)
            (project-frame-sessions--failpoint 'after-trash-snapshot-copy)
            (project-frame-sessions--atomic-write
             (project-frame-sessions--trash-metadata-file trash)
             (list :kind 'deleted :entry current :deleted-at (float-time)))
            (project-frame-sessions--failpoint 'after-delete-metadata-write)
            (project-frame-sessions--failpoint 'before-delete-index-write)
            (project-frame-sessions--write-index
             (cl-remove id entries :key (lambda (item) (plist-get item :id))
                        :test #'equal))
            (setq index-committed t)
            (project-frame-sessions--failpoint 'after-delete-index-write))
        (error
         (unless index-committed (ignore-errors (delete-directory trash t)))
         (signal (car err) (cdr err))))
      ;; Failure here is only orphan cleanup: the index and trash copy have
      ;; already committed the deletion and remain recoverable.
      (project-frame-sessions--failpoint 'before-deleted-source-cleanup)
      (condition-case err
          (delete-directory source t)
        (error (project-frame-sessions--warn
                "Session %s was deleted, but old active storage cleanup failed: %s"
                (plist-get current :name) (error-message-string err))))
      (dolist (frame (frame-list))
        (when (equal id (project-frame-sessions--frame-id frame))
          (set-frame-parameter frame project-frame-sessions--id-parameter nil)
          (set-frame-parameter frame project-frame-sessions--deleted-parameter
                               (list :id id :name (plist-get current :name)
                                     :trash trash))))
      (when-let* ((runtime (gethash id project-frame-sessions--runtimes)))
        (project-frame-sessions--cancel-runtime-timer runtime)
        (remhash id project-frame-sessions--runtimes))
      trash)))

;;;###autoload
(defun project-frame-sessions-delete ()
  "Soft-delete a selected session without closing frames or killing buffers."
  (interactive)
  (let ((entry (project-frame-sessions--select-entry-for-management
                "Delete frame session: ")))
    (when (yes-or-no-p (format "Move session %s to recoverable trash? "
                               (plist-get entry :name)))
      (project-frame-sessions--soft-delete-entry entry)
      (message "Soft-deleted frame session %s; use M-x project-frame-sessions-recover to recover it"
               (plist-get entry :name)))))

;;;###autoload
(defun project-frame-sessions-delete-all ()
  "Soft-delete all active sessions after explicit confirmation."
  (interactive)
  (let ((entries (project-frame-sessions--read-index)))
    (unless entries (user-error "No active frame sessions"))
    (when (yes-or-no-p (format "Move ALL %d sessions to recoverable trash? "
                               (length entries)))
      (dolist (entry (copy-sequence entries))
        (project-frame-sessions--soft-delete-entry entry))
      (message "Soft-deleted %d frame sessions" (length entries)))))

(defun project-frame-sessions--recover-trash-entry (trash &optional frame)
  "Recover the entry in TRASH and optionally attach FRAME."
  (project-frame-sessions--with-lock
    (let* ((data (project-frame-sessions--read-object
                  (project-frame-sessions--trash-metadata-file trash)))
           (entry (copy-sequence (plist-get data :entry)))
           (id (plist-get entry :id))
           (name (plist-get entry :name))
           (entries (project-frame-sessions--read-index))
           (destination (expand-file-name id
                                          (project-frame-sessions--sessions-directory))))
      (unless (and (eq (plist-get data :kind) 'deleted)
                   (project-frame-sessions--valid-entry-p entry))
        (error "Invalid deleted session metadata"))
      (when (project-frame-sessions--find-entry id entries)
        (error "Session %s is already active" name))
      (when (project-frame-sessions--find-entry-by-name name entries)
        (setq name (project-frame-sessions--read-unique-name
                    (format "Recovered name %S is in use; choose another: " name)
                    name))
        (setq entry (plist-put entry :name name)))
      (when (file-exists-p destination)
        ;; This can only be package-owned, unindexed storage left by a crash
        ;; after the deletion's index switch.  The complete trash copy wins.
        (unless (project-frame-sessions--safe-direct-child-p
                 destination (project-frame-sessions--sessions-directory)
                 "\\`[0-9a-f]\\{64\\}\\'")
          (error "Cannot recover %s: unsafe destination exists" name))
        (delete-directory destination t))
      ;; Keep TRASH authoritative until the index commit.  At every failure or
      ;; crash boundary recovery metadata therefore remains discoverable; the
      ;; startup cleanup reconciles any duplicate destination copy.
      (condition-case err
          (progn
            (make-directory destination t)
            (copy-directory trash destination t t t)
            (project-frame-sessions--failpoint 'after-recovery-copy)
            (delete-file (project-frame-sessions--trash-metadata-file destination))
            (project-frame-sessions--failpoint 'before-recovery-index-write)
            (project-frame-sessions--write-index (cons entry entries))
            (project-frame-sessions--failpoint 'after-recovery-index-write))
        (error
         (when (and (file-directory-p destination)
                    (not (file-symlink-p destination)))
           (ignore-errors (delete-directory destination t)))
         (signal (car err) (cdr err))))
      (project-frame-sessions--failpoint 'before-recovery-trash-cleanup)
      (condition-case err
          (delete-directory trash t)
        (error
         (project-frame-sessions--warn
          "Session %s was recovered, but trash cleanup failed: %s"
          name (error-message-string err))))
      (when frame
        (set-frame-parameter frame project-frame-sessions--id-parameter id)
        (set-frame-parameter frame project-frame-sessions--deleted-parameter nil))
      entry)))

(defun project-frame-sessions--recovery-stages ()
  "Return validated interrupted save candidates."
  (let ((parent (project-frame-sessions--recovery-directory)) result)
    (when (file-directory-p parent)
      (dolist (stage (directory-files parent t directory-files-no-dot-files-regexp))
        (when (project-frame-sessions--safe-direct-child-p
               stage parent "\\`\\.stage-[0-9a-f]\\{64\\}-[0-9a-f]\\{16\\}\\'")
          (condition-case nil
              (let* ((data (project-frame-sessions--read-object
                            (expand-file-name "metadata.eld" stage)))
                     (entry (plist-get data :entry))
                     (target (expand-file-name (plist-get data :target)
                                               project-frame-sessions-directory))
                     (staged (expand-file-name project-frame-sessions--desktop-name
                                               stage))
                     (file (if (file-exists-p target) target staged)))
                (if (and (eq (plist-get data :kind) 'save)
                         (project-frame-sessions--valid-entry-p entry)
                         (project-frame-sessions--valid-snapshot-p
                          (plist-get data :target) (plist-get entry :id))
                         (file-exists-p file)
                         (project-frame-sessions--validate-desktop file)
                         (not (equal (plist-get data :target)
                                     (when-let* ((active
                                                  (project-frame-sessions--find-entry
                                                   (plist-get entry :id))))
                                       (plist-get active :snapshot)))))
                    (progn
                      (setq entry (copy-sequence entry))
                      (setq entry (plist-put entry :stage stage))
                      (setq entry (plist-put entry :recovery-file file))
                      (push entry result))
                  ;; A committed marker or an unpromoted invalid stage is stale.
                  (let ((active (project-frame-sessions--find-entry
                                 (plist-get entry :id))))
                    (when (or (not (file-exists-p target))
                              (equal (plist-get data :target)
                                     (and active (plist-get active :snapshot))))
                      (delete-directory stage t)))))
            (error
             ;; Clearly unreadable package stages are incomplete, not history.
             (ignore-errors (delete-directory stage t)))))))
    result))

(defun project-frame-sessions--cleanup-interrupted-deletions ()
  "Reconcile safe leftovers from interrupted soft deletions.
Caller holds the transaction lock.  An active index entry wins over a duplicate
trash copy; a complete trash entry wins over unindexed active storage."
  (let ((parent (project-frame-sessions--trash-directory))
        (active (project-frame-sessions--read-index)))
    (when (file-directory-p parent)
      (dolist (trash (directory-files parent t directory-files-no-dot-files-regexp))
        (when (project-frame-sessions--safe-direct-child-p
               trash parent "\\`[0-9a-f]\\{64\\}-[0-9a-f]\\{16\\}\\'")
          (condition-case nil
              (let* ((data (project-frame-sessions--read-object
                            (project-frame-sessions--trash-metadata-file trash)))
                     (entry (plist-get data :entry))
                     (id (and entry (plist-get entry :id)))
                     (source (and id (expand-file-name
                                      id (project-frame-sessions--sessions-directory)))))
                (when (and (eq (plist-get data :kind) 'deleted)
                           (project-frame-sessions--valid-entry-p entry))
                  (if (project-frame-sessions--find-entry id active)
                      (delete-directory trash t)
                    (when (and (file-exists-p source)
                               (project-frame-sessions--safe-direct-child-p
                                source (project-frame-sessions--sessions-directory)
                                "\\`[0-9a-f]\\{64\\}\\'"))
                      (delete-directory source t)))))
            (error nil)))))))

(defun project-frame-sessions--scan-recovery ()
  "Scan package transaction artifacts and warn once when recovery is available."
  (project-frame-sessions--validate-managed-roots)
  (project-frame-sessions--with-lock
    (project-frame-sessions--cleanup-interrupted-deletions))
  (let ((candidates (project-frame-sessions--recovery-stages)))
    (when (and candidates (not project-frame-sessions--recovery-warning-shown))
      (setq project-frame-sessions--recovery-warning-shown t)
      (project-frame-sessions--warn
       "%d interrupted session save(s) can be recovered with M-x project-frame-sessions-recover"
       (length candidates)))
    candidates))

(defun project-frame-sessions--recover-stage-entry (candidate)
  "Commit interrupted save CANDIDATE as the active snapshot."
  (project-frame-sessions--with-lock
    (let* ((entry (copy-sequence candidate))
           (stage (plist-get entry :stage))
           (source (plist-get entry :recovery-file))
           (target (project-frame-sessions--entry-snapshot-file entry))
           (entries (project-frame-sessions--read-index))
           (id (plist-get entry :id))
           (name (plist-get entry :name)))
      (setq entry (plist-put entry :stage nil))
      (setq entry (plist-put entry :recovery-file nil))
      (when-let* ((collision (project-frame-sessions--find-entry-by-name name entries)))
        (unless (equal id (plist-get collision :id))
          (setq name (project-frame-sessions--read-unique-name
                      (format "Recovery name %S is in use; choose another: " name)
                      name id))
          (setq entry (plist-put entry :name name))))
      (let ((moved (not (equal source target))))
        (when moved
          (make-directory (file-name-directory target) t)
          (rename-file source target))
        (condition-case err
            (project-frame-sessions--write-index
             (project-frame-sessions--replace-entry entry entries))
          (error
           (when (and moved (file-exists-p target))
             (ignore-errors (rename-file target source)))
           (signal (car err) (cdr err)))))
      (ignore-errors (delete-directory stage t))
      (project-frame-sessions--cleanup-session-snapshots entry)
      entry)))

;;;###autoload
(defun project-frame-sessions-recover ()
  "Recover a soft-deleted session or a validated interrupted save.
Recovery never replaces the committed snapshot automatically; this command is
the explicit user choice to make recovered data active."
  (interactive)
  (let* ((trash (project-frame-sessions--trash-entries))
         (stages (project-frame-sessions--scan-recovery))
         choices)
    (dolist (entry trash)
      (push (cons (project-frame-sessions--candidate-label entry 'deleted)
                  (cons 'trash entry)) choices))
    (dolist (entry stages)
      (push (cons (project-frame-sessions--candidate-label entry 'recovery)
                  (cons 'stage entry)) choices))
    (unless choices (user-error "No deleted or interrupted sessions to recover"))
    (let* ((selection (completing-read "Recover frame session: " choices nil t))
           (candidate (cdr (assoc selection choices)))
           (entry (pcase (car candidate)
                    ('trash (project-frame-sessions--recover-trash-entry
                             (plist-get (cdr candidate) :trash)))
                    ('stage (project-frame-sessions--recover-stage-entry
                             (cdr candidate))))))
      (message "Recovered frame session %s" (plist-get entry :name))
      entry)))

;;;###autoload
(defun project-frame-sessions-purge-deleted ()
  "Permanently remove all soft-deleted session data after confirmation."
  (interactive)
  (let ((entries (project-frame-sessions--trash-entries)))
    (unless entries (user-error "No soft-deleted frame sessions"))
    (when (yes-or-no-p
           (format "Permanently purge %d deleted session(s)?  This cannot be undone; continue? "
                   (length entries)))
      (project-frame-sessions--with-lock
        (dolist (entry entries)
          (let ((trash (plist-get entry :trash)))
            (unless (project-frame-sessions--safe-direct-child-p
                     trash (project-frame-sessions--trash-directory)
                     "\\`[0-9a-f]\\{64\\}-[0-9a-f]\\{16\\}\\'")
              (error "Refusing unsafe trash path %s" trash))
            (delete-directory trash t)
            (let ((source (expand-file-name
                           (plist-get entry :id)
                           (project-frame-sessions--sessions-directory))))
              (when (and (file-exists-p source)
                         (project-frame-sessions--safe-direct-child-p
                          source (project-frame-sessions--sessions-directory)
                          "\\`[0-9a-f]\\{64\\}\\'"))
                (delete-directory source t))))))
      (message "Permanently purged %d deleted session(s)" (length entries)))))

(defun project-frame-sessions--delete-frame-hook (frame)
  "Save enrolled FRAME and schedule one post-deletion finalizer.
Cleanup is deferred so later deletion hooks can still veto the close.  Emacs
may invoke `delete-frame-functions' recursively, so repeated calls for the
same pending close are ignored."
  (when (and project-frame-sessions-mode
             (not project-frame-sessions--shutdown)
             (not (eq frame project-frame-sessions--delete-suppressed))
             (project-frame-sessions--frame-id frame)
             (not (gethash frame project-frame-sessions--pending-frame-closes)))
    (puthash frame t project-frame-sessions--pending-frame-closes)
    (condition-case err
        (let ((buffers
               (progn
                 (project-frame-sessions--save-frame-internal frame t)
                 (unless project-frame-sessions-preserve-buffers-after-frame-close
                   (project-frame-sessions--frame-close-candidates frame)))))
          ;; Schedule even under the preserve policy so a vetoed close clears
          ;; the recursion guard and a later close can save normally.
          (run-at-time 0 nil #'project-frame-sessions--finalize-frame-close
                       frame buffers))
      ((error quit)
       (remhash frame project-frame-sessions--pending-frame-closes)
       (signal (car err) (cdr err))))))

;;;###autoload
(defun project-frame-sessions-delete-frame (&optional frame before-delete)
  "Save and close enrolled FRAME.
After saving, call optional BEFORE-DELETE with FRAME before closing it.  A
failed save, callback, or deletion leaves its buffers alive.  On successful
close, buffer retention follows
`project-frame-sessions-preserve-buffers-after-frame-close'."
  (interactive)
  (let ((frame (or frame (selected-frame))))
    (unless (project-frame-sessions--frame-id frame)
      (user-error "Frame is not enrolled in a session"))
    (project-frame-sessions--save-frame-internal frame t)
    (let ((buffers (unless project-frame-sessions-preserve-buffers-after-frame-close
                     (project-frame-sessions--frame-close-candidates frame))))
      (when before-delete
        (funcall before-delete frame))
      (let ((project-frame-sessions--delete-suppressed frame))
        (delete-frame frame))
      (unless (frame-live-p frame)
        (project-frame-sessions--dispose-closed-frame-buffers buffers)))))

(defun project-frame-sessions--dirty-frames ()
  "Return enrolled live frames whose sessions are dirty."
  (cl-remove-if-not
   (lambda (frame)
     (when-let* ((id (project-frame-sessions--frame-id frame))
                 (runtime (project-frame-sessions--runtime id)))
       (> (project-frame-sessions--runtime-dirty runtime)
          (project-frame-sessions--runtime-saved runtime))))
   (frame-list)))

(defun project-frame-sessions--shutdown-attempt ()
  "Save all dirty enrolled sessions and return failure (NAME . ERROR) pairs."
  (let (failures)
    (dolist (frame (project-frame-sessions--dirty-frames))
      (let* ((id (project-frame-sessions--frame-id frame))
             (name (project-frame-sessions--entry-name id)))
        (condition-case err
            (project-frame-sessions--save-frame-internal frame t)
          (error (push (cons name (error-message-string err)) failures)))))
    (nreverse failures)))

(defun project-frame-sessions--query-exit ()
  "Strict shutdown preflight with retry, cancel, and deliberate override."
  (let ((done nil) (allow nil))
    (while (not done)
      (let ((failures (project-frame-sessions--shutdown-attempt)))
        (if (null failures)
            (setq done t allow t project-frame-sessions--preflight-complete t)
          (project-frame-sessions--warn
           "Shutdown save failed: %s. Last committed snapshots remain safe."
           (mapconcat (lambda (failure)
                        (format "%s: %s" (car failure) (cdr failure)))
                      failures "; "))
          (pcase (read-char-choice
                  "Session saves failed: [r]etry, [c]ancel shutdown, or [q]uit without saving? "
                  '(?r ?c ?q))
            (?r nil)
            (?c (setq done t allow nil))
            (?q (if (yes-or-no-p
                     "Really quit without saving these dirty frame sessions? ")
                    (setq done t allow t
                          project-frame-sessions--preflight-complete t)
                  nil))))))
    allow))

(defun project-frame-sessions--exit ()
  "Best-effort final shutdown fallback."
  (setq project-frame-sessions--shutdown t)
  (when (timerp project-frame-sessions--idle-timer)
    (cancel-timer project-frame-sessions--idle-timer))
  (unless project-frame-sessions--preflight-complete
    (dolist (frame (project-frame-sessions--dirty-frames))
      (condition-case err
          (project-frame-sessions--save-frame-internal frame t)
        (error
         (project-frame-sessions--warn
          "Final save for %s failed: %s"
          (project-frame-sessions--entry-name
           (project-frame-sessions--frame-id frame))
          (error-message-string err)))))))

(defun project-frame-sessions--install-hooks ()
  "Install dirty, close, and shutdown hooks idempotently."
  (add-hook 'delete-frame-functions #'project-frame-sessions--delete-frame-hook)
  (add-hook 'kill-emacs-query-functions #'project-frame-sessions--query-exit)
  (add-hook 'kill-emacs-hook #'project-frame-sessions--exit)
  (add-hook 'buffer-list-update-hook #'project-frame-sessions--event-dirty)
  (add-hook 'window-state-change-functions #'project-frame-sessions--event-dirty)
  (when (boundp 'tab-bar-tab-post-open-functions)
    (add-hook 'tab-bar-tab-post-open-functions #'project-frame-sessions--event-dirty))
  (when (boundp 'tab-bar-tab-post-select-functions)
    (add-hook 'tab-bar-tab-post-select-functions #'project-frame-sessions--event-dirty))
  (advice-add 'tab-bar-close-tab :after #'project-frame-sessions--event-dirty)
  (advice-add 'tab-bar-rename-tab :after #'project-frame-sessions--event-dirty))

(defun project-frame-sessions--remove-hooks ()
  "Remove all package hooks and advice idempotently."
  (remove-hook 'delete-frame-functions #'project-frame-sessions--delete-frame-hook)
  (remove-hook 'kill-emacs-query-functions #'project-frame-sessions--query-exit)
  (remove-hook 'kill-emacs-hook #'project-frame-sessions--exit)
  (remove-hook 'buffer-list-update-hook #'project-frame-sessions--event-dirty)
  (remove-hook 'window-state-change-functions #'project-frame-sessions--event-dirty)
  (when (boundp 'tab-bar-tab-post-open-functions)
    (remove-hook 'tab-bar-tab-post-open-functions
                 #'project-frame-sessions--event-dirty))
  (when (boundp 'tab-bar-tab-post-select-functions)
    (remove-hook 'tab-bar-tab-post-select-functions
                 #'project-frame-sessions--event-dirty))
  (advice-remove 'tab-bar-close-tab #'project-frame-sessions--event-dirty)
  (advice-remove 'tab-bar-rename-tab #'project-frame-sessions--event-dirty))

;;;###autoload
(define-minor-mode project-frame-sessions-mode
  "Globally maintain explicitly enrolled graphical frame sessions.
The mode never restores at startup and never enrolls an ordinary non-project
frame merely because the mode is enabled."
  :global t
  (project-frame-sessions--remove-hooks)
  (project-frame-sessions--remove-desktop-integrations)
  (when (timerp project-frame-sessions--idle-timer)
    (cancel-timer project-frame-sessions--idle-timer))
  (setq project-frame-sessions--idle-timer nil
        project-frame-sessions--shutdown nil
        project-frame-sessions--preflight-complete nil)
  (clrhash project-frame-sessions--pending-frame-closes)
  (maphash (lambda (_id runtime)
             (project-frame-sessions--cancel-runtime-timer runtime))
           project-frame-sessions--runtimes)
  (when project-frame-sessions-mode
    (condition-case err
        (progn
          (project-frame-sessions--validate-configuration)
          (project-frame-sessions--validate-managed-roots))
      (error
       (setq project-frame-sessions-mode nil)
       (signal (car err) (cdr err))))
    (project-frame-sessions--install-hooks)
    (project-frame-sessions--register-desktop-integrations)
    (project-frame-sessions--scan-recovery)
    (maphash
     (lambda (id runtime)
       (when (> (project-frame-sessions--runtime-dirty runtime)
                (project-frame-sessions--runtime-saved runtime))
         (project-frame-sessions--schedule id)))
     project-frame-sessions--runtimes)
    (when project-frame-sessions-autosave-interval
      (setq project-frame-sessions--idle-timer
            (run-with-idle-timer project-frame-sessions-autosave-interval t
                                 #'project-frame-sessions--idle-save)))))

(provide 'project-frame-sessions)
;;; project-frame-sessions.el ends here
