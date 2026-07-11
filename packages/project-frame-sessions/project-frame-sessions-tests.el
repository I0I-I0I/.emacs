;;; project-frame-sessions-tests.el --- Tests for frame sessions -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'project-frame-sessions)

(defmacro project-frame-sessions-tests--with-store (&rest body)
  (declare (indent 0) (debug t))
  `(let* ((directory (make-temp-file "pfs-test-" t))
          (project-frame-sessions-directory (file-name-as-directory directory))
          (project-frame-sessions-warn-function #'ignore)
          (project-frame-sessions--runtimes (make-hash-table :test #'equal))
          (project-frame-sessions--pending (make-hash-table :test #'eq))
          (project-frame-sessions--recovery-warning-shown nil))
     (unwind-protect (progn ,@body)
       (ignore-errors (delete-directory directory t)))))

(defmacro project-frame-sessions-tests--preserve-frame (&rest body)
  (declare (indent 0) (debug t))
  `(let* ((frame (selected-frame))
          (old-id (frame-parameter frame project-frame-sessions--id-parameter))
          (old-deleted (frame-parameter frame project-frame-sessions--deleted-parameter))
          (old-root (frame-parameter frame 'project-frame-sessions-root))
          (old-directory (frame-parameter frame 'default-directory)))
     (unwind-protect (progn ,@body)
       (set-frame-parameter frame project-frame-sessions--id-parameter old-id)
       (set-frame-parameter frame project-frame-sessions--deleted-parameter old-deleted)
       (set-frame-parameter frame 'project-frame-sessions-root old-root)
       (set-frame-parameter frame 'default-directory old-directory))))

(defun project-frame-sessions-tests--id (&optional character)
  (make-string 64 (or character ?a)))

(defun project-frame-sessions-tests--entry (&optional id name root token saved-at)
  (let ((id (or id (project-frame-sessions-tests--id))))
    (list :id id :name (or name "Work") :root root
          :snapshot (format "sessions/%s/desktop-%s.el" id (or token "1111111111111111"))
          :saved-at (or saved-at 1.0) :state 'active)))

(defun project-frame-sessions-tests--put-snapshot (entry &optional contents)
  (let ((file (project-frame-sessions--entry-snapshot-file entry)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file (insert (or contents "snapshot")))
    file))

(defun project-frame-sessions-tests--fake-desktop-save (stage &rest _)
  (with-temp-file (expand-file-name project-frame-sessions--desktop-name stage)
    (insert "snapshot")))

(ert-deftest project-frame-sessions-new-ids-are-stable-shaped-and-unique ()
  (let ((a (project-frame-sessions--new-id))
        (b (project-frame-sessions--new-id)))
    (should (project-frame-sessions--valid-id-p a))
    (should (project-frame-sessions--valid-id-p b))
    (should-not (equal a b))))

(ert-deftest project-frame-sessions-index-round-trip-with-optional-roots ()
  (project-frame-sessions-tests--with-store
    (let ((a (project-frame-sessions-tests--entry nil "Rootless" nil))
          (b (project-frame-sessions-tests--entry
              (project-frame-sessions-tests--id ?b) "Rooted" "/tmp/" "2222222222222222" 2)))
      (project-frame-sessions--write-index (list a b))
      (should (equal (project-frame-sessions--read-index) (list a b))))))

(ert-deftest project-frame-sessions-index-rejects-old-and-corrupt-schema ()
  (project-frame-sessions-tests--with-store
    (dolist (contents '("(:version 2 :entries nil)"
                        "(:version 1 :entries nil) trailing"
                        "(:version 1 :entries ((:id \"bad\" :name \"X\" :root nil :snapshot \"x\" :saved-at 1 :state active)))"))
      (with-temp-file (project-frame-sessions--index-file) (insert contents))
      (should-error (project-frame-sessions--read-index)))))

(ert-deftest project-frame-sessions-index-rejects-duplicate-active-names ()
  (project-frame-sessions-tests--with-store
    (let ((a (project-frame-sessions-tests--entry nil "Same"))
          (b (project-frame-sessions-tests--entry
              (project-frame-sessions-tests--id ?b) "Same" nil "2222222222222222")))
      (should-error (project-frame-sessions--write-index (list a b))))))

(ert-deftest project-frame-sessions-default-discovery-does-not-require-project ()
  (cl-letf (((symbol-function 'require)
             (lambda (feature &optional _filename _noerror)
               (unless (eq feature 'project) (error "unexpected require"))
               nil)))
    (should-not (project-frame-sessions--default-discovery))))

(ert-deftest project-frame-sessions-manual-first-save-works-without-root ()
  (project-frame-sessions-tests--with-store
    (project-frame-sessions-tests--preserve-frame
      (let ((frame (selected-frame)))
        (set-frame-parameter frame project-frame-sessions--id-parameter nil)
        (cl-letf (((symbol-function 'project-frame-sessions--graphical-frame-p) (lambda (_) t))
                  ((symbol-function 'project-frame-sessions--discover-root) #'ignore)
                  ((symbol-function 'project-frame-sessions--generic-frame-name-p) (lambda (_) nil))
                  ((symbol-function 'project-frame-sessions--save-desktop-to-stage)
                   (lambda (_frame stage)
                     (project-frame-sessions-tests--fake-desktop-save stage)))
                  ((symbol-function 'project-frame-sessions--validate-desktop) (lambda (_) t)))
          (let ((entry (project-frame-sessions-save frame)))
            (should (null (plist-get entry :root)))
            (should (equal (project-frame-sessions--frame-id frame)
                           (plist-get entry :id)))
            (should (file-exists-p
                     (project-frame-sessions--entry-snapshot-file entry)))))))))

(ert-deftest project-frame-sessions-project-first-save-uses-project-name-without-prompt ()
  (project-frame-sessions-tests--with-store
    (let ((frame 'frame) prompted)
      (cl-letf (((symbol-function 'project-frame-sessions--discover-root)
                 (lambda (_) "/tmp/example-project/"))
                ((symbol-function 'frame-parameter) (lambda (&rest _) "GNU Emacs"))
                ((symbol-function 'read-string) (lambda (&rest _) (setq prompted t))))
        (should (equal (project-frame-sessions--first-save-metadata frame)
                       '(:name "example-project" :root "/tmp/example-project/")))
        (should-not prompted)))))

(ert-deftest project-frame-sessions-first-save-failure-does-not-assign-id ()
  (project-frame-sessions-tests--with-store
    (project-frame-sessions-tests--preserve-frame
      (let ((frame (selected-frame)))
        (set-frame-parameter frame project-frame-sessions--id-parameter nil)
        (cl-letf (((symbol-function 'project-frame-sessions--graphical-frame-p) (lambda (_) t))
                  ((symbol-function 'project-frame-sessions--first-save-metadata)
                   (lambda (_) '(:name "Pending" :root nil)))
                  ((symbol-function 'project-frame-sessions--save-desktop-to-stage)
                   (lambda (&rest _) (error "forced failure"))))
          (should-error (project-frame-sessions--save-frame-internal frame t))
          (should-not (project-frame-sessions--frame-id frame))
          (should (gethash frame project-frame-sessions--pending)))))))

(ert-deftest project-frame-sessions-failed-first-autosave-retries-pending-frame ()
  (project-frame-sessions-tests--with-store
    (project-frame-sessions-tests--preserve-frame
      (let ((frame (selected-frame)) (attempts 0) scheduled)
        (set-frame-parameter frame project-frame-sessions--id-parameter nil)
        (cl-letf (((symbol-function 'project-frame-sessions--graphical-frame-p)
                   (lambda (_) t))
                  ((symbol-function 'project-frame-sessions--first-save-metadata)
                   (lambda (_) '(:name "Pending" :root "/tmp/")))
                  ((symbol-function 'project-frame-sessions--save-desktop-to-stage)
                   (lambda (_frame stage)
                     (cl-incf attempts)
                     (if (= attempts 1)
                         (error "first attempt failed")
                       (project-frame-sessions-tests--fake-desktop-save stage))))
                  ((symbol-function 'project-frame-sessions--validate-desktop)
                   (lambda (_) t))
                  ((symbol-function 'project-frame-sessions--schedule)
                   (lambda (id &optional delay) (setq scheduled (list id delay)))))
          (should-error (project-frame-sessions--save-frame-internal frame nil))
          (let* ((pending (gethash frame project-frame-sessions--pending))
                 (id (plist-get pending :id))
                 (runtime (project-frame-sessions--runtime id))
                 (project-frame-sessions-mode t))
            (should (= 1 (project-frame-sessions--runtime-dirty runtime)))
            (should (equal scheduled (list id 5)))
            ;; Simulate the retry timer firing at its due time.
            (setf (project-frame-sessions--runtime-next-retry runtime) nil)
            (project-frame-sessions--autosave-id id)
            (should (= attempts 2))
            (should (equal (project-frame-sessions--frame-id frame) id))
            (should (= (project-frame-sessions--runtime-dirty runtime)
                       (project-frame-sessions--runtime-saved runtime)))))))))

(ert-deftest project-frame-sessions-event-uses-supplied-frame ()
  (let ((project-frame-sessions-mode t) marked)
    (cl-letf (((symbol-function 'project-frame-sessions--mark-frame-dirty)
               (lambda (frame) (setq marked frame))))
      (project-frame-sessions--event-dirty (selected-frame))
      (should (eq marked (selected-frame))))))

(ert-deftest project-frame-sessions-frame-identity-does-not-follow-directory ()
  (project-frame-sessions-tests--with-store
    (project-frame-sessions-tests--preserve-frame
      (let* ((frame (selected-frame))
             (entry (project-frame-sessions-tests--entry nil "Stable" "/tmp/")))
        (project-frame-sessions--write-index (list entry))
        (set-frame-parameter frame project-frame-sessions--id-parameter
                             (plist-get entry :id))
        (set-frame-parameter frame 'default-directory "/var/")
        (cl-letf (((symbol-function 'project-frame-sessions--discover-root)
                   (lambda (_) "/different/")))
          (should (equal (plist-get (project-frame-sessions--frame-entry frame) :root)
                         "/tmp/")))))))

(ert-deftest project-frame-sessions-buffer-predicate-honors-frame-and-desktop-filter ()
  (let ((wanted (generate-new-buffer " pfs-wanted"))
        (other (generate-new-buffer " pfs-other")) called)
    (unwind-protect
        (let ((predicate
               (project-frame-sessions--buffer-save-predicate
                (list wanted)
                (lambda (file name mode)
                  (setq called (list file name mode)) t))))
          (should (funcall predicate "f" (buffer-name wanted) 'text-mode))
          (should (equal called (list "f" (buffer-name wanted) 'text-mode)))
          (should-not (funcall predicate nil (buffer-name other) 'fundamental-mode)))
      (kill-buffer wanted)
      (kill-buffer other))))

(ert-deftest project-frame-sessions-tab-buffers-include-hidden-tabspaces-associations ()
  (let ((current (generate-new-buffer " pfs-current"))
        (hidden (generate-new-buffer " pfs-hidden"))
        (workspace (generate-new-buffer " pfs-workspace")))
    (unwind-protect
        (cl-letf (((symbol-function 'frame-parameter)
                   (lambda (_frame parameter)
                     (pcase parameter
                       ('tabs `((current-tab (name . "One"))
                                (tab (name . "Two") (wc-bl . (,hidden))
                                     (ws (tabspaces--buffer-list (,workspace))))))
                       (_ nil)))))
          (let ((project-frame-sessions-frame-buffer-function
                 (lambda (_) (list current))))
            (let ((buffers (project-frame-sessions--tab-buffers 'frame)))
              (should (memq current buffers))
              (should (memq hidden buffers))
              (should (memq workspace buffers)))))
      (mapc #'kill-buffer (list current hidden workspace)))))

(ert-deftest project-frame-sessions-filter-tabs-keeps-one-current-tab ()
  (let ((project-frame-sessions-tab-omit-function
         (lambda (tab) (equal (alist-get 'name tab) "omit"))))
    (let ((parameter (project-frame-sessions--filter-tabs
                      '(tabs (tab (name . "keep") (wc . unsafe))
                             (current-tab (name . "omit")))
                      nil nil t)))
      (should (eq (car parameter) 'tabs))
      (should (= (length (cdr parameter)) 1))
      (should (eq (caadr parameter) 'current-tab))
      (should-not (assq 'wc (cadr parameter))))))

(ert-deftest project-frame-sessions-failed-save-preserves-last-committed-snapshot ()
  (project-frame-sessions-tests--with-store
    (project-frame-sessions-tests--preserve-frame
      (let* ((frame (selected-frame))
             (entry (project-frame-sessions-tests--entry))
             (old-file (project-frame-sessions-tests--put-snapshot entry "old")))
        (project-frame-sessions--write-index (list entry))
        (set-frame-parameter frame project-frame-sessions--id-parameter
                             (plist-get entry :id))
        (cl-letf (((symbol-function 'project-frame-sessions--graphical-frame-p) (lambda (_) t))
                  ((symbol-function 'project-frame-sessions--save-desktop-to-stage)
                   (lambda (&rest _) (error "failed before commit"))))
          (should-error (project-frame-sessions--save-frame-internal frame t))
          (should (equal (project-frame-sessions--read-index) (list entry)))
          (should (equal (with-temp-buffer
                           (insert-file-contents old-file)
                           (buffer-string)) "old")))))))

(ert-deftest project-frame-sessions-commit-index-failure-leaves-recoverable-new-snapshot ()
  (project-frame-sessions-tests--with-store
    (let* ((id (project-frame-sessions-tests--id))
           (entry (list :id id :name "Recoverable" :root nil))
           (token "2222222222222222")
           (stage (project-frame-sessions--stage-directory id token))
           (frame (selected-frame)))
      (make-directory stage t)
      (project-frame-sessions-tests--fake-desktop-save stage)
      (cl-letf (((symbol-function 'project-frame-sessions--write-index)
                 (lambda (_) (error "index failed"))))
        (should-error (project-frame-sessions--commit-save frame entry stage token)))
      (should (file-exists-p (expand-file-name "metadata.eld" stage)))
      (should (file-exists-p
               (expand-file-name (format "sessions/%s/desktop-%s.el" id token)
                                 project-frame-sessions-directory))))))

(ert-deftest project-frame-sessions-retry-delay-is-bounded ()
  (let ((project-frame-sessions-retry-delays '(5 15 45)))
    (should (= 5 (project-frame-sessions--retry-delay 1)))
    (should (= 15 (project-frame-sessions--retry-delay 2)))
    (should (= 45 (project-frame-sessions--retry-delay 3)))
    (should (= 45 (project-frame-sessions--retry-delay 30)))))

(ert-deftest project-frame-sessions-autosave-failure-keeps-dirty-and-schedules-retry ()
  (project-frame-sessions-tests--with-store
    (project-frame-sessions-tests--preserve-frame
      (let* ((frame (selected-frame))
             (entry (project-frame-sessions-tests--entry))
             (id (plist-get entry :id))
             (runtime (project-frame-sessions--runtime id))
             scheduled)
        (project-frame-sessions--write-index (list entry))
        (project-frame-sessions-tests--put-snapshot entry)
        (set-frame-parameter frame project-frame-sessions--id-parameter id)
        (setf (project-frame-sessions--runtime-dirty runtime) 1
              (project-frame-sessions--runtime-dirty-since runtime) (float-time))
        (cl-letf (((symbol-function 'project-frame-sessions--graphical-frame-p) (lambda (_) t))
                  ((symbol-function 'project-frame-sessions--save-desktop-to-stage)
                   (lambda (&rest _) (error "autosave failure")))
                  ((symbol-function 'project-frame-sessions--schedule)
                   (lambda (session &optional delay) (setq scheduled (list session delay)))))
          (should-error (project-frame-sessions--save-frame-internal frame nil))
          (should (= 1 (project-frame-sessions--runtime-failures runtime)))
          (should (project-frame-sessions--runtime-dirty-since runtime))
          (should (equal scheduled (list id 5))))))))

(ert-deftest project-frame-sessions-change-during-save-remains-dirty ()
  (project-frame-sessions-tests--with-store
    (project-frame-sessions-tests--preserve-frame
      (let* ((frame (selected-frame))
             (entry (project-frame-sessions-tests--entry))
             (id (plist-get entry :id))
             (runtime (project-frame-sessions--runtime id)) scheduled)
        (project-frame-sessions--write-index (list entry))
        (project-frame-sessions-tests--put-snapshot entry)
        (set-frame-parameter frame project-frame-sessions--id-parameter id)
        (setf (project-frame-sessions--runtime-dirty runtime) 1)
        (cl-letf (((symbol-function 'project-frame-sessions--graphical-frame-p) (lambda (_) t))
                  ((symbol-function 'project-frame-sessions--save-desktop-to-stage)
                   (lambda (_target stage)
                     (cl-incf (project-frame-sessions--runtime-dirty runtime))
                     (project-frame-sessions-tests--fake-desktop-save stage)))
                  ((symbol-function 'project-frame-sessions--validate-desktop) (lambda (_) t))
                  ((symbol-function 'project-frame-sessions--schedule)
                   (lambda (&rest _) (setq scheduled t))))
          (project-frame-sessions--save-frame-internal frame nil)
          (should (= 1 (project-frame-sessions--runtime-saved runtime)))
          (should (= 2 (project-frame-sessions--runtime-dirty runtime)))
          (should scheduled))))))

(ert-deftest project-frame-sessions-restore-selects-before-creating-frame ()
  (let (order)
    (cl-letf (((symbol-function 'project-frame-sessions--select-active)
               (lambda (_) (push 'select order)
                 (project-frame-sessions-tests--entry)))
              ((symbol-function 'project-frame-sessions--find-live-frame) #'ignore)
              ((symbol-function 'make-frame) (lambda () (push 'make order) 'new))
              ((symbol-function 'project-frame-sessions--restore-entry)
               (lambda (&rest _) (push 'restore order) 'new)))
      (project-frame-sessions-restore)
      (should (equal (nreverse order) '(select make restore))))))

(ert-deftest project-frame-sessions-open-session-is-focused-without-new-frame ()
  (let ((owner 'owner) focused made)
    (cl-letf (((symbol-function 'project-frame-sessions--select-active)
               (lambda (_) (project-frame-sessions-tests--entry)))
              ((symbol-function 'project-frame-sessions--find-live-frame) (lambda (&rest _) owner))
              ((symbol-function 'select-frame-set-input-focus) (lambda (frame) (setq focused frame)))
              ((symbol-function 'make-frame) (lambda () (setq made t))))
      (should (eq (project-frame-sessions-restore) owner))
      (should (eq focused owner))
      (should-not made))))

(ert-deftest project-frame-sessions-new-frame-is-deleted-after-restore-failure ()
  (let ((fresh 'fresh) deleted)
    (cl-letf (((symbol-function 'project-frame-sessions--select-active)
               (lambda (_) (project-frame-sessions-tests--entry)))
              ((symbol-function 'project-frame-sessions--find-live-frame) #'ignore)
              ((symbol-function 'make-frame) (lambda () fresh))
              ((symbol-function 'project-frame-sessions--restore-entry)
               (lambda (&rest _) (error "restore failed")))
              ((symbol-function 'frame-live-p) (lambda (_) t))
              ((symbol-function 'delete-frame) (lambda (frame &rest _) (setq deleted frame))))
      (should-error (project-frame-sessions-restore))
      (should (eq deleted fresh)))))

(ert-deftest project-frame-sessions-rename-keeps-id-and-snapshot ()
  (project-frame-sessions-tests--with-store
    (project-frame-sessions-tests--preserve-frame
      (let* ((frame (selected-frame))
             (entry (project-frame-sessions-tests--entry)))
        (project-frame-sessions--write-index (list entry))
        (set-frame-parameter frame project-frame-sessions--id-parameter
                             (plist-get entry :id))
        (project-frame-sessions-rename "Renamed")
        (let ((renamed (car (project-frame-sessions--read-index))))
          (should (equal (plist-get renamed :id) (plist-get entry :id)))
          (should (equal (plist-get renamed :snapshot) (plist-get entry :snapshot)))
          (should (equal (plist-get renamed :name) "Renamed")))))))

(ert-deftest project-frame-sessions-rename-rejects-name-conflict ()
  (project-frame-sessions-tests--with-store
    (project-frame-sessions-tests--preserve-frame
      (let ((a (project-frame-sessions-tests--entry))
            (b (project-frame-sessions-tests--entry
                (project-frame-sessions-tests--id ?b) "Other" nil "2222222222222222")))
        (project-frame-sessions--write-index (list a b))
        (set-frame-parameter (selected-frame) project-frame-sessions--id-parameter
                             (plist-get a :id))
        (should-error (project-frame-sessions-rename "Other") :type 'user-error)))))

(ert-deftest project-frame-sessions-soft-delete-detaches-live-frame-and-is-recoverable ()
  (project-frame-sessions-tests--with-store
    (project-frame-sessions-tests--preserve-frame
      (let* ((frame (selected-frame))
             (entry (project-frame-sessions-tests--entry))
             (id (plist-get entry :id)))
        (project-frame-sessions--write-index (list entry))
        (project-frame-sessions-tests--put-snapshot entry)
        (set-frame-parameter frame project-frame-sessions--id-parameter id)
        (let ((trash (project-frame-sessions--soft-delete-entry entry)))
          (should-not (project-frame-sessions--read-index))
          (should-not (project-frame-sessions--frame-id frame))
          (should (frame-parameter frame project-frame-sessions--deleted-parameter))
          (should (file-directory-p trash))
          (let ((recovered (project-frame-sessions--recover-trash-entry trash frame)))
            (should (equal (plist-get recovered :id) id))
            (should (equal (project-frame-sessions--frame-id frame) id))
            (should (= 1 (length (project-frame-sessions--read-index))))))))))

(ert-deftest project-frame-sessions-soft-delete-index-failure-keeps-active-source ()
  (project-frame-sessions-tests--with-store
    (let* ((entry (project-frame-sessions-tests--entry))
           (source (file-name-directory
                    (project-frame-sessions-tests--put-snapshot entry))))
      (project-frame-sessions--write-index (list entry))
      (cl-letf (((symbol-function 'project-frame-sessions--write-index)
                 (lambda (_) (error "index failure"))))
        (should-error (project-frame-sessions--soft-delete-entry entry)))
      (should (file-directory-p source))
      (should (equal (project-frame-sessions--read-index) (list entry)))
      (should-not (project-frame-sessions--trash-entries)))))

(ert-deftest project-frame-sessions-recovery-index-failure-keeps-trash-discoverable ()
  (project-frame-sessions-tests--with-store
    (let* ((entry (project-frame-sessions-tests--entry))
           (id (plist-get entry :id)))
      (project-frame-sessions--write-index (list entry))
      (project-frame-sessions-tests--put-snapshot entry)
      (let ((trash (project-frame-sessions--soft-delete-entry entry)))
        (cl-letf (((symbol-function 'project-frame-sessions--write-index)
                   (lambda (_) (error "index failure"))))
          (should-error (project-frame-sessions--recover-trash-entry trash)))
        (should (= 1 (length (project-frame-sessions--trash-entries))))
        (should (file-exists-p (project-frame-sessions--trash-metadata-file trash)))
        (should-not (file-exists-p
                     (expand-file-name id
                                       (project-frame-sessions--sessions-directory))))))))

(ert-deftest project-frame-sessions-stale-ownerless-lock-is-reclaimed ()
  (project-frame-sessions-tests--with-store
    (let ((lock (project-frame-sessions--lock-directory))
          (project-frame-sessions-ownerless-lock-stale-seconds 0))
      (make-directory lock t)
      (should (equal (project-frame-sessions--acquire-lock) lock))
      (should (file-exists-p (project-frame-sessions--lock-owner-file lock)))
      (delete-directory lock t))))

(ert-deftest project-frame-sessions-fresh-ownerless-lock-is-not-reclaimed ()
  (project-frame-sessions-tests--with-store
    (let ((lock (project-frame-sessions--lock-directory))
          (project-frame-sessions-ownerless-lock-stale-seconds 60))
      (make-directory lock t)
      (should-error (project-frame-sessions--acquire-lock))
      (should (file-directory-p lock))
      (should-not (file-exists-p (project-frame-sessions--lock-owner-file lock))))))

(ert-deftest project-frame-sessions-commit-refuses-symlink-session-directory ()
  (project-frame-sessions-tests--with-store
    (let* ((id (project-frame-sessions-tests--id))
           (entry (list :id id :name "Unsafe" :root nil))
           (token "2222222222222222")
           (stage (project-frame-sessions--stage-directory id token))
           (session-directory
            (expand-file-name id (project-frame-sessions--sessions-directory)))
           (outside (make-temp-file "pfs-outside-" t)))
      (unwind-protect
          (progn
            (make-directory stage t)
            (project-frame-sessions-tests--fake-desktop-save stage)
            (make-directory (project-frame-sessions--sessions-directory) t)
            (make-symbolic-link outside session-directory)
            (should-error
             (project-frame-sessions--commit-save
              (selected-frame) entry stage token))
            (should-not
             (file-exists-p (expand-file-name
                             (format "desktop-%s.el" token) outside))))
        (ignore-errors (delete-file session-directory))
        (ignore-errors (delete-directory outside t))))))

(ert-deftest project-frame-sessions-read-desktop-captures-frameset-in-target-frame ()
  (project-frame-sessions-tests--with-store
    (let* ((entry (project-frame-sessions-tests--entry))
           (snapshot (project-frame-sessions-tests--put-snapshot entry))
           (directory (file-name-directory snapshot))
           (target (selected-frame)))
      (cl-letf (((symbol-function 'desktop-owner) (lambda (&optional _) nil))
                ((symbol-function 'desktop-read)
                 (lambda (read-directory &optional _)
                   (should (eq (selected-frame) target))
                   (should (equal desktop-dirname directory))
                   (should (equal read-directory directory))
                   (setq desktop-saved-frameset
                         (frameset-save (list (selected-frame))))
                   ;; Emacs 32 restores/captures the frameset, then clears the
                   ;; singleton before `desktop-read' returns.
                   (desktop-restore-frameset)
                   (setq desktop-saved-frameset nil)))
                ((symbol-function 'desktop-release-lock) #'ignore))
        (should (frameset-p
                 (project-frame-sessions--read-desktop-frameset
                  entry target)))))))

(ert-deftest project-frame-sessions-read-desktop-accepts-tabs-in-one-frame-state ()
  (project-frame-sessions-tests--with-store
    (let* ((entry (project-frame-sessions-tests--entry))
           (_snapshot (project-frame-sessions-tests--put-snapshot entry))
           (frame (selected-frame))
           (old-tabs (frame-parameter frame 'tabs)))
      (unwind-protect
          (progn
            (set-frame-parameter
             frame 'tabs
             '((current-tab (name . "One"))
               (tab (name . "Two")
                    (ws (tabspaces--buffer-list ("one" "two"))))))
            (cl-letf (((symbol-function 'desktop-owner) (lambda (&optional _) nil))
                      ((symbol-function 'desktop-read)
                       (lambda (&rest _)
                         (setq desktop-saved-frameset
                               (frameset-save (list frame)))
                         (desktop-restore-frameset)
                         (setq desktop-saved-frameset nil)))
                      ((symbol-function 'desktop-release-lock) #'ignore))
              (let ((frameset
                     (project-frame-sessions--read-desktop-frameset entry frame)))
                (should (= 1 (length (frameset-states frameset)))))))
        (set-frame-parameter frame 'tabs old-tabs)))))

(ert-deftest project-frame-sessions-delete-frame-runs-save-callback-delete-in-order ()
  (let (order suppressed)
    (cl-letf (((symbol-function 'project-frame-sessions--frame-id) (lambda (_) "id"))
              ((symbol-function 'project-frame-sessions--save-frame-internal)
               (lambda (&rest _) (push 'save order)))
              ((symbol-function 'delete-frame)
               (lambda (_frame &rest _)
                 (setq suppressed project-frame-sessions--delete-suppressed)
                 (push 'delete order))))
      (project-frame-sessions-delete-frame
       'frame (lambda (frame)
                (should (eq frame 'frame))
                (push 'callback order)))
      (should (equal (nreverse order) '(save callback delete)))
      (should (eq suppressed 'frame)))))

(ert-deftest project-frame-sessions-delete-frame-failure-leaves-frame-live ()
  (dolist (failure '(save callback))
    (let (deleted)
      (cl-letf (((symbol-function 'project-frame-sessions--frame-id) (lambda (_) "id"))
                ((symbol-function 'project-frame-sessions--save-frame-internal)
                 (lambda (&rest _)
                   (when (eq failure 'save) (error "save failed"))))
                ((symbol-function 'delete-frame) (lambda (&rest _) (setq deleted t))))
        (should-error
         (project-frame-sessions-delete-frame
          'frame (lambda (_)
                   (when (eq failure 'callback) (error "callback failed")))))
        (should-not deleted)))))

(ert-deftest project-frame-sessions-restore-refuses-symlink-snapshot ()
  (project-frame-sessions-tests--with-store
    (let* ((entry (project-frame-sessions-tests--entry))
           (snapshot (project-frame-sessions--entry-snapshot-file entry))
           (outside (make-temp-file "pfs-outside-snapshot-")))
      (unwind-protect
          (progn
            (make-directory (file-name-directory snapshot) t)
            (make-symbolic-link outside snapshot)
            (should-error
             (project-frame-sessions--read-desktop-frameset
              entry (selected-frame))))
        (ignore-errors (delete-file snapshot))
        (ignore-errors (delete-file outside))))))

(ert-deftest project-frame-sessions-purge-refuses-symlink-trash ()
  (project-frame-sessions-tests--with-store
    (let* ((parent (project-frame-sessions--trash-directory))
           (target (make-temp-file "pfs-target-" t))
           (name (format "%s-%s" (project-frame-sessions-tests--id)
                         "1111111111111111"))
           (link (expand-file-name name parent)))
      (unwind-protect
          (progn
            (make-directory parent t)
            (make-symbolic-link target link)
            (should-not (project-frame-sessions--safe-direct-child-p
                         link parent
                         "\\`[0-9a-f]\\{64\\}-[0-9a-f]\\{16\\}\\'")))
        (ignore-errors (delete-file link))
        (ignore-errors (delete-directory target t))))))

(ert-deftest project-frame-sessions-unmanaged-ordinary-frame-is-not-marked-dirty ()
  (let ((project-frame-sessions--runtimes (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'project-frame-sessions--graphical-frame-p) (lambda (_) t))
              ((symbol-function 'project-frame-sessions--frame-id) #'ignore))
      (project-frame-sessions--mark-frame-dirty 'ordinary)
      (should (= 0 (hash-table-count project-frame-sessions--runtimes))))))

(ert-deftest project-frame-sessions-maximum-dirty-age-forces-immediate-attempt ()
  (let* ((project-frame-sessions--runtimes (make-hash-table :test #'equal))
         (project-frame-sessions-maximum-dirty-age 10)
         (runtime (project-frame-sessions--runtime (project-frame-sessions-tests--id)))
         delay)
    (setf (project-frame-sessions--runtime-dirty-since runtime) (- (float-time) 20))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (seconds &rest _) (setq delay seconds) 'timer)))
      (project-frame-sessions--schedule (project-frame-sessions-tests--id))
      (should (= delay 0)))))

(ert-deftest project-frame-sessions-shutdown-cancel-blocks-exit ()
  (cl-letf (((symbol-function 'project-frame-sessions--shutdown-attempt)
             (lambda () '(("Work" . "failure"))))
            ((symbol-function 'project-frame-sessions--warn) #'ignore)
            ((symbol-function 'read-char-choice) (lambda (&rest _) ?c)))
    (should-not (project-frame-sessions--query-exit))))

(ert-deftest project-frame-sessions-shutdown-force-requires-confirmation ()
  (cl-letf (((symbol-function 'project-frame-sessions--shutdown-attempt)
             (lambda () '(("Work" . "failure"))))
            ((symbol-function 'project-frame-sessions--warn) #'ignore)
            ((symbol-function 'read-char-choice) (lambda (&rest _) ?q))
            ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
    (should (project-frame-sessions--query-exit))))

(ert-deftest project-frame-sessions-mode-enable-reschedules-dirty-runtime ()
  (let* ((id (project-frame-sessions-tests--id))
         (project-frame-sessions--runtimes (make-hash-table :test #'equal))
         (project-frame-sessions-autosave-interval nil)
         (project-frame-sessions-mode nil)
         scheduled)
    (let ((runtime (project-frame-sessions--runtime id)))
      (setf (project-frame-sessions--runtime-dirty runtime) 1)
      (cl-letf (((symbol-function 'project-frame-sessions--remove-hooks) #'ignore)
                ((symbol-function 'project-frame-sessions--install-hooks) #'ignore)
                ((symbol-function 'project-frame-sessions--scan-recovery) #'ignore)
                ((symbol-function 'project-frame-sessions--schedule)
                 (lambda (session &optional _delay) (push session scheduled))))
        (unwind-protect
            (progn
              (project-frame-sessions-mode 1)
              (should (equal scheduled (list id))))
          (project-frame-sessions-mode -1))))))

(ert-deftest project-frame-sessions-mode-hooks-are-idempotent ()
  (let ((project-frame-sessions-autosave-interval nil)
        (project-frame-sessions-warn-function #'ignore))
    (unwind-protect
        (progn
          (project-frame-sessions-mode 1)
          (project-frame-sessions-mode 1)
          (should (= 1 (cl-count #'project-frame-sessions--delete-frame-hook
                                 delete-frame-functions)))
          (should (= 1 (cl-count #'project-frame-sessions--query-exit
                                 kill-emacs-query-functions)))
          (should (advice-member-p #'project-frame-sessions--event-dirty
                                   'tab-bar-close-tab))
          (project-frame-sessions-mode -1)
          (should-not (memq #'project-frame-sessions--delete-frame-hook
                            delete-frame-functions))
          (should-not (advice-member-p #'project-frame-sessions--event-dirty
                                       'tab-bar-close-tab)))
      (project-frame-sessions-mode -1))))

(ert-deftest project-frame-sessions-store-rejects-relative-and-remote-paths ()
  (dolist (directory '("relative/store/" "/ssh:example.invalid:/tmp/pfs/"))
    (let ((project-frame-sessions-directory directory))
      (should-error (project-frame-sessions--read-index)))))

(ert-deftest project-frame-sessions-store-rejects-symlink-root-without-outside-write ()
  (let* ((parent (make-temp-file "pfs-link-parent-" t))
         (outside (make-temp-file "pfs-link-outside-" t))
         (link (expand-file-name "store" parent))
         (sentinel (expand-file-name "sentinel" outside)))
    (unwind-protect
        (progn
          (with-temp-file sentinel (insert "unchanged"))
          (make-symbolic-link outside link)
          (let ((project-frame-sessions-directory (file-name-as-directory link)))
            (should-error (project-frame-sessions--write-index nil)))
          (should (equal (with-temp-buffer
                           (insert-file-contents sentinel)
                           (buffer-string))
                         "unchanged"))
          (should-not (file-exists-p (expand-file-name "index.eld" outside))))
      (ignore-errors (delete-file link))
      (ignore-errors (delete-directory parent t))
      (ignore-errors (delete-directory outside t)))))

(ert-deftest project-frame-sessions-rejects-symlink-managed-roots ()
  (dolist (root-name '("sessions" "trash" "recovery" ".transaction-lock"))
    (project-frame-sessions-tests--with-store
      (let ((outside (make-temp-file "pfs-managed-outside-" t))
            (link (expand-file-name root-name project-frame-sessions-directory)))
        (unwind-protect
            (progn
              (make-symbolic-link outside link)
              (should-error (project-frame-sessions--read-index))
              (should-not (directory-files outside nil directory-files-no-dot-files-regexp)))
          (ignore-errors (delete-file link))
          (ignore-errors (delete-directory outside t)))))))

(ert-deftest project-frame-sessions-contained-path-rejects-escape-and-prefix-confusion ()
  (project-frame-sessions-tests--with-store
    (let ((sessions (project-frame-sessions--sessions-directory)))
      (make-directory sessions t)
      (should-error
       (project-frame-sessions--assert-contained-path
        (expand-file-name "../outside" sessions) sessions))
      (should-error
       (project-frame-sessions--assert-contained-path
        (expand-file-name "sessions-other/item" project-frame-sessions-directory)
        sessions))
      (should
       (project-frame-sessions--assert-contained-path
        (expand-file-name "new-id/new-file" sessions) sessions)))))

(ert-deftest project-frame-sessions-rejects-symlinked-nested-ancestor ()
  (project-frame-sessions-tests--with-store
    (let* ((sessions (project-frame-sessions--sessions-directory))
           (outside (make-temp-file "pfs-nested-outside-" t))
           (link (expand-file-name "nested" sessions)))
      (unwind-protect
          (progn
            (make-directory sessions t)
            (make-symbolic-link outside link)
            (should-error
             (project-frame-sessions--assert-contained-path
              (expand-file-name "nested/not-created/file" sessions) sessions)))
        (ignore-errors (delete-file link))
        (ignore-errors (delete-directory outside t))))))

(ert-deftest project-frame-sessions-configuration-validation ()
  (dolist (binding '((project-frame-sessions-autosave-interval . 0)
                     (project-frame-sessions-autosave-interval . -1)
                     (project-frame-sessions-debounce-delay . -1)
                     (project-frame-sessions-maximum-dirty-age . -0.5)
                     (project-frame-sessions-ownerless-lock-stale-seconds . bad)
                     (project-frame-sessions-retry-delays . (1 bad 3))))
    (let ((project-frame-sessions-autosave-interval 300)
          (project-frame-sessions-debounce-delay 2)
          (project-frame-sessions-maximum-dirty-age 60)
          (project-frame-sessions-ownerless-lock-stale-seconds 5)
          (project-frame-sessions-retry-delays '(1 2)))
      (set (car binding) (cdr binding))
      (should-error (project-frame-sessions--validate-configuration))))
  (dolist (retry '(nil (0) (0.0 1 2.5)))
    (let ((project-frame-sessions-autosave-interval 0.1)
          (project-frame-sessions-debounce-delay 0)
          (project-frame-sessions-maximum-dirty-age 0.0)
          (project-frame-sessions-ownerless-lock-stale-seconds 0)
          (project-frame-sessions-retry-delays retry))
      (should (project-frame-sessions--validate-configuration)))))

(ert-deftest project-frame-sessions-mode-rejects-invalid-values-before-hooks ()
  (let ((project-frame-sessions-mode nil)
        (project-frame-sessions-autosave-interval 0)
        installed)
    (cl-letf (((symbol-function 'project-frame-sessions--install-hooks)
               (lambda () (setq installed t))))
      (should-error (project-frame-sessions-mode 1))
      (should-not project-frame-sessions-mode)
      (should-not installed))))

(ert-deftest project-frame-sessions-failpoint-releases-lock ()
  (project-frame-sessions-tests--with-store
    (let ((project-frame-sessions--failpoint-function
           (lambda (name)
             (when (eq name 'after-lock-acquisition) (error "injected")))))
      (should-error (project-frame-sessions--with-lock t))
      (should-not (file-exists-p (project-frame-sessions--lock-directory))))))

(ert-deftest project-frame-sessions-promotion-failpoint-keeps-old-index-and-recovery ()
  (project-frame-sessions-tests--with-store
    (let* ((entry (project-frame-sessions-tests--entry))
           (id (plist-get entry :id))
           (new-entry (list :id id :name "Work" :root nil))
           (token "2222222222222222")
           (stage (project-frame-sessions--stage-directory id token)))
      (project-frame-sessions--write-index (list entry))
      (project-frame-sessions-tests--put-snapshot entry "old")
      (make-directory stage t)
      (project-frame-sessions-tests--fake-desktop-save stage)
      (let ((project-frame-sessions--failpoint-function
             (lambda (name)
               (when (eq name 'after-snapshot-promotion) (error "injected")))))
        (should-error
         (project-frame-sessions--commit-save
          (selected-frame) new-entry stage token)))
      (should (equal (project-frame-sessions--read-index) (list entry)))
      (should (file-exists-p (expand-file-name "metadata.eld" stage)))
      (should (file-exists-p
               (expand-file-name
                (format "sessions/%s/desktop-%s.el" id token)
                project-frame-sessions-directory))))))

(provide 'project-frame-sessions-tests)
;;; project-frame-sessions-tests.el ends here
