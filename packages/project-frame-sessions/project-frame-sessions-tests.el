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
          (old-excluded
           (frame-parameter frame
                            project-frame-sessions--excluded-buffers-parameter))
          (old-root (frame-parameter frame 'project-frame-sessions-root))
          (old-directory (frame-parameter frame 'default-directory)))
     (unwind-protect (progn ,@body)
       (set-frame-parameter frame project-frame-sessions--id-parameter old-id)
       (set-frame-parameter frame project-frame-sessions--deleted-parameter old-deleted)
       (set-frame-parameter frame
                            project-frame-sessions--excluded-buffers-parameter
                            old-excluded)
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

(ert-deftest project-frame-sessions-buffer-killing-is-disabled-by-default ()
  (should-not (default-value 'project-frame-sessions-kill-buffers-on-switch)))

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

(ert-deftest project-frame-sessions-frame-eligibility-excludes-popup-frames ()
  (let ((terminal-frame 'terminal)
        desktop-excluded parent)
    (cl-letf (((symbol-function 'frame-live-p) (lambda (_) t))
              ((symbol-function 'display-graphic-p) (lambda (_) t))
              ((symbol-function 'frame-parent) (lambda (_) parent))
              ((symbol-function 'frame-parameter)
               (lambda (_frame parameter)
                 (and (eq parameter 'desktop-dont-save)
                      desktop-excluded))))
      (should (project-frame-sessions--graphical-frame-p 'candidate))
      (setq desktop-excluded t)
      (should-not (project-frame-sessions--graphical-frame-p 'candidate))
      (setq desktop-excluded nil parent 'parent)
      (should-not (project-frame-sessions--graphical-frame-p 'candidate)))))

(ert-deftest project-frame-sessions-idle-save-skips-popup-frames ()
  (let ((project-frame-sessions-mode t)
        (terminal-frame 'terminal)
        (frame 'candidate)
        desktop-excluded parent (discovered 0) (saved 0))
    (cl-letf (((symbol-function 'frame-list) (lambda () (list frame)))
              ((symbol-function 'frame-live-p) (lambda (_) t))
              ((symbol-function 'display-graphic-p) (lambda (_) t))
              ((symbol-function 'frame-parent) (lambda (_) parent))
              ((symbol-function 'frame-parameter)
               (lambda (_frame parameter)
                 (and (eq parameter 'desktop-dont-save)
                      desktop-excluded)))
              ((symbol-function 'project-frame-sessions--discover-root)
               (lambda (_) (cl-incf discovered) "/tmp/"))
              ((symbol-function 'project-frame-sessions--save-frame-internal)
               (lambda (&rest _) (cl-incf saved))))
      (setq desktop-excluded t)
      (project-frame-sessions--idle-save)
      (setq desktop-excluded nil parent 'parent)
      (project-frame-sessions--idle-save)
      (setq parent nil)
      (project-frame-sessions--idle-save)
      (should (= discovered 1))
      (should (= saved 1)))))

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

(ert-deftest project-frame-sessions-buffer-predicate-honors-ignored-name-regexp ()
  (let ((wanted (generate-new-buffer "pfs-wanted"))
        (ignored (generate-new-buffer "pfs-ignored")))
    (unwind-protect
        (let* ((project-frame-sessions-ignored-buffer-name-regexp
                "\\`pfs-ignored\\'")
               (predicate (project-frame-sessions--buffer-save-predicate
                           (list wanted ignored) nil)))
          (should (funcall predicate nil (buffer-name wanted) 'fundamental-mode))
          (should-not
           (funcall predicate nil (buffer-name ignored) 'fundamental-mode)))
      (kill-buffer wanted)
      (kill-buffer ignored))))

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
                       (_ nil))))
                  ((symbol-function 'set-frame-parameter) #'ignore)
                  ((symbol-function 'window-list) (lambda (&rest _) nil)))
          (let ((project-frame-sessions-frame-buffer-function
                 (lambda (_) (list current))))
            (let ((buffers (project-frame-sessions--tab-buffers 'frame)))
              (should (memq current buffers))
              (should (memq hidden buffers))
              (should (memq workspace buffers)))))
      (mapc #'kill-buffer (list current hidden workspace)))))

(ert-deftest project-frame-sessions-tab-buffers-keep-retained-buffers-excluded-until-reused ()
  (let ((retained (generate-new-buffer " pfs-retained"))
        (current (generate-new-buffer " pfs-current"))
        (excluded nil)
        active)
    (unwind-protect
        (progn
          (setq excluded (list retained))
          (cl-letf (((symbol-function 'project-frame-sessions--active-workspace-buffers)
                     (lambda (_) active))
                    ((symbol-function 'project-frame-sessions--tab-parameter-buffers)
                     (lambda (_) nil))
                    ((symbol-function 'project-frame-sessions--excluded-buffers)
                     (lambda (_) excluded))
                    ((symbol-function 'project-frame-sessions--set-excluded-buffers)
                     (lambda (_frame buffers) (setq excluded buffers))))
            (let ((project-frame-sessions-frame-buffer-function
                   (lambda (_) (list retained current))))
              (should (equal (project-frame-sessions--tab-buffers 'frame)
                             (list current)))
              (setq active (list retained))
              (should (memq retained
                            (project-frame-sessions--tab-buffers 'frame)))
              (should-not excluded))))
      (mapc #'kill-buffer (list retained current)))))

(ert-deftest project-frame-sessions-current-frame-switch-retains-and-excludes-by-default ()
  (let ((outgoing (generate-new-buffer " pfs-outgoing"))
        (reused (generate-new-buffer " pfs-reused"))
        (prior (generate-new-buffer " pfs-prior"))
        excluded)
    (unwind-protect
        (cl-letf (((symbol-function 'project-frame-sessions--set-excluded-buffers)
                   (lambda (_frame buffers) (setq excluded buffers))))
          (let ((project-frame-sessions-kill-buffers-on-switch nil))
            (project-frame-sessions--complete-current-frame-switch
             'frame (list outgoing reused) (list prior) (list reused)))
          (should (buffer-live-p outgoing))
          (should (memq outgoing excluded))
          (should (memq prior excluded))
          (should-not (memq reused excluded)))
      (mapc #'kill-buffer (list outgoing reused prior)))))

(ert-deftest project-frame-sessions-current-frame-switch-kills-only-unshared-buffers ()
  (let ((outgoing (generate-new-buffer " pfs-outgoing"))
        (shared (generate-new-buffer " pfs-shared"))
        (declined (generate-new-buffer " pfs-declined"))
        excluded called)
    (unwind-protect
        (cl-letf (((symbol-function 'project-frame-sessions--other-frame-buffers)
                   (lambda (_) (list shared)))
                  ((symbol-function 'project-frame-sessions--set-excluded-buffers)
                   (lambda (_frame buffers) (setq excluded buffers)))
                  ((symbol-function 'project-frame-sessions--kill-outgoing-buffer)
                   (lambda (buffer)
                     (push buffer called)
                     (unless (eq buffer declined) (kill-buffer buffer)))))
          (let ((project-frame-sessions-kill-buffers-on-switch t))
            (project-frame-sessions--complete-current-frame-switch
             'frame (list outgoing shared declined) nil nil))
          (should-not (buffer-live-p outgoing))
          (should (buffer-live-p shared))
          (should (buffer-live-p declined))
          (should-not (memq shared called))
          (should (memq declined called))
          (should (memq shared excluded))
          (should (memq declined excluded)))
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer) (kill-buffer buffer)))
            (list outgoing shared declined)))))

(ert-deftest project-frame-sessions-modified-outgoing-buffer-requires-confirmation ()
  (let ((buffer (generate-new-buffer " pfs-modified")) prompted)
    (unwind-protect
        (progn
          (with-current-buffer buffer (set-buffer-modified-p t))
          (cl-letf (((symbol-function 'kill-buffer--possibly-save)
                     (lambda (candidate) (setq prompted candidate) nil)))
            (project-frame-sessions--kill-outgoing-buffer buffer))
          (should (eq prompted buffer))
          (should (buffer-live-p buffer)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer)))))

(ert-deftest project-frame-sessions-filter-tabs-keeps-one-current-tab ()
  (let ((project-frame-sessions-ignored-tab-name-regexp "\\`omit\\'"))
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
    (cl-letf (((symbol-function 'project-frame-sessions--select-restore-candidate)
               (lambda (_) (push 'select order)
                 (list :type 'session
                       :entry (project-frame-sessions-tests--entry))))
              ((symbol-function 'project-frame-sessions--find-live-frame) #'ignore)
              ((symbol-function 'make-frame) (lambda () (push 'make order) 'new))
              ((symbol-function 'project-frame-sessions--restore-entry)
               (lambda (&rest _) (push 'restore order) 'new)))
      (project-frame-sessions-restore)
      (should (equal (nreverse order) '(select make restore))))))

(ert-deftest project-frame-sessions-open-session-is-focused-without-new-frame ()
  (let ((owner 'owner) focused made)
    (cl-letf (((symbol-function 'project-frame-sessions--select-restore-candidate)
               (lambda (_) (list :type 'session
                                  :entry (project-frame-sessions-tests--entry))))
              ((symbol-function 'project-frame-sessions--find-live-frame) (lambda (&rest _) owner))
              ((symbol-function 'select-frame-set-input-focus) (lambda (frame) (setq focused frame)))
              ((symbol-function 'make-frame) (lambda () (setq made t))))
      (should (eq (project-frame-sessions-restore) owner))
      (should (eq focused owner))
      (should-not made))))

(ert-deftest project-frame-sessions-current-frame-restore-cleans-only-after-success ()
  (project-frame-sessions-tests--preserve-frame
    (let* ((frame (selected-frame))
           (outgoing (generate-new-buffer " pfs-restore-outgoing"))
           (prior (generate-new-buffer " pfs-restore-prior"))
           (reused (generate-new-buffer " pfs-restore-reused"))
           (destination (generate-new-buffer " pfs-restore-destination"))
           completed)
      (unwind-protect
          (cl-letf (((symbol-function 'project-frame-sessions--find-live-frame)
                     #'ignore)
                    ((symbol-function 'frame-list) (lambda () (list frame)))
                    ((symbol-function 'project-frame-sessions--tab-buffers)
                     (lambda (_) (list outgoing reused)))
                    ((symbol-function 'project-frame-sessions--excluded-buffers)
                     (lambda (_) (list prior)))
                    ((symbol-function 'frameset-save) (lambda (&rest _) 'rollback))
                    ((symbol-function 'project-frame-sessions--frame-id) #'ignore)
                    ((symbol-function 'project-frame-sessions--read-desktop-frameset)
                     (lambda (&rest _)
                       (setq project-frame-sessions--desktop-restored-buffers
                             (list reused))
                       'snapshot))
                    ((symbol-function 'project-frame-sessions--prepare-restore-target)
                     #'ignore)
                    ((symbol-function 'project-frame-sessions--restore-frameset)
                     #'ignore)
                    ((symbol-function 'project-frame-sessions--active-workspace-buffers)
                     (lambda (_) (list destination)))
                    ((symbol-function
                      'project-frame-sessions--complete-current-frame-switch)
                     (lambda (&rest args) (setq completed args)))
                    ((symbol-function 'select-frame-set-input-focus) #'ignore))
            (project-frame-sessions--restore-entry
             (project-frame-sessions-tests--entry) frame t)
            (should (equal completed
                           (list frame (list outgoing reused) (list prior)
                                 (list reused destination)))))
        (mapc #'kill-buffer (list outgoing prior reused destination))))))

(ert-deftest project-frame-sessions-failed-current-frame-restore-does-not-clean-buffers ()
  (project-frame-sessions-tests--preserve-frame
    (let* ((frame (selected-frame))
           (outgoing (generate-new-buffer " pfs-restore-outgoing"))
           (restored (generate-new-buffer " pfs-restore-failed-new"))
           completed exclusions)
      (unwind-protect
          (cl-letf (((symbol-function 'project-frame-sessions--find-live-frame)
                     #'ignore)
                    ((symbol-function 'frame-list) (lambda () (list frame)))
                    ((symbol-function 'project-frame-sessions--tab-buffers)
                     (lambda (_) (list outgoing)))
                    ((symbol-function 'project-frame-sessions--excluded-buffers)
                     (lambda (_) nil))
                    ((symbol-function 'frameset-save) (lambda (&rest _) 'rollback))
                    ((symbol-function 'project-frame-sessions--frame-id) #'ignore)
                    ((symbol-function 'project-frame-sessions--read-desktop-frameset)
                     (lambda (&rest _)
                       (setq project-frame-sessions--desktop-restored-buffers
                             (list restored))
                       (error "restore failed")))
                    ((symbol-function 'project-frame-sessions--prepare-restore-target)
                     #'ignore)
                    ((symbol-function 'project-frame-sessions--restore-frameset)
                     #'ignore)
                    ((symbol-function
                      'project-frame-sessions--complete-current-frame-switch)
                     (lambda (&rest _) (setq completed t)))
                    ((symbol-function 'project-frame-sessions--set-excluded-buffers)
                     (lambda (_ buffers) (setq exclusions buffers)))
                    ((symbol-function 'select-frame-set-input-focus) #'ignore))
            (should-error
             (project-frame-sessions--restore-entry
              (project-frame-sessions-tests--entry) frame t))
            (should-not completed)
            (should (equal exclusions (list restored)))
            (should (buffer-live-p outgoing))
            (should (buffer-live-p restored)))
        (dolist (buffer (list outgoing restored))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest project-frame-sessions-new-frame-is-deleted-after-restore-failure ()
  (let ((fresh 'fresh) deleted)
    (cl-letf (((symbol-function 'project-frame-sessions--select-restore-candidate)
               (lambda (_) (list :type 'session
                                  :entry (project-frame-sessions-tests--entry))))
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

(ert-deftest project-frame-sessions-restore-choices-put-sessions-before-projects ()
  (project-frame-sessions-tests--with-store
    (let* ((root (make-temp-file "pfs-project-" t))
           (entry (project-frame-sessions-tests--entry nil "Saved" nil)))
      (unwind-protect
          (progn
            (project-frame-sessions--write-index (list entry))
            (project-frame-sessions-tests--put-snapshot entry)
            (cl-letf (((symbol-function 'project-frame-sessions--known-project-roots)
                       (lambda () (list root)))
                      ((symbol-function 'project-frame-sessions--project-name-at-root)
                       (lambda (_) "Known")))
              (let ((choices (project-frame-sessions--restore-choices)))
                (should (equal (mapcar #'car choices) '("Saved" "Known")))
                (should (eq (plist-get (cdr (car choices)) :type) 'session))
                (should (eq (plist-get (cdr (cadr choices)) :type) 'project)))))
        (ignore-errors (delete-directory root t))))))

(ert-deftest project-frame-sessions-project-discovery-filters-and-deduplicates-roots ()
  (let* ((root (make-temp-file "pfs-project-" t))
         (alias (expand-file-name "../" (expand-file-name "child/" root))))
    (unwind-protect
        (cl-letf (((symbol-function 'project-frame-sessions--known-project-roots)
                   (lambda () (list root alias "relative" "/missing/pfs/"
                                    "/ssh:host:/tmp/" 42)))
                  ((symbol-function 'project-frame-sessions--project-name-at-root)
                   (lambda (_) "Known")))
          (let ((projects (project-frame-sessions--unsaved-projects nil)))
            (should (= (length projects) 1))
            (should (equal (plist-get (car projects) :root)
                           (project-frame-sessions--canonical-root root)))))
      (ignore-errors (delete-directory root t)))))

(ert-deftest project-frame-sessions-saved-root-suppresses-remembered-project ()
  (let* ((root (make-temp-file "pfs-project-" t))
         (entry (project-frame-sessions-tests--entry nil "Saved" root)))
    (unwind-protect
        (cl-letf (((symbol-function 'project-frame-sessions--known-project-roots)
                   (lambda () (list (concat root "/../"
                                             (file-name-nondirectory root))))))
          (should-not (project-frame-sessions--unsaved-projects (list entry))))
      (ignore-errors (delete-directory root t)))))

(ert-deftest project-frame-sessions-current-frame-project-excludes-outgoing-from-first-save ()
  (project-frame-sessions-tests--preserve-frame
    (let* ((frame (selected-frame))
           (outgoing (generate-new-buffer " pfs-project-outgoing"))
           (prior (generate-new-buffer " pfs-project-prior"))
           (destination (generate-new-buffer " pfs-project-destination"))
           saved-id save-exclusions completed)
      (unwind-protect
          (progn
            (set-frame-parameter frame project-frame-sessions--id-parameter "old")
            (set-frame-parameter
             frame project-frame-sessions--excluded-buffers-parameter (list prior))
            (cl-letf (((symbol-function 'project-frame-sessions--tab-buffers)
                       (lambda (_) (list outgoing)))
                      ((symbol-function 'project-frame-sessions--active-workspace-buffers)
                       (lambda (_) (list destination)))
                      ((symbol-function 'dired) #'ignore)
                      ((symbol-function 'project-frame-sessions--save-frame-internal)
                       (lambda (candidate &rest _)
                         (setq saved-id
                               (frame-parameter
                                candidate project-frame-sessions--id-parameter)
                               save-exclusions
                               project-frame-sessions--save-buffer-exclusions)
                         (set-frame-parameter
                          candidate project-frame-sessions--id-parameter "new")))
                      ((symbol-function
                        'project-frame-sessions--complete-current-frame-switch)
                       (lambda (&rest args) (setq completed args)))
                      ((symbol-function 'select-frame-set-input-focus) #'ignore))
              (project-frame-sessions--enroll-project
               frame '(:type project :root "/tmp/" :name "Tmp") t))
            (should-not saved-id)
            (should (memq outgoing save-exclusions))
            (should (memq prior save-exclusions))
            (should-not (memq destination save-exclusions))
            (should (equal completed
                           (list frame (list outgoing) (list prior)
                                 (list destination)))))
        (mapc #'kill-buffer (list outgoing prior destination))))))

(ert-deftest project-frame-sessions-failed-current-frame-project-switch-does-not-clean-buffers ()
  (project-frame-sessions-tests--preserve-frame
    (let* ((frame (selected-frame))
           (outgoing (generate-new-buffer " pfs-project-outgoing"))
           (prior (generate-new-buffer " pfs-project-prior"))
           completed)
      (unwind-protect
          (progn
            (set-frame-parameter
             frame project-frame-sessions--excluded-buffers-parameter (list prior))
            (cl-letf (((symbol-function 'project-frame-sessions--tab-buffers)
                       (lambda (_) (list outgoing)))
                      ((symbol-function 'project-frame-sessions--active-workspace-buffers)
                       (lambda (_) nil))
                      ((symbol-function 'dired) #'ignore)
                      ((symbol-function 'project-frame-sessions--save-frame-internal)
                       (lambda (&rest _) (error "save failed")))
                      ((symbol-function
                        'project-frame-sessions--complete-current-frame-switch)
                       (lambda (&rest _) (setq completed t))))
              (should-error
               (project-frame-sessions--enroll-project
                frame '(:type project :root "/tmp/" :name "Tmp") t)))
            (should-not completed)
            (should (buffer-live-p outgoing))
            (should (equal
                     (frame-parameter
                      frame project-frame-sessions--excluded-buffers-parameter)
                     (list prior))))
        (mapc #'kill-buffer (list outgoing prior))))))

(ert-deftest project-frame-sessions-project-route-leaves-new-frame-on-failure ()
  (let ((fresh 'fresh) deleted made)
    (cl-letf (((symbol-function 'project-frame-sessions--select-restore-candidate)
               (lambda (_) '(:type project :root "/tmp/" :name "Tmp")))
              ((symbol-function 'make-frame)
               (lambda () (setq made t) fresh))
              ((symbol-function 'project-frame-sessions--enroll-project)
               (lambda (&rest _) (error "save failed")))
              ((symbol-function 'delete-frame)
               (lambda (&rest _) (setq deleted t))))
      (should-error (project-frame-sessions-restore))
      (should made)
      (should-not deleted))))

(ert-deftest project-frame-sessions-frame-close-preservation-is-enabled-by-default ()
  (should (default-value
           'project-frame-sessions-preserve-buffers-after-frame-close)))

(ert-deftest project-frame-sessions-frame-close-cleanup-honors-policy-and-sharing ()
  (let ((private (generate-new-buffer " pfs-close-private"))
        (shared (generate-new-buffer " pfs-close-shared"))
        (adopted (generate-new-buffer " pfs-close-adopted"))
        (project-frame-sessions-preserve-buffers-after-frame-close t)
        other-frame-buffers)
    (unwind-protect
        (cl-letf (((symbol-function 'project-frame-sessions--other-frame-buffers)
                   (lambda (_) other-frame-buffers)))
          (project-frame-sessions--dispose-closed-frame-buffers
           (list private shared adopted))
          (should (cl-every #'buffer-live-p (list private shared adopted)))
          (setq project-frame-sessions-preserve-buffers-after-frame-close nil
                other-frame-buffers (list shared adopted))
          (project-frame-sessions--dispose-closed-frame-buffers
           (list private shared adopted))
          (should-not (buffer-live-p private))
          (should (buffer-live-p shared))
          (should (buffer-live-p adopted)))
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer) (kill-buffer buffer)))
            (list private shared adopted)))))

(ert-deftest project-frame-sessions-frame-close-cleanup-continues-after-refusal ()
  (let ((declined (generate-new-buffer " pfs-close-declined"))
        (private (generate-new-buffer " pfs-close-next"))
        (project-frame-sessions-preserve-buffers-after-frame-close nil))
    (unwind-protect
        (cl-letf (((symbol-function 'project-frame-sessions--other-frame-buffers)
                   (lambda (_) nil))
                  ((symbol-function 'project-frame-sessions--kill-outgoing-buffer)
                   (lambda (buffer)
                     (if (eq buffer declined) (signal 'quit nil)
                       (kill-buffer buffer)))))
          (project-frame-sessions--dispose-closed-frame-buffers
           (list declined private))
          (should (buffer-live-p declined))
          (should-not (buffer-live-p private)))
      (when (buffer-live-p declined) (kill-buffer declined))
      (when (buffer-live-p private) (kill-buffer private)))))

(ert-deftest project-frame-sessions-delete-frame-cleans-once-only-after-deletion ()
  (let ((project-frame-sessions-preserve-buffers-after-frame-close nil)
        (live t) cleaned scheduled)
    (cl-letf (((symbol-function 'project-frame-sessions--frame-id) (lambda (_) "id"))
              ((symbol-function 'project-frame-sessions--save-frame-internal)
               (lambda (&rest _) t))
              ((symbol-function 'project-frame-sessions--frame-close-candidates)
               (lambda (_) '(one two)))
              ((symbol-function 'delete-frame) (lambda (&rest _) (setq live nil)))
              ((symbol-function 'frame-live-p) (lambda (_) live))
              ((symbol-function 'project-frame-sessions--dispose-closed-frame-buffers)
               (lambda (buffers) (push buffers cleaned)))
              ((symbol-function 'run-at-time)
               (lambda (&rest args) (push args scheduled))))
      (project-frame-sessions-delete-frame 'frame)
      (should (equal cleaned '((one two))))
      (should-not scheduled))))

(ert-deftest project-frame-sessions-delete-frame-failed-deletion-preserves-buffers ()
  (let ((project-frame-sessions-preserve-buffers-after-frame-close nil) cleaned)
    (cl-letf (((symbol-function 'project-frame-sessions--frame-id) (lambda (_) "id"))
              ((symbol-function 'project-frame-sessions--save-frame-internal)
               (lambda (&rest _) t))
              ((symbol-function 'project-frame-sessions--frame-close-candidates)
               (lambda (_) '(buffer)))
              ((symbol-function 'delete-frame) (lambda (&rest _) (error "veto")))
              ((symbol-function 'project-frame-sessions--dispose-closed-frame-buffers)
               (lambda (&rest _) (setq cleaned t))))
      (should-error (project-frame-sessions-delete-frame 'frame))
      (should-not cleaned))))

(ert-deftest project-frame-sessions-delete-hook-defers-and-rechecks-deletion ()
  (let ((project-frame-sessions-mode t)
        (project-frame-sessions-preserve-buffers-after-frame-close nil)
        (project-frame-sessions--pending-frame-closes
         (make-hash-table :test #'eq))
        scheduled finalized)
    (cl-letf (((symbol-function 'project-frame-sessions--frame-id) (lambda (_) "id"))
              ((symbol-function 'project-frame-sessions--save-frame-internal)
               (lambda (&rest _) t))
              ((symbol-function 'project-frame-sessions--frame-close-candidates)
               (lambda (_) '(buffer)))
              ((symbol-function 'run-at-time)
               (lambda (_delay _repeat function &rest args)
                 (setq scheduled (cons function args))))
              ((symbol-function 'project-frame-sessions--dispose-closed-frame-buffers)
               (lambda (_) (setq finalized t)))
              ((symbol-function 'frame-live-p) (lambda (_) t)))
      (project-frame-sessions--delete-frame-hook 'frame)
      (should (gethash 'frame project-frame-sessions--pending-frame-closes))
      (apply (car scheduled) (cdr scheduled))
      (should-not finalized)
      (should-not (gethash 'frame project-frame-sessions--pending-frame-closes))
      (cl-letf (((symbol-function 'frame-live-p) (lambda (_) nil)))
        (apply (car scheduled) (cdr scheduled)))
      (should finalized))))

(ert-deftest project-frame-sessions-delete-hook-coalesces-recursive-calls ()
  (let ((project-frame-sessions-mode t)
        (project-frame-sessions-preserve-buffers-after-frame-close t)
        (project-frame-sessions--pending-frame-closes
         (make-hash-table :test #'eq))
        (saves 0) scheduled)
    (cl-letf (((symbol-function 'project-frame-sessions--frame-id) (lambda (_) "id"))
              ((symbol-function 'project-frame-sessions--save-frame-internal)
               (lambda (&rest _)
                 (cl-incf saves)
                 (project-frame-sessions--delete-frame-hook 'frame)))
              ((symbol-function 'run-at-time)
               (lambda (_delay _repeat function &rest args)
                 (push (cons function args) scheduled))))
      (project-frame-sessions--delete-frame-hook 'frame)
      (should (= saves 1))
      (should (= (length scheduled) 1))
      (cl-letf (((symbol-function 'frame-live-p) (lambda (_) t)))
        (apply (caar scheduled) (cdar scheduled)))
      (project-frame-sessions--delete-frame-hook 'frame)
      (should (= saves 2))
      (should (= (length scheduled) 2)))))

(ert-deftest project-frame-sessions-pi-metadata-resolves-input-through-chat ()
  (let ((chat (generate-new-buffer " pfs-pi-chat"))
        (input (generate-new-buffer " pfs-pi-input"))
        (session "/tmp/pfs-session.jsonl"))
    (unwind-protect
        (progn
          (with-current-buffer chat
            (setq major-mode 'pi-coding-agent-chat-mode)
            (setq-local pi-coding-agent--state (list :session-file session))
            (setq default-directory "/tmp/"))
          (with-current-buffer input
            (setq major-mode 'pi-coding-agent-input-mode)
            (setq-local pi-coding-agent--chat-buffer chat)
            (let ((data (project-frame-sessions--pi-save-data nil)))
              (should (eq (plist-get data :role) 'input))
              (should (equal (plist-get data :session-file) session))
              (should (equal (plist-get data :directory) "/tmp/")))))
      (mapc #'kill-buffer (list chat input)))))

(ert-deftest project-frame-sessions-pi-metadata-without-session-file-is-unrestorable ()
  (let ((chat (generate-new-buffer " pfs-pi-no-session")))
    (unwind-protect
        (with-current-buffer chat
          (setq major-mode 'pi-coding-agent-chat-mode)
          (setq-local pi-coding-agent--state '(:model "test"))
          (should-not (project-frame-sessions--pi-save-data nil)))
      (kill-buffer chat))))

(ert-deftest project-frame-sessions-pi-paired-restore-opens-exact-file-once ()
  (let* ((session (make-temp-file "pfs-pi-" nil ".jsonl"))
         (chat (generate-new-buffer " pfs-restored-chat"))
         (input (generate-new-buffer " pfs-restored-input"))
         (project-frame-sessions--utility-restore-cache
          (make-hash-table :test #'equal))
         (calls 0)
         (original-require (symbol-function 'require)))
    (unwind-protect
        (progn
          (with-current-buffer chat
            (setq-local pi-coding-agent--input-buffer input))
          (cl-letf (((symbol-function 'require)
                     (lambda (feature &optional filename noerror)
                       (if (eq feature 'pi-coding-agent) t
                         (funcall original-require feature filename noerror))))
                    ((symbol-function 'pi-coding-agent--setup-session)
                     (lambda (&rest _) chat))
                    ((symbol-function 'pi-coding-agent-open-session-file)
                     (lambda (file)
                       (should (equal file session))
                       (cl-incf calls)
                       chat)))
            (let ((chat-data (list :project-frame-sessions 1 :kind 'pi
                                   :role 'chat :session-file session))
                  (input-data (list :project-frame-sessions 1 :kind 'pi
                                    :role 'input :session-file session)))
              (should (eq (project-frame-sessions--restore-pi-buffer
                           nil "chat" chat-data) chat))
              (should (eq (project-frame-sessions--restore-pi-buffer
                           nil "input" input-data) input))
              (should (= calls 1)))))
      (mapc (lambda (buffer) (when (buffer-live-p buffer) (kill-buffer buffer)))
            (list chat input))
      (delete-file session))))

(ert-deftest project-frame-sessions-pi-restore-isolates-files-with-the-same-cwd ()
  (let* ((first-file (make-temp-file "pfs-pi-first-" nil ".jsonl"))
         (second-file (make-temp-file "pfs-pi-second-" nil ".jsonl"))
         (project-frame-sessions--utility-restore-cache
          (make-hash-table :test #'equal))
         (buffers (make-hash-table :test #'equal))
         identities
         (original-require (symbol-function 'require)))
    (unwind-protect
        (cl-letf (((symbol-function 'require)
                   (lambda (feature &optional filename noerror)
                     (if (eq feature 'pi-coding-agent) t
                       (funcall original-require feature filename noerror))))
                  ((symbol-function 'pi-coding-agent--setup-session)
                   (lambda (_directory &optional identity)
                     (push identity identities)
                     (or (gethash identity buffers)
                         (let ((chat (generate-new-buffer " pfs-isolated-chat")))
                           (puthash identity chat buffers)
                           chat))))
                  ((symbol-function 'pi-coding-agent-open-session-file)
                   (lambda (_file)
                     (pi-coding-agent--setup-session "/same/project/"))))
          (let* ((first-data
                  (list :project-frame-sessions 1 :kind 'pi :role 'chat
                        :session-file first-file))
                 (second-data
                  (list :project-frame-sessions 1 :kind 'pi :role 'chat
                        :session-file second-file))
                 (first
                  (project-frame-sessions--restore-pi-buffer nil "first" first-data))
                 (second
                  (project-frame-sessions--restore-pi-buffer nil "second" second-data)))
            (should (buffer-live-p first))
            (should (buffer-live-p second))
            (should-not (eq first second))
            (should (= (length (cl-remove-duplicates identities :test #'equal)) 2))
            (should (eq first
                        (project-frame-sessions--restore-pi-buffer
                         nil "first" first-data)))))
      (maphash (lambda (_ buffer)
                 (when (buffer-live-p buffer) (kill-buffer buffer)))
               buffers)
      (delete-file first-file)
      (delete-file second-file))))

(ert-deftest project-frame-sessions-pi-restore-tolerates-absent-package ()
  (let ((session (make-temp-file "pfs-pi-absent-" nil ".jsonl"))
        opened)
    (unwind-protect
        (cl-letf (((symbol-function 'require)
                   (lambda (feature &rest _)
                     (unless (eq feature 'pi-coding-agent) t)))
                  ((symbol-function 'pi-coding-agent-open-session-file)
                   (lambda (&rest _) (setq opened t))))
          (should-not
           (project-frame-sessions--restore-pi-buffer
            nil "pi" (list :project-frame-sessions 1 :kind 'pi
                           :role 'chat :session-file session)))
          (should-not opened))
      (delete-file session))))

(ert-deftest project-frame-sessions-pi-restore-rejects-unavailable-paths ()
  (dolist (file '("relative.jsonl" "/ssh:host:/tmp/session.jsonl"
                  "/missing/pfs-session.jsonl"))
    (should-not
     (project-frame-sessions--restore-pi-buffer
      nil "pi" (list :project-frame-sessions 1 :kind 'pi
                     :role 'chat :session-file file)))))

(ert-deftest project-frame-sessions-eshell-metadata-and-fresh-restore-round-trip ()
  (let* ((directory (make-temp-file "pfs-eshell-" t))
         (source (generate-new-buffer "*pfs-eshell-saved*"))
         data restored hook-ran hook-function)
    (unwind-protect
        (progn
          (with-current-buffer source
            (setq major-mode 'eshell-mode
                  default-directory (file-name-as-directory directory))
            (setq data (project-frame-sessions--eshell-save-data nil)))
          (require 'eshell)
          (setq hook-function (lambda () (setq hook-ran t)))
          (add-hook 'eshell-mode-hook hook-function)
          (setq restored
                (project-frame-sessions--restore-eshell-buffer
                 nil (buffer-name source) data))
          (should (buffer-live-p restored))
          (with-current-buffer restored
            (should (derived-mode-p 'eshell-mode))
            (should (equal default-directory
                           (file-name-as-directory directory))))
          (should hook-ran))
      (when hook-function (remove-hook 'eshell-mode-hook hook-function))
      (when (buffer-live-p source) (kill-buffer source))
      (when (buffer-live-p restored) (kill-buffer restored))
      (ignore-errors (delete-directory directory t)))))

(ert-deftest project-frame-sessions-eshell-tramp-directory-round-trip-is-lazy ()
  (let* ((directory "/ssh:test@example:/srv/project/")
         (source (generate-new-buffer "*pfs-eshell-remote-saved*"))
         data restored observed-directory)
    (unwind-protect
        (progn
          (with-current-buffer source
            (setq major-mode 'eshell-mode
                  default-directory directory
                  data (project-frame-sessions--eshell-save-data nil)))
          (should (equal (plist-get data :directory) directory))
          (cl-letf (((symbol-function 'file-directory-p)
                     (lambda (_) (ert-fail "Tramp host contacted during restore")))
                    ((symbol-function 'require) (lambda (&rest _) t))
                    ((symbol-function 'eshell)
                     (lambda (&optional _)
                       (setq observed-directory default-directory)
                       (generate-new-buffer "*pfs-eshell-remote-restored*"))))
            (setq restored
                  (project-frame-sessions--restore-eshell-buffer
                   nil (buffer-name source) data)))
          (should (buffer-live-p restored))
          (should (equal observed-directory directory)))
      (when (buffer-live-p source) (kill-buffer source))
      (when (buffer-live-p restored) (kill-buffer restored)))))

(ert-deftest project-frame-sessions-eshell-restore-keeps-same-directory-sessions-distinct ()
  (let* ((directory (make-temp-file "pfs-eshell-many-" t))
         (base (list :project-frame-sessions 1 :kind 'eshell
                     :directory (file-name-as-directory directory)
                     :buffer-name "*pfs-many*"))
         first second)
    (unwind-protect
        (progn
          (setq first (project-frame-sessions--restore-eshell-buffer
                       nil "*pfs-many*" (append base '(:identity "one")))
                second (project-frame-sessions--restore-eshell-buffer
                        nil "*pfs-many*" (append base '(:identity "two"))))
          (should (buffer-live-p first))
          (should (buffer-live-p second))
          (should-not (eq first second)))
      (when (buffer-live-p first) (kill-buffer first))
      (when (buffer-live-p second) (kill-buffer second))
      (ignore-errors (delete-directory directory t)))))

(ert-deftest project-frame-sessions-desktop-registrations-preserve-existing-handlers ()
  (let* ((third-party (cons 'eshell-mode #'ignore))
         (desktop-buffer-mode-handlers (list third-party))
         (project-frame-sessions--desktop-handler-registrations nil))
    (project-frame-sessions--register-desktop-handler
     'eshell-mode #'project-frame-sessions--restore-eshell-buffer)
    (should (eq (assq 'eshell-mode desktop-buffer-mode-handlers) third-party))
    (should-not project-frame-sessions--desktop-handler-registrations)
    (project-frame-sessions--remove-desktop-integrations)
    (should (memq third-party desktop-buffer-mode-handlers))))

(provide 'project-frame-sessions-tests)
;;; project-frame-sessions-tests.el ends here
