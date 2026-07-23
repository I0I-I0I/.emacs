;;; project-frame-sessions-graphical-tests.el --- Graphical tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'project-frame-sessions)

(defmacro project-frame-sessions-graphical-tests--with-store (&rest body)
  (declare (indent 0) (debug t))
  `(let* ((directory (make-temp-file "pfs-graphical-" t))
          (project-frame-sessions-directory (file-name-as-directory directory))
          (project-frame-sessions-warn-function #'ignore)
          (project-frame-sessions--runtimes (make-hash-table :test #'equal))
          (project-frame-sessions--pending (make-hash-table :test #'eq)))
     (unwind-protect (progn ,@body)
       (ignore-errors (delete-directory directory t)))))

(defun project-frame-sessions-graphical-tests--make-frame (parameters)
  "Create a graphical frame on the current frame's display with PARAMETERS."
  (make-frame-on-display (frame-parameter nil 'display) parameters))

(ert-deftest project-frame-sessions-graphical-real-desktop-round-trip ()
  (unless (display-graphic-p) (ert-skip "No graphical display"))
  (project-frame-sessions-graphical-tests--with-store
    (let* ((source (make-temp-file "pfs-graphical-buffer-" nil ".txt" "saved text"))
           (buffer (find-file-noselect source))
           (second (get-buffer-create "*pfs-second*"))
           (frame (project-frame-sessions-graphical-tests--make-frame
                   '((name . "PFS graphical test"))))
           entry)
      (unwind-protect
          (progn
            (with-selected-frame frame
              (switch-to-buffer buffer)
              (set-frame-parameter frame 'buffer-list (list buffer))
              (tab-bar-mode 1)
              (tab-bar-new-tab)
              (tab-bar-rename-tab "Second")
              (switch-to-buffer second)
              (set-frame-parameter frame 'buffer-list (list second)))
            (let ((project-frame-sessions-discovery-function #'ignore))
              (setq entry (project-frame-sessions-save frame)))
            (should (file-regular-p
                     (project-frame-sessions--entry-snapshot-file entry)))
            (with-selected-frame frame
              (tab-bar-close-other-tabs)
              (switch-to-buffer (get-buffer-create "*scratch*")))
            (project-frame-sessions--restore-entry entry frame nil)
            (should (equal (project-frame-sessions--frame-id frame)
                           (plist-get entry :id)))
            (should (get-buffer (file-name-nondirectory source)))
            (should (>= (length (frame-parameter frame 'tabs)) 2))
            (with-selected-frame frame
              (tab-bar-select-tab 1)
              (should (memq buffer (frame-parameter frame 'buffer-list)))
              (should-not (memq second (frame-parameter frame 'buffer-list)))
              (tab-bar-select-tab 2)
              (should (memq second (frame-parameter frame 'buffer-list)))
              (should-not (memq buffer (frame-parameter frame 'buffer-list)))))
        (when (frame-live-p frame)
          (let ((project-frame-sessions--delete-suppressed frame))
            (delete-frame frame t)))
        (when (buffer-live-p buffer) (kill-buffer buffer))
        (when (buffer-live-p second) (kill-buffer second))
        (ignore-errors (delete-file source))))))

(ert-deftest project-frame-sessions-graphical-frame-close-buffer-policy ()
  (unless (display-graphic-p) (ert-skip "No graphical display"))
  (project-frame-sessions-graphical-tests--with-store
    (dolist (preserve '(t nil))
      (let* ((frame (project-frame-sessions-graphical-tests--make-frame
                     `((name . ,(format "PFS close %s" preserve)))))
             (buffer (generate-new-buffer
                      (format "*pfs-close-%s*" preserve))))
        (unwind-protect
            (progn
              (with-selected-frame frame (switch-to-buffer buffer))
              (let ((project-frame-sessions-discovery-function #'ignore))
                (project-frame-sessions-save frame))
              (let ((project-frame-sessions-preserve-buffers-after-frame-close
                     preserve))
                (project-frame-sessions-delete-frame frame))
              (should-not (frame-live-p frame))
              (should (eq (buffer-live-p buffer) preserve)))
          (when (frame-live-p frame)
            (let ((project-frame-sessions--delete-suppressed frame))
              (delete-frame frame t)))
          (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest project-frame-sessions-graphical-eshell-desktop-round-trip ()
  (unless (display-graphic-p) (ert-skip "No graphical display"))
  (project-frame-sessions-graphical-tests--with-store
    (let* ((work-directory (make-temp-file "pfs-eshell-work-" t))
           (frame (project-frame-sessions-graphical-tests--make-frame
                   '((name . "PFS Eshell restore"))))
           (project-frame-sessions-autosave-interval nil)
           eshell-buffer entry)
      (unwind-protect
          (progn
            (project-frame-sessions-mode 1)
            (with-selected-frame frame
              (let ((default-directory (file-name-as-directory work-directory)))
                (require 'eshell)
                (setq eshell-buffer (eshell t)))
              (tab-bar-mode 1)
              (tab-bar-rename-tab "eshell"))
            (let ((project-frame-sessions-discovery-function #'ignore))
              (setq entry (project-frame-sessions-save frame)))
            (when (buffer-live-p eshell-buffer) (kill-buffer eshell-buffer))
            (with-selected-frame frame
              (switch-to-buffer (get-buffer-create "*scratch*")))
            (project-frame-sessions--restore-entry entry frame nil)
            (let ((restored
                   (cl-find-if
                    (lambda (buffer)
                      (with-current-buffer buffer
                        (and (derived-mode-p 'eshell-mode)
                             (equal default-directory
                                    (file-name-as-directory work-directory)))))
                    (buffer-list))))
              (should (buffer-live-p restored))))
        (project-frame-sessions-mode -1)
        (when (frame-live-p frame)
          (let ((project-frame-sessions--delete-suppressed frame))
            (delete-frame frame t)))
        (dolist (buffer (buffer-list))
          (when (with-current-buffer buffer
                  (and (derived-mode-p 'eshell-mode)
                       (equal default-directory
                              (file-name-as-directory work-directory))))
            (kill-buffer buffer)))
        (ignore-errors (delete-directory work-directory t))))))

(provide 'project-frame-sessions-graphical-tests)
;;; project-frame-sessions-graphical-tests.el ends here
