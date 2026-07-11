;;; project-frame-sessions-graphical-tests.el --- Graphical tests -*- lexical-binding: t; -*-

(require 'ert)
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

(ert-deftest project-frame-sessions-graphical-real-desktop-round-trip ()
  (unless (display-graphic-p) (ert-skip "No graphical display"))
  (project-frame-sessions-graphical-tests--with-store
    (let* ((source (make-temp-file "pfs-graphical-buffer-" nil ".txt" "saved text"))
           (buffer (find-file-noselect source))
           (frame (make-frame '((name . "PFS graphical test"))))
           entry)
      (unwind-protect
          (progn
            (with-selected-frame frame
              (switch-to-buffer buffer)
              (tab-bar-mode 1)
              (tab-bar-new-tab)
              (tab-bar-rename-tab "Second")
              (switch-to-buffer (get-buffer-create "*pfs-second*")))
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
            (should (>= (length (frame-parameter frame 'tabs)) 2)))
        (when (frame-live-p frame)
          (let ((project-frame-sessions--delete-suppressed frame))
            (delete-frame frame t)))
        (when (buffer-live-p buffer) (kill-buffer buffer))
        (when-let* ((second (get-buffer "*pfs-second*"))) (kill-buffer second))
        (ignore-errors (delete-file source))))))

(provide 'project-frame-sessions-graphical-tests)
;;; project-frame-sessions-graphical-tests.el ends here
