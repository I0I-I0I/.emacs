;;; project-frame-sessions-concurrency-tests.el --- Process tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'project-frame-sessions)

(defun project-frame-sessions-concurrency-tests--emacs ()
  "Return the current Emacs executable."
  (expand-file-name invocation-name invocation-directory))

(defun project-frame-sessions-concurrency-tests--wait-for (file seconds)
  "Wait at most SECONDS for FILE to appear."
  (let ((deadline (+ (float-time) seconds)))
    (while (and (not (file-exists-p file)) (< (float-time) deadline))
      (sleep-for 0.02))
    (file-exists-p file)))

(ert-deftest project-frame-sessions-separate-process-lock-exclusion ()
  (let* ((store (make-temp-file "pfs-process-store-" t))
         (user-a (make-temp-file "pfs-process-user-a-" t))
         (user-b (make-temp-file "pfs-process-user-b-" t))
         (package-directory (file-name-directory
                             (locate-library "project-frame-sessions")))
         (holder-script (make-temp-file "pfs-holder-" nil ".el"))
         (client-script (make-temp-file "pfs-client-" nil ".el"))
         (holder-buffer (generate-new-buffer " *pfs-holder*"))
         process)
    (unwind-protect
        (progn
          (with-temp-file holder-script
            (prin1
             `(progn
                (setq user-emacs-directory ,(file-name-as-directory user-a)
                      project-frame-sessions-directory ,(file-name-as-directory store))
                (add-to-list 'load-path ,package-directory)
                (require 'project-frame-sessions)
                (project-frame-sessions--with-lock (sleep-for 1.0)))
             (current-buffer)))
          (with-temp-file client-script
            (prin1
             `(progn
                (setq user-emacs-directory ,(file-name-as-directory user-b)
                      project-frame-sessions-directory ,(file-name-as-directory store))
                (add-to-list 'load-path ,package-directory)
                (require 'project-frame-sessions)
                (condition-case nil
                    (progn (project-frame-sessions--with-lock t) (kill-emacs 0))
                  (error (kill-emacs 23))))
             (current-buffer)))
          (setq process
                (make-process
                 :name "pfs-lock-holder" :buffer holder-buffer
                 :command (list (project-frame-sessions-concurrency-tests--emacs)
                                "-Q" "--batch" "-l" holder-script)
                 :noquery t))
          (should
           (project-frame-sessions-concurrency-tests--wait-for
            (expand-file-name ".transaction-lock/owner.eld" store) 5))
          (should (= 23 (call-process
                         (project-frame-sessions-concurrency-tests--emacs)
                         nil nil nil "-Q" "--batch" "-l" client-script)))
          (while (process-live-p process) (accept-process-output process 0.1))
          (should (= 0 (process-exit-status process)))
          (should (= 0 (call-process
                        (project-frame-sessions-concurrency-tests--emacs)
                        nil nil nil "-Q" "--batch" "-l" client-script))))
      (when (and process (process-live-p process)) (delete-process process))
      (kill-buffer holder-buffer)
      (mapc (lambda (file) (ignore-errors (delete-file file)))
            (list holder-script client-script))
      (mapc (lambda (directory) (ignore-errors (delete-directory directory t)))
            (list store user-a user-b)))))

(provide 'project-frame-sessions-concurrency-tests)
;;; project-frame-sessions-concurrency-tests.el ends here
