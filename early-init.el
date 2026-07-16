;;; early-init.el -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil)

(setq gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-percentage 0.1)))
