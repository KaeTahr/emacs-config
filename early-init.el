;;; early-init.el --- Loaded before the GUI and package system -*- lexical-binding: t; -*-

;; Speed up startup: raise GC threshold, restore a sane value afterwards.
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 64 1024 1024))))

;; LSP servers send big JSON payloads.
(setq read-process-output-max (* 1024 1024))

;; Disable UI chrome before the first frame is drawn (no flicker).
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

;; Transparency for every frame (including daemon frames).
(push '(alpha . (95 . 90)) default-frame-alist)

(setq inhibit-startup-message t
      frame-inhibit-implied-resize t
      native-comp-async-report-warnings-errors 'silent)
