;;; sprawl-terminal.el --- Laptop-specific settings -*- lexical-binding: t; -*-

;; Loaded automatically when (system-name) is "sprawl-terminal".
;; Put anything here that should differ from the desktop.

(setq my/font-family "Terminus (TTF)"
      ;; ~162 DPI panel but X reports 96 DPI, so this needs to be
      ;; larger than the desktop value to look the same size.
      my/font-height 200)
