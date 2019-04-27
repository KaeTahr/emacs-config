;; Stuff related to packages
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" .  "https://melpa.org/packages/"))
(package-initialize)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; My Packages ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure use-package is installed
;; Installed using Uncle Dave's method so this config is portable
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
;; Check and install theme if needed
(unless (package-installed-p 'xresources-theme)
  (package-refresh-contents)
  (package-install 'xresources-theme))

;; Enable and start which key to make it easier to learn emacs
;; Or so you don't forget commands
(use-package which-key
  :ensure t
  :init
  (which-key-mode))

;; Enable beacon
(use-package beacon
  :ensure t
  :init
  (beacon-mode 1))

;; Can try packages without installing them
(use-package try
  :ensure t)

;; Nicer bullets for org mode
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode))))

;; Bigger buffer for ido
(use-package ido-vertical-mode
  :ensure t
  :init
  (ido-vertical-mode 1))
(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
;; Configure ido-mode
(setq ido-enable-flex-matching nil)
(setq ido-create-new-buffer 'always)
(setq ido-everywhere t )
(ido-mode 1)

;; Nicer M-x
(use-package smex
  :ensure t
  :init (smex-initialize)
  :bind
  ("M-x" . smex))

;; Avy
(use-package avy
  :ensure t
  :bind
  ("M-s" . avy-goto-char))

;; Hex colors
(use-package  rainbow-mode
  :ensure t)
;  :init (add-hook 'prog-mode-hook 'rainbow-mode))

;; evil-mode
(use-package evil
  :ensure t
  :init
  (evil-mode 1)
  :config
  (evil-set-initial-state 'term-mode 'emacs)
  (evil-set-initial-state 'dired-mode 'emacs))

;; switching windows
(use-package switch-window
  :ensure t
  :config
  (setq switch-window-input-style 'minibuffer)
  (setq switch-window-increase 4)
  (setq switch-window-threshold 2)
  (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-qwerty-shortcuts
	'("a" "s" "d" "f" "j" "k" "l" ";"))
  :bind
  ([remap other-window] . switch-window))

;; Deleting whitespace
(use-package hungry-delete
  :ensure t
  :config
  (global-hungry-delete-mode))

;; costumizable and actually useful init screen
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents . 10)))
  (setq dashboard-banner-logo-title "ドイツ世界一"))

;; autocompletion
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3)
  (company-tng-configure-default)
  :init
  (add-hook 'after-init-hook 'global-company-mode))
(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))

					;C/C++
(use-package company-irony
  :ensure t
  :config
  (require 'company)
  (add-to-list 'company-backends 'company-irony))

(use-package irony
  :ensure t
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

					;python
(use-package elpy
  :ensure t
  :config
  (add-to-list 'company-backends 'elpy-company-backend)
  :init
  (elpy-enable))
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))
(use-package py-autopep8
  :ensure t
  :config
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
  )

;; Syntax checking
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode t))

;; Rainbow pairs
(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'scheme-mode-hook 'rainbow-delimiters-mode)
  )

;; Modeline
(use-package smart-mode-line
  :ensure t
  :config
  (sml/toggle-shorten-directory)
  (sml/toggle-shorten-modes)
  (setq sml/no-confirm-load-theme t)
  (sml/setup))

;; Make not so many minor modes appear on my modeline
(use-package diminish
  :ensure t
  :init
  (diminish 'hungry-delete-mode)
  (diminish 'beacon-mode)
  (diminish 'rainbow-delimiters)
  (diminish 'which-key-mode)
  (diminish 'eldoc-mode)
  (diminish 'undo-tree-mode)
  (diminish 'rainbow-mode))

;; Improve kill ring
(use-package popup-kill-ring
  :ensure t
  :bind ("M-y" . popup-kill-end))

;; Mark multiple (like search and replace, just a little easier to control)
;; Can act like multiple cursors like sublime if you really need to have something like that
(use-package mark-multiple
  :ensure t
  :bind ("C-c q" . mark-next-like-this))

;; Make it easier to highlight a region
(use-package expand-region
  :ensure t
  :bind ("C-q" . er/expand-region))

;; Code snippets
(use-package yasnippet
  :ensure t
  :config
  (use-package yasnippet-snippets
    :ensure t)
  (yas-reload-all))

(add-hook 'c++-mode-hook 'yas-minor-mode)
(add-hook 'python-mode-hook 'yas-minor-mode)
(add-hook 'c-mode-hook 'yas-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'yas-minor-mode)
(add-hook 'latex-mode-hook 'yas-minor-mode)

;; Manage indent better
(use-package aggressive-indent
  :ensure t
  :config
  (global-aggressive-indent-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; End of packages ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; display columns
(line-number-mode 1)
(column-number-mode 1)

;; Splitting window shortcuts
(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

;; Shortcut to edit config
(defun config-visit ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "C-c e") 'config-visit)

;; Reload config
(defun config-reload ()
  (interactive)
  (load-file (expand-file-name "~/.emacs.d/init.el")))
(global-set-key (kbd "C-c r") 'config-reload)

;; Enable ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Hide Menu bar
(menu-bar-mode -1)
(tool-bar-mode -1)
;; Make sure syntax highliting is on
(global-font-lock-mode 1)
(setq python-shell-enable-font-lock nil) ; if you don't do this then python shell is unbarably slow
;; Don't display default init screen
(setq inhibit-startup-message t)

;; Display relative line numbers
(setq-default display-line-numbers 'relative)

(electric-pair-mode 1) ; Pairs for syntax
(show-paren-mode 1) ;shows parenthesis pairs
(setq c-default-style "linux") ;set indentation  style
(setq-default frame-title-format "GNU Emacs - %b") ; set better window title
(scroll-bar-mode -1)

;; set default shell for terminal applications
(defvar my-term-shell "/bin/bash")
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)
(global-set-key (kbd "<C-return>") 'ansi-term)

;;Alias for yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Change scroll behaviour
(setq scroll-conservatively 100)

;; Set Hacker symbols
(when window-system (global-prettify-symbols-mode t))

;; Org-mode configuration

(setq org-src-window-setup 'current-window) ; Edit code in same window
(add-hook 'org-mode-hook 'org-indent-mode) ; Visual indentation

;; Transpanren
(set-frame-parameter (selected-frame) 'alpha ' (95 . 90))
(add-to-list 'default-frame-alist '(alpha . (95 . 90)))

 (defun toggle-transparency ()
   (interactive)
   (let ((alpha (frame-parameter nil 'alpha)))
     (set-frame-parameter
      nil 'alpha
      (if (eql (cond ((numberp alpha) alpha)
                     ((numberp (cdr alpha)) (cdr alpha))
                     ;; Also handle undocumented (<active> <inactive>) form.
                     ((numberp (cadr alpha)) (cadr alpha)))
               100)
          '(95 . 90) '(100 . 100)))))
(global-set-key (kbd "C-c t") 'toggle-transparency)

;; disable line numbers on the shell and terminal modes
(add-hook 'shell-mode-hook (lambda ()
			     (setq display-line-numbers 'nil)))
(add-hook 'term-mode-hook (lambda ()
			    (setq display-line-numbers 'nil)))
(add-hook 'dashboard-mode-hook (lambda ()
				 (setq display-line-numbers 'nil)))

;; Fonts auto generated by M-x customize
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(custom-enabled-themes (quote (xresources)))
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "86704574d397606ee1433af037c46611fb0a2787e8b6fd1d6c96361575be72d2" "3cd4f09a44fe31e6dd65af9eb1f10dc00d5c2f1db31a427713a1784d7db7fdfc" default)))
 '(gdb-many-windows t)
 '(package-selected-packages
   (quote
    (aggressive-indent agressive-indent elpy-company elpy py-autopep8 flycheck yasnippet-snippets yasnippet company-irony expand-region mark-multiple popup-kill-ring company hungry-delete evil rainbow-mode avy smex org-bullets try beacon xresources-theme nyx-theme which-key use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#110e12" :foreground "#ffffff" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 150 :width normal :foundry "xos4" :family "xos4 Terminus"))))
 '(company-scrollbar-bg ((t (:background "#2c3324662ecc"))))
 '(company-scrollbar-fg ((t (:background "#1e9919332066"))))
 '(company-tooltip ((t (:inherit default :background "#1670127a17c2"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
 '(mode-line ((t (:background "black" :foreground "#9278d6" :box (:line-width -1 :style released-button))))))

;; Highlight line on cursor
(when window-system(global-hl-line-mode t))
(set-face-background 'hl-line "#171318")

;; set company colors
  (require 'color)
  
  (let ((bg (face-attribute 'default :background)))
    (custom-set-faces
     `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
     `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
     `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
     `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
     `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))

;; set modeline colors
(sml/apply-theme 'respectful)

;; set indentation highlight color for languages that need it
(set-face-background 'highlight-indentation-face "#1e9919332066")
