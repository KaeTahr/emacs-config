;;; init.el --- KaeTah'r's Emacs config -*- lexical-binding: t; -*-

;;;; ------------------------------------------------------------------
;;;; Package management
;;;; ------------------------------------------------------------------

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(require 'use-package)
(setq use-package-always-ensure t)

;; Keep Customize's output out of init.el (and out of git).
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file t)

;;;; ------------------------------------------------------------------
;;;; Per-machine settings
;;;; ------------------------------------------------------------------
;; Defaults here; machines/<hostname>.el (tracked) overrides them;
;; local.el (untracked) is for secrets or one-off experiments.

(defvar my/font-family "Terminus (TTF)")
(defvar my/font-height 140)

(load (locate-user-emacs-file (format "machines/%s.el" (system-name))) t)
(load (locate-user-emacs-file "local.el") t)

;; Fall back to a commonly installed mono font if the preferred one is
;; missing, and always apply the height (so a missing font doesn't leave
;; us at Emacs' tiny default size).
(defvar my/font-fallbacks '("DejaVu Sans Mono" "Noto Sans Mono" "Liberation Mono"))

(let ((family (seq-find (lambda (f) (find-font (font-spec :family f)))
                        (cons my/font-family my/font-fallbacks))))
  (unless (equal family my/font-family)
    (message "Font %S not found, using %S" my/font-family family))
  (set-face-attribute 'default nil :height my/font-height)
  (when family
    (set-face-attribute 'default nil :family family)))

;;;; ------------------------------------------------------------------
;;;; Environment (so Emacs sees nvm's node, ~/.local/bin, etc.)
;;;; ------------------------------------------------------------------

(use-package exec-path-from-shell
  :if (or (daemonp) (memq window-system '(x pgtk)))
  :config
  ;; nvm is set up in .bashrc, so we need an interactive shell.
  (setq exec-path-from-shell-arguments '("-l" "-i"))
  (exec-path-from-shell-initialize))

;;;; ------------------------------------------------------------------
;;;; Sane defaults
;;;; ------------------------------------------------------------------

(setq use-short-answers t
      ring-bell-function #'ignore
      scroll-conservatively 100
      ;; Backups and auto-saves go in one place instead of littering dirs.
      backup-directory-alist `(("." . ,(locate-user-emacs-file "backups/")))
      auto-save-file-name-transforms `((".*" ,(locate-user-emacs-file "auto-saves/") t))
      create-lockfiles nil
      c-default-style "linux"
      python-shell-enable-font-lock nil) ; inferior python is slow otherwise
(make-directory (locate-user-emacs-file "auto-saves/") t)
(setq-default indent-tabs-mode nil
              frame-title-format "GNU Emacs - %b")

;; Reload files changed on disk (e.g. by Claude) within a second. Buffers
;; with unsaved edits are never reloaded; use M-x revert-buffer for those.
(setq auto-revert-interval 1
      auto-revert-check-vc-info t               ; keep mode-line git branch current
      global-auto-revert-non-file-buffers t     ; refresh dired too
      auto-revert-verbose nil)
(global-auto-revert-mode 1)
(column-number-mode 1)
(electric-pair-mode 1)
(show-paren-mode 1)
(global-hl-line-mode 1)
(savehist-mode 1)
(recentf-mode 1)
(save-place-mode 1)
(pixel-scroll-precision-mode 1)
(when window-system (global-prettify-symbols-mode 1))

;; Ruler at column 80 in code (color set in `my/apply-faces').
(setq-default display-fill-column-indicator-column 80
              display-fill-column-indicator-character ?┊)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;; Line numbers only where they make sense (not in terminals/dashboard).
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)

;; Code folding + spell checking in comments/strings.
(add-hook 'prog-mode-hook #'hs-minor-mode)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)

;;;; ------------------------------------------------------------------
;;;; Personal helpers & keys
;;;; ------------------------------------------------------------------

(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (other-window 1))
(global-set-key (kbd "C-x 2") #'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (other-window 1))
(global-set-key (kbd "C-x 3") #'split-and-follow-vertically)

(defun config-visit ()
  (interactive)
  (find-file (locate-user-emacs-file "init.el")))
(global-set-key (kbd "C-c e") #'config-visit)

(defun config-reload ()
  (interactive)
  (load-file (locate-user-emacs-file "init.el")))
(global-set-key (kbd "C-c r") #'config-reload)

(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(95 . 90) '(100 . 100)))))
(global-set-key (kbd "C-c T") #'toggle-transparency)

(global-set-key (kbd "C-x C-b") #'ibuffer)

;;;; ------------------------------------------------------------------
;;;; Theme (xresources, daemon-aware)
;;;; ------------------------------------------------------------------

(use-package xresources-theme :defer t)   ; loaded via load-theme below

(defvar my:theme 'xresources)
(defvar my:theme-loaded nil)

(defun my/apply-faces ()
  (set-face-background 'hl-line "#171318")
  ;; xresources-theme sets this to the plain background, which makes the
  ;; selected candidate in vertico (and other selections) invisible.
  (set-face-attribute 'highlight nil :background "#3a2d4a" :foreground 'unspecified)
  ;; Gutter (fringe + line numbers) as its own panel, set apart from the code.
  (set-face-attribute 'fringe nil :background "#1a161c")
  (set-face-attribute 'line-number nil :background "#1a161c" :foreground "#4f4655")
  (set-face-attribute 'line-number-current-line nil
                      :background "#1a161c" :foreground "#9278d6" :weight 'bold)
  (set-face-attribute 'fill-column-indicator nil
                      :foreground "#2a2330" :background 'unspecified)
  (set-face-attribute 'mode-line nil :background "black" :foreground "#9278d6"
                      :box '(:line-width -1 :style released-button)))

(defun my/load-theme-for-frame (&optional frame)
  "Load the theme once, on the first graphical frame.
xresources-theme reads X resources, which only exist with a GUI
display. In daemon mode that means waiting for a real frame.
Terminal frames (emacs -nw) just keep the terminal's colors."
  (with-selected-frame (or frame (selected-frame))
    (when (and (display-graphic-p) (not my:theme-loaded))
      (load-theme my:theme t)
      (my/apply-faces)
      (setq my:theme-loaded t))))

(if (daemonp)
    (add-hook 'after-make-frame-functions #'my/load-theme-for-frame)
  (my/load-theme-for-frame))

;;;; ------------------------------------------------------------------
;;;; Evil
;;;; ------------------------------------------------------------------

(use-package evil
  :init
  (setq evil-want-keybinding nil      ; required by evil-collection
        evil-want-C-u-scroll t
        evil-undo-system 'undo-redo)
  :config
  (evil-mode 1)
  (dolist (mode '(term-mode ghostel-mode dired-mode eshell-mode gdb-mode
                            claude-code-ide-mode))
    (evil-set-initial-state mode 'emacs)))

(use-package evil-collection
  :after evil
  :config
  ;; Only the modes where vim keys really help; extend as needed.
  (evil-collection-init '(magit xref flymake help info ibuffer compile
                                eglot vertico corfu consult)))

;;;; ------------------------------------------------------------------
;;;; Minibuffer completion: vertico + orderless + marginalia + consult
;;;; ------------------------------------------------------------------

(use-package vertico
  :init (vertico-mode 1)
  :bind (:map vertico-map
              ("C-n" . vertico-next)
              ("C-p" . vertico-previous)))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :init (marginalia-mode 1))

(use-package consult
  :bind (("C-x b"   . consult-buffer)
         ("M-y"     . consult-yank-pop)    ; browse kill ring
         ("C-c l"   . consult-line)
         ("C-c g"   . consult-ripgrep)     ; project-wide search (needs rg)
         ("C-c i"   . consult-imenu)
         ("C-c f"   . consult-recent-file)
         ("C-c !"   . consult-flymake)))

;; Project-wide search & replace, VSCode style:
;;   C-c g  search  ->  C-c C-e  export results to a buffer
;;   C-c C-p  make that buffer editable  ->  edit (e.g. :%s/old/new/)
;;   C-c C-c  write changes to all files   (C-c C-k to abort)
;; Simpler built-in alternative: C-x p r (query-replace across the project).
(use-package embark
  :bind (("C-." . embark-act)              ; actions on thing at point / candidate
         ("C-h B" . embark-bindings)
         :map minibuffer-local-map
         ("C-c C-e" . embark-export))
  :init
  ;; Evil's normal state binds C-. (repeat-pop); embark is more useful there.
  (with-eval-after-load 'evil
    (define-key evil-normal-state-map (kbd "C-.") #'embark-act)))

(use-package embark-consult
  :after (embark consult))

(use-package wgrep
  :custom
  (wgrep-auto-save-buffer t)               ; save edited files automatically
  (wgrep-change-readonly-file t))

;;;; ------------------------------------------------------------------
;;;; In-buffer completion: corfu
;;;; ------------------------------------------------------------------

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.15)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  :init
  (global-corfu-mode 1)
  (corfu-popupinfo-mode 1))

(use-package cape
  :init
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-dabbrev))

;;;; ------------------------------------------------------------------
;;;; Tree-sitter, LSP (eglot), linting (flymake), formatting (apheleia)
;;;; ------------------------------------------------------------------

;; Uses python-ts-mode, tsx-ts-mode, etc., and offers to install grammars.
(use-package treesit-auto
  :custom (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode 1))

(use-package eglot
  :ensure nil
  :hook ((python-base-mode
          js-base-mode
          typescript-ts-base-mode) . eglot-ensure)
  :bind (:map eglot-mode-map
              ("C-c a" . eglot-code-actions)
              ("C-c n" . eglot-rename)
              ("C-c d" . eldoc-doc-buffer))
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-config '(:size 0)) ; faster: no event logging
  ;; basedpyright defaults to its strictest mode ("recommended"): hundreds of
  ;; "type is unknown" hints on untyped code. "standard" = real errors only.
  (eglot-workspace-configuration
   '(:basedpyright (:analysis (:typeCheckingMode "standard"))))
  :config
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("basedpyright-langserver" "--stdio"))))

;; ruff lints in the flymake diagnostics alongside basedpyright.
(use-package flymake-ruff
  :hook (eglot-managed-mode . (lambda ()
                                (when (derived-mode-p 'python-base-mode)
                                  (flymake-ruff-load)))))

(use-package flymake
  :ensure nil
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)))

;; Format on save: prettier (project-local via npx) for JS/TS.
;; Python is NOT formatted on save (legacy files would reformat entirely and
;; blow up diffs); run M-x apheleia-format-buffer by hand when wanted.
(use-package apheleia
  :hook (python-base-mode . (lambda () (setq-local apheleia-inhibit t)))
  :config
  (setf (alist-get 'python-mode apheleia-mode-alist) '(ruff-isort ruff))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff-isort ruff))
  (apheleia-global-mode 1))

;; ESLint alongside the TypeScript server (eglot runs one server per buffer),
;; using the project's own node_modules/.bin/eslint.
(use-package flymake-eslint
  :custom (flymake-eslint-prefer-json-diagnostics t)
  :init
  (defun my/maybe-enable-eslint ()
    (when-let* (((derived-mode-p 'js-base-mode 'typescript-ts-base-mode))
                (root (locate-dominating-file default-directory
                                              "node_modules/.bin/eslint")))
      (setq-local flymake-eslint-executable-name
                  (expand-file-name "node_modules/.bin/eslint" root)
                  flymake-eslint-project-root root)
      (flymake-eslint-enable)))
  ;; eglot resets the diagnostic backends when it starts, so hook there too.
  (add-hook 'eglot-managed-mode-hook #'my/maybe-enable-eslint)
  (add-hook 'js-base-mode-hook #'my/maybe-enable-eslint)
  (add-hook 'typescript-ts-base-mode-hook #'my/maybe-enable-eslint))

;; Debugger.
;; VSCode-style keys: F5 start/continue, F9 breakpoint, F10 step over,
;; F11 step in, S-F11 step out, S-F5 stop, C-S-F5 restart.
;; Everything else is under C-x C-a (e.g. C-x C-a c to set a condition).
(use-package dape
  :bind (("<f5>"    . my/dape-start-or-continue)
         ("S-<f5>"  . dape-quit)
         ("C-S-<f5>" . dape-restart)
         ("<f9>"    . dape-breakpoint-toggle)
         ("<f10>"   . dape-next)
         ("<f11>"   . dape-step-in)
         ("S-<f11>" . dape-step-out))
  :hook (kill-emacs . dape-breakpoint-save)    ; breakpoints survive restarts
  :custom
  (dape-buffer-window-arrangement 'right)
  (dape-inlay-hints t)                         ; show variable values inline
  :init
  (defun my/dape-start-or-continue ()
    (interactive)
    (require 'dape)
    (call-interactively
     (if (bound-and-true-p dape-active-mode) #'dape-continue #'dape)))

  (defun my/django-root ()
    (or (locate-dominating-file default-directory "manage.py")
        (user-error "No manage.py found above %s" default-directory)))
  :config
  (dape-breakpoint-load)

  ;; `M-x dape RET django' : runserver under the debugger.
  ;; Reuses dape's built-in debugpy config, pointed at manage.py.
  (let ((cfg (copy-sequence (cdr (assq 'debugpy dape-configs)))))
    (setq cfg (plist-put cfg :program
                         (lambda () (expand-file-name "manage.py" (my/django-root)))))
    (setq cfg (plist-put cfg :cwd #'my/django-root))
    (setq cfg (plist-put cfg :args ["runserver" "--noreload"]))
    (setq cfg (plist-put cfg :django t))
    (add-to-list 'dape-configs (cons 'django cfg))))

;; Per-project environments from direnv (.envrc).
(use-package envrc
  :if (executable-find "direnv")
  :hook (after-init . envrc-global-mode))

;; Django templates (and other HTML).
(use-package web-mode
  :mode ("\\.html?\\'" "\\.djhtml\\'")
  :custom
  (web-mode-engines-alist '(("django" . "\\.html?\\'")))
  (web-mode-markup-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-css-indent-offset 2))

;;;; ------------------------------------------------------------------
;;;; Markdown (rendering uses pandoc)
;;;; ------------------------------------------------------------------
;; In Emacs:  C-c C-x C-m  hide/show markup (**, #, links) for a clean read
;;            C-c C-c l    live preview side by side (eww; no mermaid)
;; Browser:   C-c C-c m    live preview with mermaid diagrams

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :custom
  (markdown-command "pandoc -f gfm -t html5")
  (markdown-header-scaling t)                  ; bigger headings
  (markdown-fontify-code-blocks-natively t)    ; real syntax colors in ``` blocks
  (markdown-italic-underscore t)
  :bind (:map markdown-mode-map
              ("C-c C-c m" . markdown-preview-mode))
  :config
  ;; Headers in the same font as the text (header scaling otherwise switches
  ;; them to variable-pitch, which has no font set); colour unchanged.
  (set-face-attribute 'markdown-header-face nil
                      :inherit 'font-lock-function-name-face))

(use-package markdown-preview-mode
  :commands markdown-preview-mode
  :config
  ;; GitHub's dark style + mermaid's dark theme (the package default,
  ;; Solarized Dark, clashes with mermaid's themes).
  (setq markdown-preview-stylesheets
        (list "https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/5.8.1/github-markdown-dark.min.css"
              "<style>html,body{background:#0d1117}.markdown-body{max-width:980px;margin:0 auto;padding:32px}</style>"))
  ;; Render ```mermaid blocks on every update.
  (add-to-list 'markdown-preview-javascript
               "https://cdn.jsdelivr.net/npm/mermaid@11.4.1/dist/mermaid.min.js")
  (setq markdown-preview-script-oninit
        "mermaid.initialize({ startOnLoad: false, theme: 'dark' });"
        markdown-preview-script-onupdate
        "document.querySelectorAll('code.language-mermaid, pre.mermaid > code').forEach(function (c) {
           var pre = c.parentNode; pre.className = 'mermaid'; pre.textContent = c.textContent;
         });
         mermaid.run({ querySelector: 'pre.mermaid' });"))

;;;; ------------------------------------------------------------------
;;;; Git
;;;; ------------------------------------------------------------------

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package diff-hl
  :hook ((after-init . global-diff-hl-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :custom
  (diff-hl-draw-borders nil)            ; solid bars instead of outlined ones
  :custom-face
  (diff-hl-insert ((t (:foreground "#50c878" :background "#50c878"))))
  (diff-hl-change ((t (:foreground "#3794ff" :background "#3794ff"))))
  (diff-hl-delete ((t (:foreground "#e05561" :background "#e05561"))))
  :config
  (diff-hl-flydiff-mode 1)              ; update as you type, not just on save

  ;; Commits made outside Emacs/magit don't touch the file, so nothing tells
  ;; diff-hl. auto-revert re-checks each buffer's git state; redraw the
  ;; markers when that state changes (e.g. edited -> committed).
  (define-advice vc-refresh-state (:around (fn &rest args) my/diff-hl-on-vc-change)
    (let ((before (and buffer-file-name (vc-state buffer-file-name))))
      (apply fn args)
      (when (and diff-hl-mode buffer-file-name
                 (not (eq before (vc-state buffer-file-name))))
        (diff-hl-update)))))

;; GitLens-style blame at the end of the current line.
;; For full-file blame: M-x magit-blame (or C-x v g).
(use-package blamer
  :hook (after-init . global-blamer-mode)      ; on everywhere by default
  :bind (("C-c b" . blamer-mode)               ; toggle off/on in this buffer
         ("C-c B" . blamer-show-commit-info))  ; popup with full commit info
  :custom
  (blamer-idle-time 0.5)
  (blamer-min-offset 40)
  (blamer-author-formatter "  %s, ")
  (blamer-datetime-formatter "%s")
  (blamer-commit-formatter " · %s")
  :custom-face
  (blamer-face ((t (:foreground "#4f4655" :background unspecified
                                :italic t :height 0.9)))))

;;;; ------------------------------------------------------------------
;;;; Remote files (TRAMP): C-x C-f /ssh:user@host:/path  (or /sftp:...)
;;;; ------------------------------------------------------------------

(use-package tramp
  :ensure nil
  :defer t
  :custom
  (tramp-default-method "ssh")                    ; faster than scp for small files
  (remote-file-name-inhibit-locks t)              ; no .#lock files on the server
  (remote-file-name-inhibit-auto-save-visited t)
  (tramp-use-scp-direct-remote-copying t)
  (tramp-verbose 1)
  :config
  ;; Reuse one SSH connection for everything (big speedup).
  (setq tramp-ssh-controlmaster-options
        "-o ControlMaster=auto -o ControlPath=tramp.%%C -o ControlPersist=60")
  ;; Async processes over ssh: makes magit / eglot over TRAMP much faster.
  (connection-local-set-profile-variables
   'remote-direct-async-process '((tramp-direct-async-process . t)))
  (connection-local-set-profiles
   '(:application tramp :protocol "ssh") 'remote-direct-async-process))

;;;; ------------------------------------------------------------------
;;;; AI: Copilot (inline suggestions) and Claude Code
;;;; ------------------------------------------------------------------

(defun my/maybe-enable-copilot ()
  "Enable copilot only once its language server is installed.
Otherwise copilot-mode errors in every code buffer on a fresh machine."
  ;; `copilot-server-executable' isn't autoloaded, so load copilot first;
  ;; otherwise this is a void-function swallowed by `ignore-errors'.
  (if (and (require 'copilot nil t)
           (ignore-errors (copilot-server-executable)))
      (progn (copilot-mode 1)
             (copilot-nes-mode 1))
    (message "Copilot server missing: run M-x copilot-install-server")))

;; Next Edit Suggestions: TAB accepts, C-g dismisses. Its keys are only
;; bound while a suggestion shows; let them win over evil's TAB then.
(with-eval-after-load 'copilot-nes
  (with-eval-after-load 'evil
    (evil-make-overriding-map copilot-nes-mode-map 'normal)
    (add-hook 'copilot-nes-mode-hook #'evil-normalize-keymaps)))

(use-package copilot
  :hook (prog-mode . my/maybe-enable-copilot)
  :bind (:map copilot-completion-map
              ;; When the corfu popup is open, corfu's TAB wins; otherwise
              ;; TAB accepts the copilot ghost text.
              ("<tab>" . copilot-accept-completion)
              ("TAB"   . copilot-accept-completion)
              ("M-f"   . copilot-accept-completion-by-word)
              ("M-e"   . copilot-accept-completion-by-line)
              ("M-n"   . copilot-next-completion)
              ("M-p"   . copilot-previous-completion))
  :custom
  (copilot-indent-offset-warning-disable t)
  (copilot-max-char-warning-disable t))

;; Terminal emulator (used by Claude Code; M-x ghostel for a shell).
;; Flat tabs matching the gutter instead of the default raised grey buttons.
(use-package tab-line
  :ensure nil
  :defer t
  :custom
  (tab-line-separator "")
  :custom-face
  (tab-line ((t (:inherit nil :background "#1a161c" :foreground "#4f4655"
                          :box nil :height 0.9))))
  (tab-line-tab ((t (:inherit tab-line :background "#110e12" :foreground "#9278d6"
                              :box nil))))
  (tab-line-tab-inactive ((t (:inherit tab-line :box nil))))
  (tab-line-tab-inactive-alternate ((t (:inherit tab-line :box nil))))
  (tab-line-tab-current ((t (:inherit tab-line :background "#110e12"
                                      :foreground "#9278d6" :weight bold :box nil))))
  (tab-line-highlight ((t (:background "#3a2d4a" :foreground "#ffffff" :box nil)))))

;; C-c t toggles a project terminal panel along the bottom of the frame;
;; C-u C-c t opens another terminal in it. Terminals show as tabs:
;; C-c ] / C-c [ next/previous tab.
(use-package ghostel
  :bind (("C-c t" . my/toggle-terminal)
         :map ghostel-mode-map
         ("C-c ]" . tab-line-switch-to-next-tab)
         ("C-c [" . tab-line-switch-to-prev-tab))
  :init
  (defconst my/terminal-buffer-regexp "\\*[^*]*ghostel[^*]*\\*")

  (defun my/terminal-buffers ()
    (sort (seq-filter (lambda (b) (string-match-p my/terminal-buffer-regexp
                                                  (buffer-name b)))
                      (buffer-list))
          :key #'buffer-name))

  (defun my/terminal-tab-name (buffer &optional _buffers)
    "\"*proj-ghostel*<2>\" -> \" proj 2 \"."
    (let* ((name (buffer-name buffer))
           (num (and (string-match "<\\([0-9]+\\)>\\'" name) (match-string 1 name)))
           (base (replace-regexp-in-string
                  "-?ghostel" "" (string-trim (replace-regexp-in-string "<[0-9]+>\\'" "" name)
                                              "\\*" "\\*"))))
      (format " %s%s " (if (string-empty-p base) "terminal" base)
              (if num (concat " " num) ""))))

  (defun my/new-terminal ()
    (interactive)
    (if (project-current) (ghostel-project-new) (ghostel-new)))

  (add-hook 'ghostel-mode-hook
            (lambda ()
              (when (string-match-p my/terminal-buffer-regexp (buffer-name))
                (setq-local tab-line-tabs-function #'my/terminal-buffers
                            tab-line-tab-name-function #'my/terminal-tab-name
                            tab-line-new-tab-choice #'my/new-terminal
                            tab-line-close-tab-function #'kill-buffer)
                (tab-line-mode 1))))

  ;; Shell terminals (not the Claude buffer) open as a full-width bottom panel.
  (add-to-list 'display-buffer-alist
               `(,my/terminal-buffer-regexp
                 (display-buffer-reuse-window display-buffer-in-side-window)
                 (side . bottom)
                 (slot . 0)
                 (window-height . 0.3)))

  (defun my/toggle-terminal (&optional new)
    "Focus, hide, or open the bottom terminal panel.
With prefix argument NEW, open an additional terminal."
    (interactive "P")
    (let ((win (seq-find (lambda (w)
                           (string-match-p my/terminal-buffer-regexp
                                           (buffer-name (window-buffer w))))
                         (window-list))))
      (cond (new (my/new-terminal))
            ((and win (eq win (selected-window))) (delete-window win))
            (win (select-window win))
            ((when-let* ((last (seq-find (lambda (b)
                                           (string-match-p my/terminal-buffer-regexp
                                                           (buffer-name b)))
                                         (buffer-list))))
               (pop-to-buffer last)))
            ((project-current) (ghostel-project))
            (t (ghostel))))))

;; Claude Code inside Emacs, connected to the editor (selection,
;; diagnostics, diffs in ediff).
(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :bind ("C-c C-'" . claude-code-ide-menu)
  :custom (claude-code-ide-terminal-backend 'ghostel)
  :config (claude-code-ide-emacs-tools-setup))

;;;; ------------------------------------------------------------------
;;;; Editing helpers
;;;; ------------------------------------------------------------------

(use-package which-key
  :ensure nil                           ; built in since Emacs 30
  :init (which-key-mode 1))

(use-package diminish)

(use-package avy
  :bind ("M-s" . avy-goto-char))

(use-package expand-region
  :bind ("C-q" . er/expand-region))

(use-package hungry-delete
  :diminish
  :config (global-hungry-delete-mode 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :diminish)

(use-package yasnippet
  :diminish yas-minor-mode
  :hook (prog-mode . yas-minor-mode)
  :config (yas-reload-all))
(use-package yasnippet-snippets :after yasnippet)

(use-package switch-window
  :bind ([remap other-window] . switch-window)
  :custom
  (switch-window-input-style 'minibuffer)
  (switch-window-increase 4)
  (switch-window-threshold 2)
  (switch-window-shortcut-style 'qwerty)
  (switch-window-qwerty-shortcuts '("a" "s" "d" "f" "j" "k" "l" ";")))

(use-package beacon
  :diminish
  :config (beacon-mode 1))

(use-package try)

;;;; ------------------------------------------------------------------
;;;; Project tree
;;;; ------------------------------------------------------------------

;; Inside the tree: RET/TAB open/expand, `?' shows all keys,
;; `cf' new file, `cd' new dir, `R' rename, `d' delete, `m' move.
(use-package treemacs
  :bind (("<f1>"  . treemacs)
         ("C-c 0" . treemacs-select-window))  ; jump focus to the tree
  :custom
  (treemacs-width 32)
  (treemacs-is-never-other-window t)  ; C-x o / switch-window skip it
  (treemacs-show-hidden-files t)
  (treemacs-collapse-dirs 3)          ; a/b/c shown as one row
  :config
  (treemacs-project-follow-mode 1)    ; show the current project, not a fixed list
  (treemacs-follow-mode 1)            ; highlight the current file
  (treemacs-filewatch-mode 1)
  (treemacs-git-mode 'deferred)
  (treemacs-hide-gitignored-files-mode 1)
  (treemacs-fringe-indicator-mode 'always)
  :custom-face
  ;; Same meaning as the diff-hl colors (the theme makes most of these red).
  (treemacs-git-modified-face ((t (:foreground "#3794ff"))))
  (treemacs-git-added-face ((t (:foreground "#50c878"))))
  (treemacs-git-untracked-face ((t (:foreground "#50c878" :slant italic))))
  (treemacs-git-renamed-face ((t (:foreground "#9278d6"))))
  (treemacs-git-conflict-face ((t (:foreground "#e05561" :weight bold))))
  (treemacs-git-ignored-face ((t (:foreground "#4f4655")))))
(use-package treemacs-evil :after (treemacs evil))
(use-package treemacs-magit :after (treemacs magit))

;;;; ------------------------------------------------------------------
;;;; Org
;;;; ------------------------------------------------------------------

(use-package org
  :ensure nil
  :custom (org-src-window-setup 'current-window)
  :hook ((org-mode . org-indent-mode)
         (org-mode . auto-fill-mode)
         (org-mode . (lambda () (setq fill-column 80)))))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(use-package htmlize)                   ; org HTML export of code blocks

;;;; ------------------------------------------------------------------
;;;; Misc packages
;;;; ------------------------------------------------------------------

;; Weekly package updates; delete superseded versions so elpa/ doesn't bloat.
(use-package auto-package-update
  :custom
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t)
  (auto-package-update-interval 7)
  :config
  (auto-package-update-maybe))

;;;; ------------------------------------------------------------------
;;;; Waifu & roll (optional: separate repos cloned into lisp/)
;;;; ------------------------------------------------------------------

(dolist (pkg '("waifu" "roll"))
  (let ((dir (locate-user-emacs-file (concat "lisp/" pkg))))
    (when (file-directory-p dir)
      (add-to-list 'load-path dir))))

(when (require 'waifu nil t)
  (setq waifu-name "Holo"
        weeb-name "KaeTah'r")
  (global-set-key (kbd "C-c c") 'comfort))
(require 'roll nil t)

;;;; ------------------------------------------------------------------
;;;; Dashboard
;;;; ------------------------------------------------------------------

(use-package dashboard
  :config
  (let ((banner (expand-file-name "~/Pictures/cropped2.png")))
    (setq dashboard-items '((recents . 10) (projects . 5))
          dashboard-projects-backend 'project-el
          dashboard-center-content t
          dashboard-banner-logo-title "Holo welcomes you to GNU Emacs, KaeTah'r!"
          dashboard-startup-banner (if (file-exists-p banner) banner 'logo)))
  (when (fboundp 'get-comfort)
    (setq dashboard-footer-messages (list (get-comfort))))
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name))))

;;; init.el ends here
