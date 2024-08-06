(eval-when-compile
  (require 'use-package))
(require 'diminish)
;; -*- lexical-binding: t -*-
(setq emacs-data-directory (let (
				 (local-share
				  (or
				   (getenv "XDG_DATA_HOME")
				   (concat (getenv "HOME") "/.local/share"))))
			     (expand-file-name (concat local-share "/emacs/"))))

(unless (file-directory-p emacs-data-directory)
  (make-directory emacs-data-directory))

;; Cleanup
(setq backup-directory-alist `(("." . ,(expand-file-name "backup-files" emacs-data-directory))))


(let ((auto-save-dir (expand-file-name "auto-saves/" emacs-data-directory)))
  (unless (file-directory-p auto-save-dir)
    (make-directory auto-save-dir))
  (setq auto-save-list-file-prefix auto-save-dir
	auto-save-file-name-transforms `((".*" ,auto-save-dir t))))

(setq create-lockfiles nil)

;; Emacs
(add-function :after after-focus-change-function
	(defun th/garbage-collect ()
	  (unless (frame-focus-state)
	    (garbage-collect))))

(setq use-short-answers t
      native-comp-async-report-warnings-errors 'silent
      indent-tabs-mode t
      c-basic-offset 'tab-width
      tab-width 4
      gc-cons-threshold (* 8 1024 1024)

      ring-bell-function 'ignore ;; i hate that stupid bell

      inhibit-startup-screen t
      inhibit-splash-screen t

      display-line-numbers-type 'relative

      recentf-save-file (expand-file-name "recentf" emacs-data-directory)

      backward-delete-char-untabify-method nil)

(electric-indent-mode)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode 1)
(global-visual-line-mode t)

(recentf-mode)

(dolist (mode '(vterm-mode-hook
	  simple-mpc-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(global-set-key (kbd "s-`") #'(lambda () (interactive) (insert "`"))) ;; weird keyboard shenanigans
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "ESC") 'keyboard-escape-quit)

(set-face-attribute 'default nil
		    :family "Monospace"
		    :height 90
		    :weight 'regular)

(set-face-attribute 'variable-pitch nil
		    :font "Sans-Serif"
		    :height 120
		    :weight 'medium)

(set-face-attribute 'fixed-pitch nil
		    :font "Monospace"
		    :weight 'medium)

(set-face-attribute 'font-lock-comment-face nil
		    :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
		    :slant 'italic)

(add-to-list 'default-frame-alist '(font . "Monospace"))

(setq-default line-spacing 0.12)

(setq scroll-conservatively 10)
(setq scroll-margin 7)
(setq pixel-scroll-precision-large-scroll-height 40.0)
(setq pixel-scroll-precision-use-momentum t)

;; Functions
(defun make-mode-keymap (map outer)
  (mapc (lambda (inner)
	  (define-key map (kbd (car inner)) (cdr inner)))
	outer))

(defun comment-or-uncomment-region-or-line ()
  "If a region is selected, either uncomment or comment it, if not, uncomment or comment the current line."
  (interactive)
  (let (beg end)
    (if (region-active-p)
	(setq beg (region-beginning) end (region-end))
      ;; else
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(defun eval-region-and-go-to-normal-mode ()
  "Evaluate elisp in the selected region, and go back to normal mode."
  (interactive)
  (let (beg end)
    (if (region-active-p)
	(setq beg (region-beginning) end (region-end))
      ;; else
      (setq beg (line-beginning-position) end (line-end-position)))
    (eval-region beg end)
    (evil-normal-state)))

(defvar saved-window-configurations '())

(use-package subr-x)

(defun format-window-list ()
  (let ((window-list-string-formatted) (value))
    (dolist (window (window-list) value)
      (setq window-list-string-formatted (concat
					  window-list-string-formatted
					  (format "%s, " (buffer-name (window-buffer window))))))
    (setq window-list-string-formatted (string-remove-suffix ", " window-list-string-formatted))
    window-list-string-formatted))

(defun save-current-window-configuration (&optional name)
  "Add `current-window-configuration` to saved window configurations, if NAME is provided, give it a name."
  (interactive)
  (add-to-list 'saved-window-configurations `(,(or name
						   (if (string= (projectile-project-name) "-")
						       (format "%s (%s)"
							       (shell-command-to-string "date \"+%a %R\"")
							       (format-window-list))
						     (format "%s: %s (%s)"
							     (projectile-project-name)
							     (shell-command-to-string "date \"+%a %R\"")
							     (format-window-list))))
						   . ,(current-window-configuration))))

(defun new-window-configuration ()
  "Save the current window configuration, create a new window and close every other window."
  (interactive)
  (save-current-window-configuration)
  (select-window (split-window))
  (delete-other-windows))

(defun load-a-saved-window-configuration ()
  "Select a window configuration from the list."
  (interactive)
  (let ((config (cdr
		 (assoc
		  (completing-read "Select a window configuration: " saved-window-configurations)
		  saved-window-configurations))))
    (if config
	(set-window-configuration config)
      (message "Selected item is invalid, something has gone wrong."))))

(defun delete-from-saved-window-configurations ()
  "Select a window configuration to delete."
  (interactive)
  (setq saved-window-configurations
	(delq (assoc
	       (completing-read "Delete a window configuration: "
				saved-window-configurations)
	       saved-window-configurations)
	      saved-window-configurations)))

(with-eval-after-load 'diminish
  (diminish 'font-lock-mode)
  (diminish 'visual-line-mode)
  (diminish 'auto-revert-mode)
  (diminish 'eldoc-mode))

(use-package dired
  :config
  (make-mode-keymap dired-mode-map '(("SPC" . nil)
				     ("<backspace>" . dired-up-directory)))
  (unless (display-graphic-p)
    (define-key dired-mode-map (kbd "DEL") #'dired-up-directory)))

(use-package undo-tree
  :diminish undo-tree-mode
  :diminish global-undo-tree-mode
  :config
  (global-undo-tree-mode)
  (defvar th/undo-tree-dir (expand-file-name "undo-tree/" emacs-data-directory))
  (unless (file-directory-p th/undo-tree-dir)
    (make-directory th/undo-tree-dir))
  (defadvice undo-tree-make-history-save-file-name
      (after undo-tree activate)
    (setq ad-return-value (concat th/undo-tree-dir ad-return-value))))

(use-package evil
  :after undo-tree
  :init
  (setq evil-want-integration t
	evil-want-keybinding nil
	evil-vsplit-window-right t
	evil-split-window-below t
	evil-undo-system 'undo-tree)
  :config
  (evil-set-undo-system evil-undo-system)
  (evil-mode))

(use-package evil-collection
  :after evil
  :diminish evil-collection-unimpaired-mode
  :config
  (evil-collection-init '(dashboard
			  woman
			  pdf
			  dired
			  elfeed
			  wdired
			  image
			  eglot
			  ibuffer
			  simple-mpc
			  magit
			  vterm)))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-setup-side-window-bottom)
  (which-key-mode))

(use-package evil-better-visual-line
  :after evil
  :config
  (evil-better-visual-line-on))

(use-package general
  :config
  (general-evil-setup))

(general-create-definer th/leader
		  :states '(normal insert visual emacs motion)
		  :keymaps 'override
		  :prefix "SPC"
		  :global-prefix "C-SPC")

(general-create-definer th/local
		  :states '(normal insert visual emacs motion)
		  :keymaps 'override
		  :prefix "SPC l"
		  :global-prefix "C-SPC l")

(th/leader
 "w" '(:ignore t :wk "window")
 "wh" '(windmove-left :wk "move left")
 "wj" '(windmove-down :wk "move down")
 "wk" '(windmove-up :wk "move up")
 "wl" '(windmove-right :wk "move right")
 "<left>" '(windmove-left :wk "move left")
 "<down>" '(windmove-down :wk "move down")
 "<up>" '(windmove-up :wk "move up")
 "<right>" '(windmove-right :wk "move right")
 "wq" '(evil-quit :wk "close")
 "q" '(evil-quit :wk "close window")
 "ws" '(split-window-below :wk "horizontal split")
 "wv" '(split-window-right :wk "vertical split")

 "wc" '(:ignore t :wk "window configurations")
 "wcl" '(load-a-saved-window-configuration :wk "load")
 "wcs" '(save-current-window-configuration :wk "save")
 "wcn" '(new-window-configuration :wk "new")
 
 "H" '((lambda () (interactive) (evil-window-increase-width 2)) :wk "increase window width")
 "J" '((lambda () (interactive) (evil-window-increase-height 2)) :wk "increase window height")
 
 "l" '(:ignore t :wk "local (mode specific)")
 "s" '(:ignore t :wk "search")

 "o" '(:ignore t :wk "open")
 "ot" '(vterm :wk "vterm")

 ":" '(execute-extended-command :wk "M-x")
 ";" '(execute-extended-command :wk "M-x")
 "." '(find-file :wk "find file")
 ">" '((lambda () (interactive) (find-file nil (getenv "HOME"))) :wk "find file from ~/")
 
 "h" '(:ignore t :wk "help")
 "hb" '(describe-bindings t :wk "describe binding")
 "hf" '(describe-function t :wk "describe function")
 "hv" '(describe-variable t :wk "describe variable")
 
 "b" '(:ignore t :wk "buffer")
 "bi" '(ibuffer :wk "ibuffer")
 "bK" '(kill-buffer :wk "kill buffer")
 "bk" '(kill-this-buffer :wk "kill this buffer")

 "gg" '(magit-status :wk "open magit")

 "e" '(:ignore t :wk "emacs")
 "er" '(eval-region-and-go-to-normal-mode :wk "eval region or line")
 "eb" '(eval-buffer :wk "eval buffer")
 "ee" '(eval-expression :wk "eval expression"))
 
(general-define-key
 :states '(normal visual)
 "gc" 'comment-or-uncomment-region-or-line
 "<up>" 'enlarge-window
 "<left>" 'shrink-window-horizontally
 "<right>" 'enlarge-window-horizontally
 "<down>" 'shrink-window
 ";" 'evil-ex
 "C-k" 'enlarge-window
 "C-h" 'shrink-window-horizontally
 "C-l" 'enlarge-window-horizontally
 "C-j" 'shrink-window)

(general-define-key
 "C-=" 'text-scale-increase
 "C--" 'text-scale-decrease
 "<escape>" #'keyboard-quit
 "<escape>" #'keyboard-escape-quit
 "ESC" #'keyboard-quit
 "ESC" #'keyboard-escape-quit
 "<C-wheel-up>" 'text-scale-increase
 "<C-wheel-down>" 'text-scale-decrease)

(use-package smartparens
  :diminish smartparens-mode
  :config
  (smartparens-global-mode))

(use-package org
  :demand t
  :init
  (setq org-src-preserve-indentation t
	org-src-tab-acts-natively t)
  (add-hook 'org-mode-hook #'org-indent-mode)
  (add-hook 'org-mode-hook (lambda () (electric-indent-local-mode -1))))

(th/leader
  "ol" '(org-open-at-point :wk "open org link"))

(th/local
  "l" '(:ignore t :wk "org link")
  "li" '(org-insert-link :wk "insert org link")
  "lo" '(org-open-at-point :wk "open org link")
  "le" '(org-edit-special :wk "open org link")
  "lt" '(org-toggle-link-display :wk "toggle link display"))

(use-package olivetti
  :after org
  :init (setq olivetti-min-body-width 50
	      olivetti-body-width 80
	      olivetti-style 'fancy
	      olivetti-margin-width 12)
  :config
  (set-face-attribute 'olivetti-fringe nil :background "#313244")
  :hook
  (olivetti-mode-on . (lambda () (olivetti-set-width 85)))
  (org-mode . olivetti-mode))

(use-package org-roam
  :after org
  :init
  (setq org-roam-directory (file-truename "~/Documents/org-roam"))
  (unless (file-directory-p org-roam-directory)
    (make-directory org-roam-directory))
  :config
  (org-roam-db-autosync-mode)
  (org-roam-setup))

(th/leader
  "r" '(:ignore t :wk "roam")
  "rb" '(org-roam-buffer-toggle :wk "buffer")
  "rf" '(org-roam-node-find :wk "find")
  "rc" '(org-roam-node-find :wk "find")
  "ri" '(org-roam-node-insert :wk "find"))

(use-package org-tempo
  :after org
  :config
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp")))

(use-package org-bullets
  :diminish org-bullets-mode
  :hook (org-mode . org-bullets-mode))

(use-package hl-todo
  :diminish hl-todo-mode
  :diminish global-hl-todo-mode
  :config
  (global-hl-todo-mode 1))

;; IDE
;; (use-package flymake
;;   :hook (prog-mode . flymake-mode))


(use-package eglot
  :hook (prog-mode . eglot-ensure)
  :init
  (fset #'jsonrpc--log-event #'ignore)
  (setq eglot-autoshutdown t))

(th/leader
  "c" '(:ignore t :wk "code")
  "ca" '((lambda () (interactive)
	   (eglot-code-actions 1 (point-max) nil t))
	 :wk "code actions"))

(use-package eglot-booster
  :after eglot
  :config
  (eglot-booster-mode))

(use-package flycheck
  :config (global-flycheck-mode))

(use-package flycheck-eglot
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))

;; (require 'indent-bars)
;; (add-hook 'prog-mode-hook #'indent-bars-mode)

(use-package company
  :hook
  (after-init . global-company-mode)
  (prog-mode . company-mode)
  :diminish company-mode
  :init
  (setq company-idle-delay 0.1)
  :config
  (general-define-key :states '(insert)
		      "C-k" nil)
  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "<tab>") nil)

  (make-mode-keymap company-mode-map '(
				       ("C-j" . company-select-next)
				       ("C-k" . company-select-previous)
				       ("C-<return>" . company-complete-selection)
				       ("C-RET" . company-complete-selection)
				       ("S-<return>" . company-complete-selection)
				       ("S-RET" . company-complete-selection)
				       ("<escape>" . company-abort)
				       ("ESC" . company-abort))))


(use-package company-box
  :after company
  :init (add-hook 'company-mode #'company-box-mode))

(use-package rustic
  :after eglot
  :mode "\\.rs\\'"
  :hook (rustic-mode . eglot-ensure)
  :init
  (setq rustic-format-trigger 'on-save
	rustic-lsp-client 'eglot
	rustic-format-on-save-method 'rustic-cargo-fmt
	rustic-format-display-method 'ignore
	rustic-use-rust-save-some-buffers t
	compilation-ask-about-save nil))

(use-package nix-mode
  :after eglot
  :mode "\\.nix\\'"
  :hook (nix-mode . eglot-ensure)
  :init (add-to-list 'eglot-server-programs '(nix-mode . ("nixd"))))

(use-package haskell-mode
  :after eglot
  :mode "\\.hs\\'"
  :hook (haskell-mode . eglot-ensure))

(use-package lua-mode
  :after eglot
  :mode "\\.lua\\'"
  :hook (lua-mode . eglot-ensure))

(use-package gdscript-mode
  :after eglot
  :mode "\\.gdscript\\'"
  :hook (gdscript-mode . eglot-ensure))

(use-package fennel-mode
  :after eglot
  :mode "\\.fnl\\'"
  :hook (fennel-mode . eglot-ensure)
  :init (add-to-list 'eglot-server-programs '(fennel-mode . ("fennel-ls"))))

(use-package csharp-mode
  :after eglot
  :mode "\\.cs\\'"
  :hook
  (csharp-mode . eglot-ensure)
  (csharp-mode . csharp-ts-mode))

(add-hook 'emacs-lisp-mode-hook #'company-mode)

(use-package rainbow-delimiters
  :diminish rainbow-delimiters-mode
  :hook
  (prog-mode . rainbow-delimiters-mode)
  (org-mode . rainbow-delimiters-mode))

(use-package projectile
  :diminish projectile-mode
  :init
  (setq projectile-switch-project-action #'projectile-dired)
  :config
  (projectile-mode))

(th/leader
 "P" '(:keymap projectile-command-map :package projectile)
 "p" '(:ignore t :package projectile :wk "project")
 "pp" '(projectile-switch-project :wk "switch project")
 "ps" '((lambda () (interactive) (consult-ripgrep (projectile-project-root))) :wk "search project")
 "p." '(projectile-find-file :wk "find project file")
 "po" '(:ignore t :wk "open")
 "pot" '(projectile-run-vterm :wk "vterm")
 "pog" '(projectile-vc :wk "project version control (git)")
 "pb" '(projectile-switch-to-buffer :wk "switch buffer in project"))

(use-package ibuffer-projectile
  :config
  (add-hook 'ibuffer-hook (lambda ()
			  (ibuffer-projectile-set-filter-groups)
			  (unless (eq ibuffer-sorting-mode 'alphabetic
				      (ibuffer-do-sort-by-alphabetic))))))

(use-package magit
  :init
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1
	magit-auto-revert-mode nil))

(use-package vterm)


(use-package vertico
  :init
  (setq vertico-resize t)
  :config
  (make-mode-keymap vertico-map '(("C-j" . vertico-next)
				  ("C-k" . vertico-previous)
				  ("C-u" . vertico-quick-exit)
				  ("<backspace>" . vertico-directory-delete-char)
				  ("DEL" . vertico-directory-delete-char)))
  (vertico-mode))

(use-package consult)

(th/leader
  "sg" '((lambda () (interactive) (consult-ripgrep (expand-file-name ""))) :wk "M-x")
  "sf" '(consult-fd :wk "find")
  "bs" '(consult-buffer :wk "switch"))

(use-package orderless
  :after (vertico consult)
  :init
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :config
  (marginalia-mode))

(defun crm-indicator (args)
  (cons (format "[CRM%s] %s"
	  (replace-regexp-in-string
	   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
	   crm-separator)
	  (car args))
  (cdr args)))
(advice-add #'completing-read-multiple :filter-args #'crm-indicator)

(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible-mode t face minibuffer-prompt)
      enable-recursive-minibuffers t)

(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(savehist-mode 1)

(use-package popper
  :init
  (setq popper-reference-buffers
	'("^\\*vterm.*\\*$" vterm-mode
	  "\\*eldoc\\*" vterm-mode
	  ("\\*elpaca-log\\*" . hide)
	  ("\\*rustic.*\\*" . hide)
	  ("\\*rustfmt\\*" . hide)
	  ("\\*rust-analyzer.*\\*" . hide)
	  ("\\*EGLOT.*\\*" . hide)
	  ("\\*scratch\\*" . hide)
	  ("\\*Warnings\\*" . hide)
	  (compilation-mode . hide))
	popper-group-function #'popper-group-by-projectile
	popper-window-height 20)
  :config
  (popper-mode 1)
  (popper-echo-mode 1))

(th/leader
  "op" '(:ignore t :wk "popper")
  "opt" '(popper-toggle :wk "popper toggle")
  "opm" '(popper-toggle-type :wk "popper toggle type")
  "opc" '(popper-cycle :wk "popper cycle"))

(use-package simple-mpc)

(th/leader
  "m" '(:ignore t :wk "media")
  "mm" '(simple-mpc :wk "open simple-mpc")
  "ms" '(simple-mpc-query :wk "search")
  "mp" '(simple-mpc-toggle :wk "play/pause")
  "mC" '(simple-mpc-clear-current-playlist :wk "clear")
  "mP" '(simple-mpc-view-current-playlist :wk "playlist")
  "ma" '(simple-mpc-load-playlist :wk "load playlist")
  "mh" '(simple-mpc-prev :wk "prev")
  "ml" '(simple-mpc-next :wk "next"))

;; Fix tramp for nixos systems
(use-package tramp-sh
  :config
  (setq tramp-remote-path
	(append tramp-remote-path
 		'(tramp-own-remote-path))))

(use-package separedit)

(defvar th/first-server-frame-created nil)
(defun th--unless-first-server-frame-created (func)
  (unless th/first-server-frame-created
    (funcall func)
    (setq th/first-server-frame-created t)))

(use-package catppuccin-theme
  :init
  (setq catppuccin-flavor 'mocha)
  :hook
  (after-init . catppuccin-reload)
  (server-after-make-frame . (lambda () (when (display-graphic-p)
				    (th--unless-first-server-frame-created 'catppuccin-reload)))))

(use-package solaire-mode
  :hook
  (after-init . (lambda ()
		  (when (display-graphic-p) (solaire-global-mode +1))))
  (server-after-make-frame . (lambda ()
			       (when (display-graphic-p) (solaire-global-mode +1)))))

(use-package all-the-icons)

(defun th--ati-dired ()
  (when (display-graphic-p)
    (th--unless-first-server-frame-created
     (lambda () (add-hook 'dired-mode-hook #'all-the-icons-dired-mode)))))
(use-package all-the-icons-dired
  :after all-the-icons
  :diminish all-the-icons-dired-mode
  :hook
  (after-init . (lambda ()
		  (when (display-graphic-p) (add-hook 'dired-mode-hook #'all-the-icons-dired-mode))))
  (server-after-make-frame . th--ati-dired))

(defun th--ati-ibuffer ()
  (when (display-graphic-p)
    (th--unless-first-server-frame-created
     (lambda () (add-hook 'ibuffer-mode-hook #'all-the-icons-ibuffer-mode)))))
(use-package all-the-icons-ibuffer
  :after all-the-icons
  :diminish all-the-icons-ibuffer-mode
  :hook
  (after-init . (lambda ()
		  (when (display-graphic-p) (add-hook 'ibuffer-mode-hook #'all-the-icons-ibuffer-mode))))
  (server-after-make-frame . th--ati-ibuffer))
