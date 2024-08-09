(eval-when-compile
  (require 'use-package))
(require 'diminish)

;; Cleanup
(setq backup-directory-alist `(("." . ,(expand-file-name "backup-files" user-emacs-directory))))


(let ((auto-save-dir (expand-file-name "auto-saves/" user-emacs-directory)))
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

      recentf-save-file (expand-file-name "recentf" user-emacs-directory)

      backward-delete-char-untabify-method nil)

(electric-indent-mode)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode 1)
(global-visual-line-mode t)

(recentf-mode)

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

(defun turn-off-line-numbers ()
  "Dumb function like this so i can hook it without a lambda."
  (display-line-numbers-mode 0))

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
  (defvar th/undo-tree-dir (expand-file-name "undo-tree/" user-emacs-directory))
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
			  org
			  dired
			  elfeed
			  wdired
			  emoji
			  image
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

  "d" '((lambda () (interactive)
	  (when default-directory
	    (dired default-directory))) :wk "dired")

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
	org-src-tab-acts-natively t
	org-startup-with-inline-images t)
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
  "rf" '(org-roam-node-find :wk "find node")
  "rI" '(org-id-get-create :wk "create id")
  "ri" '(org-roam-node-insert :wk "insert node"))

(require 'org-download) ;; this makes zero sense but yeah
(use-package org-download
  :after org
  :hook (dired-mode . org-download-enable)
  :init (setq org-download-screenshot-method "grim -g \"$(slurp)\" -t png %s"))

(th/leader
  "rs" '(org-download-screenshot :wk "screenshot")
  "rc" '(org-download-clipboard :wk "image from clipboard"))

(use-package org-tempo
  :after org
  :config
  (setq org-structure-template-alist (cons '("el" . "src emacs-lisp") org-structure-template-alist))
  (setq org-structure-template-alist (cons '("fnl" . "src fennel") org-structure-template-alist)))

(use-package org-bullets
  :diminish org-bullets-mode
  :hook (org-mode . org-bullets-mode))

(use-package hl-todo
  :diminish hl-todo-mode
  :diminish global-hl-todo-mode
  :config
  (global-hl-todo-mode 1))

(use-package lsp-mode
  :demand t
  :diminish lsp-lens-mode
  :init (setq lsp-auto-guess-root t
	      lsp-enable-symbol-highlighting t)
  :hook (lsp-mode . (lambda ()
		      (setq lsp-headerline-breadcrumb-segments '(project file))
		      (lsp-headerline-breadcrumb-mode))))

(use-package lsp-ui
  :after lsp-mode
  :init (setq lsp-ui-doc-enable nil
	      lsp-ui-show-with-cursor nil
	      lsp-ui-show-with-mouse nil
	      )
  :hook (lsp-mode . lsp-ui-mode))

(th/leader
  "c" '(:ignore t :wk "code")
  "cr" '(lsp-rename :wk "lsp rename")
  "ca" '(lsp-execute-code-action :wk "code action")
  "ch" '(lsp-describe-thing-at-point :wk "help")
  "S" '(lsp-workspace-shutdown :wk "lsp shutdown"))

(use-package flycheck
  :config (global-flycheck-mode))

(th/leader
  "cn" '(flycheck-next-error :wk "next error")
  "cN" '(flycheck-previous-error :wk "previous error"))

(use-package apheleia
  :config
  (setf (alist-get 'nixfmt apheleia-formatters)
	'("alejandra"))
  (apheleia-global-mode +1))

(use-package magit
  :init
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1
	magit-auto-revert-mode nil))

(th/leader
  "g" '(:ignore t :wk "git")
  "gg" '(magit-status :wk "open magit")
  "gd" '(:ignore t :wk "diff")
  "gdu" '(magit-diff-unstaged :wk "diff unstaged")
  "gds" '(magit-diff-staged :wk "diff staged")
  "gc" '(magit-commit-create :wk "commit"))

(use-package magit-todos
  :after magit
  :config (magit-todos-mode 1))

(use-package git-gutter-fringe+
  :diminish git-gutter+-mode
  :hook
  (prog-mode . git-gutter+-mode)
  (git-gutter+-mode . (lambda ()
			(set-face-background 'git-gutter+-added "green"))))

(th/leader
  "gs" '(git-gutter+-show-hunk :wk "stage hunks")
  "ga" '(git-gutter+-stage-hunks :wk "stage hunks")
  "gn" '(git-gutter+-next-hunk :wk "next hunk")
  "gN" '(git-gutter+-previous-hunk :wk "previous hunk"))

(use-package git-timemachine
  :config
  (general-define-key
   :states 'normal
   :keymaps 'git-timemachine-mode-map
   "<" 'git-timemachine-show-previous-revision
   "H" 'git-timemachine-show-previous-revision
   ">" 'git-timemachine-show-next-revision
   "L" 'git-timemachine-show-next-revision
   "f" (lambda () (git-timemachine-show-nth-revision 1))
   "g" 'git-timemachine-show-nth-revision
   "c" 'git-timemachine-show-current-revision))

(th/leader
  "gt" '(git-timemachine-toggle :wk "git timemachine"))

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

  (make-mode-keymap company-mode-map '(("C-j" . company-select-next)
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
  :after lsp-mode
  :diminish rustic-mode
  :mode ("\\.rs\\'" . rustic-mode)
  :hook (rustic-mode . lsp)
  :init
  (setq rustic-lsp-client 'lsp-mode
	rustic-use-rust-save-some-buffers t
	compilation-ask-about-save nil))

(use-package nix-mode
  :after lsp-mode
  :mode "\\.nix\\'"
  :hook (nix-mode . lsp))

(use-package haskell-mode
  :after lsp-mode
  :mode "\\.hs\\'"
  :hook (haskell-mode . lsp))

(use-package lua-mode
  :after lsp-mode
  :mode "\\.lua\\'"
  :hook (lua-mode . lsp))

(use-package gdscript-mode
  :after lsp-mode
  :mode "\\.gdscript\\'"
  :hook (gdscript-mode . lsp))

(use-package fennel-mode
  :after lsp-mode
  :mode "\\.fnl\\'"
  :hook (fennel-mode . lsp)
  :init
  (add-to-list 'lsp-language-id-configuration '(fennel-mode . "fennel"))
  (lsp-register-client (make-lsp-client
			:new-connection (lsp-stdio-connection "fennel-ls")
			:activation-fn (lsp-activate-on "fennel")
			:server-id 'fennel-ls)))

(use-package janet-mode
  :after lsp-mode
  :mode "\\.janet\\'")

(use-package csharp-mode
  :after lsp-mode
  :mode "\\.cs\\'"
  :hook
  (csharp-mode . lsp)
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

(use-package vterm
  :hook (vterm-mode . turn-off-line-numbers))


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

(use-package simple-mpc
  :hook (simple-mpc-mode . turn-off-line-numbers))

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
