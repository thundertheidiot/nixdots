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

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "SPC") nil)
  (define-key dired-mode-map (kbd "<backspace>") #'dired-up-directory)
  (define-key dired-mode-map (kbd "DEL") #'dired-up-directory))

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

(require 'subr-x)

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


(with-eval-after-load 'undo-tree
  (global-undo-tree-mode)
  (setq th/undo-tree-dir (expand-file-name "undo-tree/" emacs-data-directory))
  (unless (file-directory-p th/undo-tree-dir)
    (make-directory th/undo-tree-dir))
  (defadvice undo-tree-make-history-save-file-name
      (after undo-tree activate)
    (setq ad-return-value (concat th/undo-tree-dir ad-return-value))))


(with-eval-after-load 'evil
  (setq
   evil-want-integration t
   evil-want-keybinding nil
   evil-vsplit-window-right t
   evil-split-window-below t
   evil-undo-system 'undo-tree)
  (evil-set-undo-system evil-undo-system)
  (evil-mode))

(with-eval-after-load 'evil-collection
  (evil-collection-init '(dashboard
			  woman
			  pdf
			  dired
			  wdired
			  image
			  eglot
			  ibuffer
			  simple-mpc
			  magit
			  vterm)))

(with-eval-after-load 'which-key
  (which-key-setup-side-window-bottom)
  (which-key-mode))

(with-eval-after-load 'evil-better-visual-line
  (evil-better-visual-line-on))

(with-eval-after-load 'smartparens)

(require 'undo-tree)
(require 'evil)
(require 'evil-collection)
(require 'evil-better-visual-line)
(require 'which-key)

;; General
(require 'general)
(general-evil-setup)
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

;; Smartparens
(with-eval-after-load 'smartparens
  (smartparens-global-mode))

(require 'smartparens)

;; Org
(require 'org)
(setq org-src-preserve-indentation t
      org-src-tab-acts-natively t)

(require 'org-tempo)
`(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))

(add-hook 'org-mode-hook #'org-indent-mode)
(add-hook 'org-mode-hook (lambda () (electric-indent-local-mode -1)))
(th/local
 "l" '(:ignore t :wk "org link")
 "li" '(org-insert-link :wk "insert org link")
 "lo" '(org-open-at-point :wk "open org link")
 "le" '(org-edit-special :wk "open org link")
 "lt" '(org-toggle-link-display :wk "toggle link display"))

(require 'org-bullets)
(add-hook 'org-mode-hook #'org-bullets-mode)

;; hl-todo
(require 'hl-todo)
(global-hl-todo-mode 1)

;; IDE
(require 'flymake)
(add-hook 'prog-mode-hook #'flymake-mode)

(require 'eglot)
(fset #'jsonrpc--log-event #'ignore)
(add-hook 'prog-mode-hook #'eglot-ensure)
(setq eglot-autoshutdown t)

(th/leader
  "c" '(:ignore t :wk "code")
  "ca" '((lambda () (interactive)
	   (eglot-code-actions 1 (point-max) nil t))
	 :wk "code actions"))

(require 'eglot-booster)
(eglot-booster-mode)

;; (require 'indent-bars)
;; (add-hook 'prog-mode-hook #'indent-bars-mode)


(require 'company)
(add-hook 'after-init-hook #'global-company-mode)
(add-hook 'prog-mode-hook  #'company-mode)
(setq company-idle-delay 0.1)
(general-define-key :states '(insert)
	      "C-k" nil)
(define-key company-active-map (kbd "<return>") nil)
(define-key company-active-map (kbd "RET") nil)
(define-key company-active-map (kbd "<tab>") nil)

;; (general-define-key :keymap company-mode-map
;; 	    "C-j" #'company-select-next
;; 	    "C-k" #'company-select-previous
;; 	    "C-<return>" #'company-complete-selection
;; 	    "C-RET" #'company-complete-selection
;; 	    "S-<return>" #'company-complete-selection
;; 	    "S-RET" #'company-complete-selection
;; 	    "<escape>" #'company-abort)

(make-mode-keymap company-mode-map '(
		    ("C-j" . company-select-next)
		    ("C-k" . company-select-previous)
		    ("C-<return>" . company-complete-selection)
		    ("C-RET" . company-complete-selection)
		    ("S-<return>" . company-complete-selection)
		    ("S-RET" . company-complete-selection)
		    ("<escape>" . company-abort)
		    ("ESC" . company-abort)))

(require 'company-box)
(add-hook 'company-mode #'company-box-mode)

;; (require 'treesit-auto)
;; (setq treesit-auto-install 'prompt)
;; (treesit-auto-add-to-auto-mode-alist 'all)
;; (global-treesit-auto-mode)
;; (add-to-list 'treesit-extra-load-path (expand-file-name "tree-sitter/" user-emacs-directory))

(with-eval-after-load 'rustic
  (add-hook 'rustic-mode-hook #'eglot-ensure)
  (setq rustic-format-trigger 'on-save
	rustic-lsp-client 'eglot
	rustic-format-on-save-method 'rustic-cargo-fmt
	rustic-format-display-method 'ignore
	rustic-use-rust-save-some-buffers t
	compilation-ask-about-save nil))

(with-eval-after-load 'nix-mode
  (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))
  (add-to-list 'eglot-server-programs '(nix-mode . ("nixd")))
  (add-hook 'nix-mode-hook #'eglot-ensure))

(with-eval-after-load 'haskell-mode
  (add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode))
  (add-hook 'haskell-mode-hook #'eglot-ensure))

(with-eval-after-load 'lua-mode
  (add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))
  (add-hook 'lua-mode #'eglot-ensure))

(with-eval-after-load 'gscript-mode
  (add-to-list 'auto-mode-alist '("\\.gdscript\\'" . gdscript-mode))
  (add-hook 'gdscript-mode-hook #'eglot-ensure))

(with-eval-after-load 'fennel-mode
  (add-to-list 'auto-mode-alist '("\\.fnl\\'" . fennel-mode))
  (add-to-list 'eglot-server-programs '(fennel-mode . ("fennel-ls"))))

(require 'rustic)
(require 'nix-mode)
(require 'haskell-mode)
(require 'lua-mode)
(require 'gdscript-mode)
(require 'fennel-mode)

(add-hook 'emacs-lisp-mode-hook #'company-mode)

(add-hook 'csharp-mode-hook #'eglot-ensure)
(add-hook 'csharp-mode-hook #'csharp-ts-mode)
(add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-mode))


(with-eval-after-load 'rainbow-delimiters
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'org-mode-hook #'rainbow-delimiters-mode))

(require 'rainbow-delimiters)

(require 'projectile)
(setq projectile-switch-project-action #'projectile-dired)
(projectile-mode)
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

(require 'ibuffer-projectile)
(add-hook 'ibuffer-hook (lambda ()
			  (ibuffer-projectile-set-filter-groups)
			  (unless (eq ibuffer-sorting-mode 'alphabetic
				      (ibuffer-do-sort-by-alphabetic)))))


(require 'magit)
(setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

(require 'vterm)


(with-eval-after-load 'vertico
  (make-mode-keymap vertico-map '(("C-j" . vertico-next)
				  ("C-k" . vertico-previous)
				  ("C-u" . vertico-quick-exit)
				  ("<backspace>" . vertico-directory-delete-char)
				  ("DEL" . vertico-directory-delete-char)))
  (setq vertico-resize t)
  (vertico-mode))

(with-eval-after-load 'consult
  (th/leader
    "sg" '((lambda () (interactive) (consult-ripgrep (expand-file-name ""))) :wk "M-x")
    "sf" '(consult-fd :wk "find")
    "bs" '(consult-buffer :wk "switch")))

(with-eval-after-load 'orderless
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))

(with-eval-after-load
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

(require 'vertico)
(require 'consult)
(require 'orderless)
(require 'marginalia)

(with-eval-after-load 'popper
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
	popper-group-function #'popper-group-by-projectile)
  (th/leader
    "op" '(:ignore t :wk "popper")
    "opt" '(popper-toggle :wk "popper toggle")
    "opm" '(popper-toggle-type :wk "popper toggle type")
    "opc" '(popper-cycle :wk "popper cycle"))

  (setq popper-window-height 20)

  (popper-mode 1)
  (popper-echo-mode 1))

(with-eval-after-load 'simple-mpc
  (th/leader
    "m" '(:ignore t :wk "media")
    "mm" '(simple-mpc :wk "open simple-mpc")
    "ms" '(simple-mpc-query :wk "search")
    "mp" '(simple-mpc-toggle :wk "play/pause")
    "mC" '(simple-mpc-clear-current-playlist :wk "clear")
    "mP" '(simple-mpc-view-current-playlist :wk "playlist")
    "ma" '(simple-mpc-load-playlist :wk "load playlist")
    "mh" '(simple-mpc-prev :wk "prev")
    "ml" '(simple-mpc-next :wk "next")))

(require 'popper)
(require 'simple-mpc)

;; Fix tramp for nixos systems
(with-eval-after-load 'tramp-sh
  (setq tramp-remote-path
	(append tramp-remote-path
 		'(tramp-own-remote-path))))

(require 'separedit)

(with-eval-after-load 'diminish
  (diminish 'which-key-mode)
  (diminish 'font-lock-mode)
  (diminish 'visual-line-mode)
  (diminish 'evil-collection-unimpaired-mode)
  (diminish 'smartparens-mode)
  (diminish 'evil-smartparens-mode)
  (diminish 'org-bullets-mode)
  (diminish 'hl-todo-mode)
  (diminish 'global-hl-todo-mode)
  (diminish 'company-mode)
  (diminish 'rainbow-delimiters-mode)
  (diminish 'projectile-mode)
  (diminish 'auto-revert-mode)
  (diminish 'eldoc-mode)
  (diminish 'undo-tree-mode))

(require 'diminish)

(defun th--init-frame ()
  "Initialize a frame."
  (when (display-graphic-p)
    (with-eval-after-load 'catppuccin-theme
      (setq catppuccin-flavor 'mocha)
      (catppuccin-reload))
    (with-eval-after-load 'solaire-mode
      (solaire-global-mode +1))
    (with-eval-after-load 'all-the-icons
      (with-eval-after-load 'all-the-icons-dired
	(add-hook 'dired-mode-hook #'all-the-icons-dired-mode))
      (with-eval-after-load 'all-the-icons-ibuffer
	(add-hook 'ibuffer-mode-hook #'all-the-icons-ibuffer-mode))))
  (unless (display-graphic-p)
    (xterm-mouse-mode 1))
  (remove-hook 'server-after-make-frame-hook #'th--init-frame))


;; (add-hook 'after-make-frame-functions #'th--init-frame) ;; breaks emacs(client)
(add-hook 'after-init-hook #'th--init-frame)
(add-hook 'server-after-make-frame-hook #'th--init-frame)

(require 'catppuccin-theme)
(require 'solaire-mode)
(require 'all-the-icons)
(require 'all-the-icons-dired)
(require 'all-the-icons-ibuffer)
