;; -*- lexical-binding: t -*-
(setq emacs-data-directory (let (
				 (local-share 
				  (or 
				   (getenv "XDG_DATA_HOME") 
				   (concat (getenv "HOME") "/.local/share"))))
			     (expand-file-name (concat local-share "/emacs/"))))

(unless (file-directory-p emacs-data-directory)
  (make-directory emacs-data-directory))

(setq extrapkgs-dir (expand-file-name "extrapkgs/" emacs-data-directory))
(unless (file-directory-p extrapkgs-dir)
  (make-directory extrapkgs-dir))

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
      c-basic-offset 4
      tab-width 4
      gc-cons-threshold (* 8 1024 1024)

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
	  simple-mpc-mode-hook
	  eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(global-set-key (kbd "s-`") #'(lambda () (interactive) (insert "`")))
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "ESC") 'keyboard-escape-quit)

(set-face-attribute 'default nil
		    :font "Monospace-9"
		    :weight 'medium)

(set-face-attribute 'variable-pitch nil
		    :font "Sans-Serif-9"
		    :weight 'medium)

(set-face-attribute 'fixed-pitch nil
		    :font "Monospace-9"
		    :weight 'medium)

(set-face-attribute 'font-lock-comment-face nil
		    :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
		    :slant 'italic)

(add-to-list 'default-frame-alist '(font . "Monospace-9"))

(setq-default line-spacing 0.12)

(setq scroll-conservatively 10)
(setq scroll-margin 7)
(setq pixel-scroll-precision-large-scroll-height 40.0)
(setq pixel-scroll-precision-use-momentum t)

(require 'dired)
(define-key dired-mode-map (kbd "SPC") nil)
(define-key dired-mode-map (kbd "<backspace>") #'dired-up-directory)

;; Functions
(defun make-mode-keymap (map outer)
  (mapc (lambda (inner)
	  (define-key map (kbd (car inner)) (cdr inner)))
	outer))

(defun comment-or-uncomment-region-or-line ()
  "If a region is selected, either uncomment or comment it, if not, uncomment or comment the current line"
  (interactive)
  (let (beg end)
    (if (region-active-p)
	(setq beg (region-beginning) end (region-end))
      ;; else
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(defun eval-region-and-go-to-normal-mode ()
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
  "Add `current-window-configuration` to saved window configurations."
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
  (interactive)
  (setq saved-window-configurations
	(delq (assoc
	       (completing-read "Delete a window configuration: "
				saved-window-configurations)
	       saved-window-configurations)
	      saved-window-configurations)))


;; (load (expand-file-name "eglot-booster.el" extrapkgs-dir))
;; (require 'eglot-booster)
;; (load (expand-file-name "indent-bars.el" extrapkgs-dir))

;; Evil
;; (require 'undo-fu)
(require 'undo-tree)
(global-undo-tree-mode)
(setq th/undo-tree-dir (expand-file-name "undo-tree/" emacs-data-directory))
(unless (file-directory-p th/undo-tree-dir)
  (make-directory th/undo-tree-dir))
(defadvice undo-tree-make-history-save-file-name
  (after undo-tree activate)
  (setq ad-return-value (concat th/undo-tree-dir ad-return-value)))
(require 'evil)
(setq evil-want-integration t)
(setq evil-want-keybinding nil)
(setq evil-vsplit-window-right t)
(setq evil-split-window-below t)
(setq evil-undo-system 'undo-tree)
(evil-set-undo-system evil-undo-system)
(evil-mode)

;; Which-key
(require 'which-key)
(which-key-setup-side-window-bottom)
(which-key-mode)

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
 "ot" '(eshell :wk "eshell")
 "ov" '(vterm :wk "vterm")

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

;; Evil collection
(require 'evil-collection)
(setq evil-collection-mode-list '(dashboard dired ibuffer simple-mpc magit))
(evil-collection-init)

;; Evil better visual line
(require 'evil-better-visual-line)
(evil-better-visual-line-on)

;; Smartparens
(require 'smartparens)
(require 'evil-smartparens)
(add-hook 'emacs-lisp-mode 'smartparens-mode)
(add-hook 'rustic-mode-hook 'smartparens-mode)
(smartparens-global-mode)

;; Org
(setq org-src-preserve-indentation t
      org-src-tab-acts-natively t)

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))

(add-hook 'org-mode-hook #'org-indent-mode)
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

(require 'treesit-auto)
(setq treesit-auto-install 'prompt)
(treesit-auto-add-to-auto-mode-alist 'all)
(global-treesit-auto-mode)
(add-to-list 'treesit-extra-load-path (expand-file-name "tree-sitter/" user-emacs-directory))

(require 'rustic)
(add-hook 'rustic-mode-hook #'lsp-deferred)
(setq rustic-format-trigger 'on-save
      rustic-lsp-client 'eglot
      rustic-format-on-save-method 'rustic-cargo-fmt
      rustic-format-display-method 'ignore
      rustic-use-rust-save-some-buffers t
      compilation-ask-about-save nil)

(require 'nix-mode)
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))
(add-to-list 'eglot-server-programs '(nix-mode . ("nil")))
(add-hook 'nix-mode-hook #'eglot-ensure)

;; elisp compltion
(add-hook 'emacs-lisp-mode-hook #'company-mode)

(require 'lua-mode)
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))
(add-hook 'lua-mode #'lsp-deferred)

(add-hook 'csharp-mode-hook #'lsp-deferred)
(add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-mode))

(require 'gdscript-mode)
(add-to-list 'auto-mode-alist '("\\.gdscript\\'" . gdscript-mode))
(add-hook 'gdscript-mode-hook #'lsp-deferred)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'org-mode-hook #'rainbow-delimiters-mode)

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
 "pot" '(projectile-run-eshell :wk "eshell")
 "pov" '(projectile-run-vterm :wk "vterm")
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
(require 'eshell-vterm)
(eshell-vterm-mode)
(require 'fish-completion)
(global-fish-completion-mode)

(setq th/eshell-aliases
      '((v . eshell-exec-visual)))

(mapc (lambda (alias)
	(defalias (car alias) (cdr alias)))
      th/eshell-aliases)

(require 'vertico)
;; (general-define-key :keymap vertico-map
;; 		    "C-j" #'vertico-next
;; 		    "C-k" #'vertico-previous
;; 		    "C-u" #'vertico-quick-exit
;; 		    "<backspace>" #'vertico-directory-delete-char)
(make-mode-keymap vertico-map '(
				("C-j" . vertico-next)
				("C-k" . vertico-previous)
				("C-u" . vertico-quick-exit)
				("<backspace>" . vertico-directory-delete-char)))

(setq vertico-resize t)
(vertico-mode)


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

(require 'consult)
(th/leader
  "sg" '((lambda () (interactive) (consult-ripgrep (expand-file-name ""))) :wk "M-x")
  "sf" '(consult-fd :wk "M-x")
  "bs" '(consult-buffer :wk "M-x"))

(savehist-mode 1)

(require 'orderless)
(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion))))

(require 'marginalia)
;; (general-define-key :keymap message-minibuffer-local-map
;; 		    "C-m" #'marginalia-cycle)
(marginalia-mode)

(require 'popper)
(setq popper-reference-buffers
      '("^\\*eshell.*\\*$" eshell-mode
	"^\\*vterm.*\\*$" vterm-mode
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
(popper-echo-mode 1)


(require 'simple-mpc)
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

(require 'diminish)
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
(diminish 'undo-tree-mode)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "ESC") 'keyboard-escape-quit)

;; Theming
(require 'catppuccin-theme)
;; (catppuccin-reload)
;; (load-theme 'catppuccin t)
;; (add-hook 'server-after-make-frame-hook #'catppuccin-reload)
(add-hook 'server-after-make-frame-hook (lambda () (catppuccin-reload) (catppuccin-load-flavor 'mocha)))
;; (add-hook 'server-after-make-frame-hook (lambda () (catppuccin-load-flavor 'mocha)))
;; (add-hook 'emacs-startup-hook (lambda () (catppuccin-load-flavor 'mocha)))

;; (add-hook 'after-init-hook (lambda () (catppuccin-reload)))

(require 'solaire-mode)
(solaire-global-mode 1)
