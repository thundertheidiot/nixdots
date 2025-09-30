					; -*- lexical-binding: t -*-
(require 'use-package)
(require 'general)

;; wrong place lo 
;; (add-to-list 'default-frame-alist '(alpha-background . 0.88))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package diminish
  :demand t
  :config
  (diminish 'font-lock-mode)
  (diminish 'visual-line-mode)
  (diminish 'auto-revert-mode)
  (diminish 'eldoc-mode))

(use-package pcre2el)
(use-package dash)
(use-package plz)

(setq backup-directory-alist `(("." . ,(expand-file-name "backup-files" user-emacs-directory))))

(let ((auto-save-dir (expand-file-name "auto-saves/" user-emacs-directory)))
  (unless (file-directory-p auto-save-dir)
    (make-directory auto-save-dir))
  (setq auto-save-list-file-prefix auto-save-dir
	auto-save-file-name-transforms `((".*" ,auto-save-dir t))))

(setq create-lockfiles nil)

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
      read-process-output-max (* 1024 1024)

      ring-bell-function 'ignore ;; i hate that stupid bell

      inhibit-startup-screen t
      inhibit-splash-screen t

      display-line-numbers-type 'relative

      backward-delete-char-untabify-method nil)

(electric-indent-mode)
(electric-pair-mode)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode 1)
(global-prettify-symbols-mode 1)
(global-visual-line-mode t)
(savehist-mode 1)

(defun advice!-keyboard-escape-quit-adv (fun)
  "Around advice for `keyboard-escape-quit' FUN.
Preserve window configuration when pressing ESC."
  (let ((buffer-quit-function (or buffer-quit-function #'ignore)))
    (funcall fun)))
(advice-add #'keyboard-escape-quit :around #'advice!-keyboard-escape-quit-adv)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "ESC") 'keyboard-escape-quit)

(recentf-mode)
(setq recentf-max-menu-items 1000
      recentf-max-saved-items 1000)
(run-at-time "5 min" 300 'recentf-save-list)

(global-set-key (kbd "s-`") #'(lambda () (interactive) (insert "`")))

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

(use-package subr-x :ensure nil)

(defvar saved-window-configurations '())

(defun wcs--format-window-list ()
  (let ((window-list-string-formatted) (value))
    (dolist (window (window-list) value)
      (setq window-list-string-formatted (concat
					  window-list-string-formatted
					  (format "%s, " (buffer-name (window-buffer window))))))
    (setq window-list-string-formatted (string-remove-suffix ", " window-list-string-formatted))
    window-list-string-formatted))

(defun save-window-configuration (&optional name)
  "Add the current window configuration to saved window configurations, if NAME is provided, give it a name."
  (interactive "P")
  (when (and name (not (stringp name)))
    (setq name (completing-read "Name wcfg: " '())))
  (add-to-list 'saved-window-configurations `(,(or name
						   (if (string= (projectile-project-name) "-")
						       (format "%s (%s)"
							       (shell-command-to-string "date \"+%a %R\"")
							       (wcs--format-window-list))
						     (format "%s: %s (%s)"
							     (projectile-project-name)
							     (shell-command-to-string "date \"+%a %R\"")
							     (wcs--format-window-list))))
					      . ,(window-state-get (frame-root-window) t))))

(defun new-window-configuration ()
  "Save the current window configuration close other buffers."
  (interactive)
  (save-window-configuration)
  (select-window (split-window))
  (delete-other-windows))

(defun load-window-configuration ()
  "Select a window configuration from the list and load it."
  (interactive)
  (let ((config (cdr
		 (assoc
		  (completing-read "Select window config: " saved-window-configurations)
		  saved-window-configurations))))
    (when config
      (window-state-put config (frame-root-window) t))))

(defun delete-window-configuration ()
  "Select a window configuration to delete."
  (interactive)
  (setq saved-window-configurations
	(delq (assoc
	       (completing-read "Delete a window configuration: "
				saved-window-configurations)
	       saved-window-configurations)
	      saved-window-configurations)))

(defun th/turn-off-line-numbers ()
  "Turn off line numbers 🤯"
  (display-line-numbers-mode 0))

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
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(defun eval-region-and-go-to-normal-mode ()
  "Evaluate elisp in the selected region and go back to normal mode."
  (interactive)
  (let (beg end)
    (if (region-active-p)
	(setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (eval-region beg end)
    (evil-normal-state)))

;; (setq split-width-threshold 160)
;; (setq split-height-threshold 80)
(defun th/intelligent-split (&optional force)
  (interactive)
  (let* ((width (window-total-width))
	 (height (window-total-height))
	 (window (cond ((and (< width split-width-threshold) (< height split-height-threshold) (not force)) (current-buffer))
		       ((> (+ 10 (* 2 height)) width) (split-window-below))
		       (t (split-window-right)))))
    (ignore-errors (balance-windows (window-parent)))
    window))



(use-package undo-tree
  :demand t
  :diminish undo-tree-mode
  :diminish global-undo-tree-mode
  :custom
  (undo-tree-auto-save-history t)
  (undo-tree-history-directory-alist `((,(rx (* nonl)) . ,(expand-file-name "undo-tree/" user-emacs-directory))))
  :config
  ;; redefine this so i can override the default undo binding
  (defun undo-tree-overridden-undo-bindings-p () 
    nil)
  (global-undo-tree-mode)
  (unless (file-directory-p (expand-file-name "undo-tree/" user-emacs-directory))
    (make-directory (expand-file-name "undo-tree/" user-emacs-directory))))

(use-package evil
  :after undo-tree
  :demand t
  :custom
  (evil-want-integration t)
  (evil-want-keybinding nil)
  (evil-vsplit-window-right t)
  (evil-split-window-below t)
  (evil-want-minibuffer t) ;; we're going extra evil
  (evil-undo-system 'undo-tree)
  :config
  (evil-set-undo-system evil-undo-system)
  (evil-mode))

(use-package evil-collection
  :demand t
  :after evil
  :diminish evil-collection-unimpaired-mode
  :config
  (evil-collection-init '(apropos
			  calc
			  compile
			  comint
			  dashboard
			  debug
			  ediff
			  emoji
			  eshell
			  woman
			  pdf
			  org
			  proced
			  dired
			  elfeed
			  wdired
			  image
			  ibuffer
			  simple-mpc
			  magit
			  forge
			  magit-todos
			  vdiff
			  sly
			  wgrep
			  yaml-mode
			  diff-hl
			  vterm)))

(use-package evil-better-visual-line
  :demand t
  :after evil
  :config
  (evil-better-visual-line-on))

;; (use-package evil-textobj-tree-sitter
;;   :after evil
;;   :config
;;   (define-key evil-outer-text-objects-map "f" (evil-treeobj-tree-sitter-get-textobj "function.outer"))
;;   (define-key evil-inner-text-objects-map "f" (evil-treeobj-tree-sitter-get-textobj "function.inner")))

(use-package which-key
  :demand t
  :diminish which-key-mode
  :config
  (which-key-setup-side-window-bottom)
  (which-key-mode))

(use-package general
  :demand t
  :config
  (general-evil-setup))

;; (general-create-definer th/leader
;;   :states '(normal insert visual emacs motion)
;;   :keymaps 'override
;;   :prefix "SPC"
;;   :global-prefix "C-SPC")

;; (general-create-definer th/local
;;   :states '(normal insert visual emacs motion)
;;   :keymaps 'override
;;   :prefix "SPC l"
;;   :global-prefix "C-SPC l")

(general-def :keymaps 'override
  "M-x" 'execute-extended-command)

(general-def :states '(normal visual motion) :keymaps 'override :prefix "SPC"
  "w" '(:ignore t :wk "window")
  "wh" '("move left" . windmove-left)
  "wj" '("move down" . windmove-down)
  "wk" '("move up" . windmove-up)
  "wl" '("move right" . windmove-right)
  "<left>" '("move left" . windmove-left)
  "<down>" '("move down" . windmove-down)
  "<up>" '("move up" . windmove-up)
  "<right>" '("move right" . windmove-right)
  "wq" '("close" . evil-quit)
  "ww" '("close" . evil-quit)
  "ws" '("horizontal split" . (lambda () (interactive) (select-window (th/intelligent-split t))))

  "wc" '(:ignore t :wk "window configurations")
  "wcl" '("load" . load-window-configuration)
  "wcs" '("save" . save-window-configuration)
  "wcn" '("new" . new-window-configuration)
  
  "H" '("increase window width" . (lambda () (interactive) (evil-window-increase-width 2)))
  "J" '("increase window height" . (lambda () (interactive) (evil-window-increase-height 2)))
  
  "l" '(:ignore t :wk "local (mode specific)")
  "s" '(:ignore t :wk "search")

  "d" '("dired" . (lambda () (interactive)
                    (when default-directory
                      (select-window (th/intelligent-split t))
                      (dired default-directory))))

  "D" '("dired in current window" . (lambda () (interactive)
				      (when default-directory
					(dired default-directory))))

  "o" '(:ignore t :wk "open")

  ":" '("M-x" . execute-extended-command)
  ";" '("M-x" . execute-extended-command)
  "." '("find file" . find-file)
  ">" '("find file from ~/" . (lambda () (interactive) (find-file (getenv "HOME"))))
  
  "h" '(:ignore t :wk "help")
  "hb" '("describe binding" . describe-bindings)
  "hf" '("describe function" . describe-function)
  "hv" '("describe variable" . describe-variable)
  "hF" '("describe face" . describe-face)
  "hk" '("describe key" . describe-key)
  "ha" '("describe" . apropos)
  
  "b" '(:ignore t :wk "buffer")
  "bi" '("ibuffer" . ibuffer)
  "bK" '("kill buffer" . kill-buffer)
  "bk" '("kill this buffer" . kill-current-buffer)

  "e" '(:ignore t :wk "emacs")
  "ec" '("async shell command" . async-shell-command)
  "er" '("eval region or line" . eval-region-and-go-to-normal-mode)
  "eb" '("eval buffer" . eval-buffer)
  "ee" '("eval expression" . eval-expression))

(general-define-key
 :states '(normal visual)
 "gc" 'comment-or-uncomment-region-or-line
 "<up>" 'enlarge-window
 "<left>" 'shrink-window-horizontally
 "<right>" 'enlarge-window-horizontally
 "<down>" 'shrink-window
 ";" 'evil-ex
 "M-k" 'enlarge-window
 "M-h" 'shrink-window-horizontally
 "M-l" 'enlarge-window-horizontally
 "M-j" 'shrink-window

 "C-j" #'backward-sexp
 "C-k" #'forward-sexp
 "C-d" #'kill-sexp)

(general-define-key
 "C-=" 'text-scale-increase
 "C--" 'text-scale-decrease
 "C-j" nil
 "<escape>" #'keyboard-quit
 "<escape>" #'keyboard-escape-quit
 "ESC" #'keyboard-quit
 "ESC" #'keyboard-escape-quit
 "<C-wheel-up>" 'text-scale-increase
 "<C-wheel-down>" 'text-scale-decrease)

(use-package org
  :demand t
  :mode ("\\.org\\'" . org-mode)
  :diminish org-indent-mode
  :custom
  (org-src-preserve-indentation t)
  (org-src-tab-acts-natively t)
  (org-startup-with-inline-images t)
  (org-log-done 'time)
  (calendar-week-start-day 1)
  (org-babel-load-languages '((emacs-lisp . t)
			      (shell . t)
			      (eshell . t)
			      (lisp . t)))
  :init
  (add-hook 'org-mode-hook #'org-indent-mode)
  (add-hook 'org-mode-hook (lambda () (electric-indent-local-mode -1)))
  :general
  (:states '(normal visual motion) :keymaps 'override :prefix "SPC"
	   "oa" '("org agenda" . org-agenda))
  :general-config
  (:keymaps 'org-mode-map
	    "C-j" nil)
  (:states '(normal visual motion) :keymaps 'org-mode-map :prefix "SPC l"
	   "l" '(:ignore t :wk "org link")
	   "li" '("insert org link" . org-insert-link)
	   "lo" '("open org link" . org-open-at-point)
	   "le" '("open org link" . org-edit-special)
	   "lt" '("toggle link display" . org-toggle-link-display))
  (:keymaps 'org-mode-map :states '(normal visual motion)
	    "RET" (lambda () (interactive)
		    (unless (ignore-errors (org-open-at-point))
		      (evil-ret)))))

(use-package evil-org
  :after org
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-roam
  :custom
  (org-roam-directory (file-truename "~/Documents/org/roam"))
  :config
  (unless (file-directory-p org-roam-directory)
    (make-directory org-roam-directory t))
  (setq org-agenda-files (org-roam-list-files))
  (org-roam-db-autosync-mode)
  (org-roam-setup)
  :general
  (:states '(normal visual motion) :keymaps 'override :prefix "SPC"
	   "r" '(:ignore t :wk "roam")
	   "rb" '("buffer" . org-roam-buffer-toggle)
	   "rf" '("find node" . org-roam-node-find)
	   "rI" '("create id" . org-id-get-create)
	   "ri" '("insert node" . org-roam-node-insert)))

(use-package org-download
  :hook (dired-mode . org-download-enable)
  :custom (org-download-screenshot-method "grim -g \"$(slurp)\" -t png %s")
  :general
  (:states '(normal visual motion) :keymaps 'org-mode-map :prefix "SPC l"
	   "s" '("screenshot" . org-download-screenshot)
	   "c" '("image from clipboard" . org-download-clipboard)))

(defun th/org-tempo-electric-pair-fix ()
  (setq-local electric-pair-inhibit-predicate
	      `(lambda (c)
		 (if (char-equal c ?<)
		     t
		   (,electric-pair-inhibit-predicate c)))))

(use-package org-tempo
  :demand t
  :ensure nil ;; included with org
  :after org
  :hook (org-mode . th/org-tempo-electric-pair-fix)
  :custom
  (org-structure-template-alist '(("el" . "src emacs-lisp"))))

(use-package org-bullets
  :diminish org-bullets-mode
  :hook (org-mode . org-bullets-mode))

(use-package olivetti
  :diminish olivetti-mode
  :custom 
  (olivetti-min-body-width 50)
  (olivetti-body-width 80)
  (olivetti-style 'fancy)
  (olivetti-margin-width 12)
  :config
  (set-face-attribute 'olivetti-fringe nil :background "#313244")
  :hook
  (olivetti-mode-on . (lambda () (olivetti-set-width olivetti-body-width)))
  (org-mode . olivetti-mode))

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :hook (pdf-view-mode . (lambda () (display-line-numbers-mode -1)))
  :config
  (evil-collection-pdf-setup))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (magit-auto-revert-mode nil)
  :general
  (:states '(normal visual motion) :keymaps 'override :prefix "SPC"
	   "g" '(:ignore t :wk "git")
	   "gg" '("open magit" . magit-status)
	   "gd" '(:ignore t :wk "diff")
	   "gdu" '("diff unstaged" . magit-diff-unstaged)
	   "gds" '("diff staged" . magit-diff-staged)
	   "gc" '("commit" . magit-commit)
	   "gp" '("push" . magit-push)
	   "gF" '("push" . magit-pull)))

(use-package forge
  :after magit)

(defun my/last-diff-hl-hunk (&optional backward)
  "Go to the last hunk in the file, first if BACKWARD is t."
  (while-let ((pos (diff-hl-search-next-hunk backward)))
    (goto-char (overlay-start pos))))

(defun advice!diff-hl-next-hunk-loop-around (orig-fun &rest args)
  (let ((backward (if (car args)
		      nil
		    t)) ;; flip
	(return (ignore-errors (funcall orig-fun args)))) 
    (unless return
      (my/last-diff-hl-hunk backward)
      (message "Looped around"))))

(use-package diff-hl
  :demand t
  :diminish diff-hl-mode
  :custom
  (diff-hl-show-staged-changes nil)
  (diff-hl-global-modes '(not image-mode pdf-view-mode))
  (diff-hl-update-async t)
  (vc-git-diff-switches '("--histogram"))
  :config
  (advice-add 'diff-hl-next-hunk :around #'advice!diff-hl-next-hunk-loop-around)
  (global-diff-hl-mode +1)
  (mapc (lambda (f) 
	  (set-face-background f "green")
	  (set-face-foreground f "green"))
	'(diff-hl-insert diff-hl-dired-insert diff-hl-margin-insert))
  (mapc (lambda (f) 
	  (set-face-background f "purple")
	  (set-face-foreground f "purple"))
	'(diff-hl-change diff-hl-dired-change diff-hl-margin-change))
  (mapc (lambda (f) 
	  (set-face-background f "red")
	  (set-face-foreground f "red"))
	'(diff-hl-delete diff-hl-dired-delete diff-hl-margin-delete))
  :hook
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh)

  (dired-mode . diff-hl-dired-mode)
  (diff-hl-mode . diff-hl-flydiff-mode)
  (diff-hl-mode . diff-hl-margin-mode) ;; to simultaniously support flycheck symbols in fringe
  :general
  (:states '(normal visual motion) :keymaps 'override :prefix "SPC"
	   "ga" '("stage hunk" . diff-hl-stage-current-hunk)
	   "gr" '("revert hunk" . diff-hl-revert-hunk)
	   "gn" '("next hunk" . diff-hl-next-hunk)
	   "gN" '("previous hunk" . diff-hl-previous-hunk)))

(use-package git-timemachine
  :general-config
  (:states 'normal :keymaps 'git-timemachine-mode-map
	   "<" 'git-timemachine-show-previous-revision
	   "J" 'git-timemachine-show-previous-revision
	   ">" 'git-timemachine-show-next-revision
	   "K" 'git-timemachine-show-next-revision
	   "i" nil ;; no point in going to insert mode, the buffer is read only
	   "C-f" (lambda () (git-timemachine-show-nth-revision 1))
	   "C-g" 'git-timemachine-show-nth-revision
	   "C-c" 'git-timemachine-show-current-revision)
  :general
  (:states '(normal visual motion) :keymaps 'override :prefix "SPC"
	   "gt" '("timemachine" . git-timemachine-toggle)))

(use-package emsg-blame
  :demand t
  :config
  (global-emsg-blame-mode t))

(use-package projectile
  :diminish projectile-mode
  :commands (projectile-run-eshell projectile-run-vterm)
  :custom
  (projectile-switch-project-action #'projectile-dired)
  :config
  (projectile-mode)
  :general
  (:states '(normal visual motion) :keymaps 'override :prefix "SPC"
	   "P" '(:keymap projectile-command-map :package projectile)
	   "p" '(:ignore t :package projectile :wk "project")
	   "pp" '("switch project" . projectile-switch-project)
	   "ps" '("search project" . (lambda () (interactive) (consult-ripgrep (projectile-project-root))))
	   "p." '("find project file" . projectile-find-file)
	   "po" '(:ignore t :wk "open")
	   "pog" '("project version control (git)" . projectile-vc)
	   "pb" '("switch buffer in project" . projectile-switch-to-buffer)))

(use-package ibuffer-projectile
  :hook
  (ibuffer-mode . (lambda () (ibuffer-projectile-set-filter-groups)
		    (unless (eq ibuffer-sorting-mode 'alphabetic)
		      (ibuffer-do-sort-by-alphabetic)))))

(use-package hl-todo
  :demand t
  :diminish hl-todo-mode
  :diminish global-hl-todo-mode
  :custom
  (hl-todo-keyword-faces '(("TODO" . ,(face-attribute 'error :foreground))
			   ("HACK" . ,(face-attribute 'warning :foreground))
			   ("NOTE" . ,(face-attribute 'match :foreground))
			   ("FIXME" . ,(face-attribute 'error :foreground))))
  :config
  (global-hl-todo-mode 1))

(use-package magit-todos
  :after magit
  :hook (magit-mode . magit-todos-mode)
  :config (magit-todos-mode 1))

(use-package yasnippet
  :custom
  (yas-snippets-dirs (expand-file-name "snippets" user-emacs-directory))
  :config
  (yas-global-mode 1))

(use-package eglot
  :commands eglot-ensure
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-size 0)
  (eglot-sync-connect nil)
  (eglot-connect-timeout nil)
  :config
  (fset #'jsonrpc--log-event #'ignore)
  ;; nixos executable is OmniSharp
  (setf (alist-get '(csharp-mode csharp-ts-mode) eglot-server-programs nil nil #'equal) '("OmniSharp" "-lsp"))
  :general-config
  (:states '(normal visual motion) :keymaps 'override :prefix "SPC"
	   "c" '(:ignore t :wk "code")
	   "ca" '("code actions" . (lambda () (interactive)
				     (eglot-code-actions 1 (point-max) nil t)))))

(use-package eglot-booster
  :demand t
  :after eglot
  :config (eglot-booster-mode))

(use-package flycheck
  :hook
  (prog-mode . flycheck-mode)
  (eglot-managed-mode . flycheck-mode)
  (flycheck-mode . (lambda () (flycheck-set-indication-mode 'left-fringe)))
  :general-config
  (:states '(normal visual motion) :keymaps 'override :prefix "SPC"
	   "cn" '("next error" . flycheck-next-error)
	   "cN" '("previous error" . flycheck-previous-error)))

(use-package flycheck-eglot
  :demand t
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))

(use-package apheleia
  :demand t
  :diminish apheleia
  :config
  (setf (alist-get 'nixfmt apheleia-formatters)
	'("alejandra"))
  (apheleia-global-mode +1))

(use-package corfu
  :demand t
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.18)
  (corfu-cycle t)
  :hook
  (after-init . global-corfu-mode)
  :general-config
  (:states '(insert)
	   "C-j" nil
	   "C-k" nil)
  (:states '(normal visual insert) :keymaps 'corfu-mode-map
	   "C-j" nil
	   "C-k" nil
	   "C-i" nil)
  (:keymaps 'corfu-map
	    "RET" nil
	    "<up>" nil
	    "<down>" nil
	    "M-i" (lambda () (interactive) 
		    (let ((current-prefix-arg t))
		      (call-interactively #'corfu-info-documentation)))
	    "C-j" #'corfu-next
	    "C-k" #'corfu-previous
	    "S-RET" #'corfu-complete
	    "S-<return>" #'corfu-complete))

(use-package rustic
  :diminish rustic-mode
  :mode ("\\.rs\\'" . rustic-mode)
  :hook (rustic-mode . eglot-ensure)
  :init
  (setq rustic-lsp-client 'eglot
	rustic-use-rust-save-some-buffers t
	compilation-ask-about-save nil))

(use-package nix-mode
  :mode "\\.nix\\'"
  :hook (nix-mode . eglot-ensure))

(use-package haskell-mode
  :mode "\\.hs\\'"
  :hook (haskell-mode . eglot-ensure))
;; TODO: try haskell-ts-mode
;; (use-package haskell-ts-mode
;;   :mode "\\.hs\\'"
;;   :config (haskell-ts-setup-eglot))

(use-package typescript-mode
  :mode "\\.tsx?\\'")

;; (use-package tsi
;;   :after tree-sitter
;;   :hook
;;   (typescript-mode . tsi-typescript-mode)
;;   (css-mode . tsi-css-mode)
;;   (scss-mode . tsi-scss-mode)
;;   (json-mode . tsi-json-mode))

(use-package lua-mode
  :mode "\\.lua\\'"
  :hook (lua-mode . eglot-ensure))

(use-package gdscript-mode
  :mode "\\.gdscript\\'"
  :hook (gdscript-mode . eglot-ensure))

(use-package glsl-mode)
(use-package gdshader-mode)

(use-package csharp-mode
  :ensure nil
  :mode "\\.cs\\'"
  :hook
  (csharp-mode . eglot-ensure)
  (csharp-mode . csharp-ts-mode))

(add-hook 'emacs-lisp-mode-hook #'corfu-mode)

(use-package puni
  :config
  (puni-global-mode)
  :general-config
  (:states '(normal visual) :keymaps 'override))

;; (use-package smartparens
;;   :demand t
;;   :diminish smartparens-mode
;;   :config
;;   (smartparens-global-mode)
;;   :general-config
;;   (:states '(normal visual) :keymaps 'override
;; 	   "C-h" #'sp-backward-sexp
;; 	   "C-k" #'sp-up-sexp
;; 	   "C-j" #'sp-down-sexp
;; 	   "C-l" #'sp-next-sexp
;; 	   "C-y" #'sp-copy-sexp
;; 	   )
;;   )

;; (use-package evil-smartparens
;;   :hook (smartparens-enabled . evil-smartparens-mode))

(use-package rainbow-delimiters
  :diminish rainbow-delimiters-mode
  :hook
  (prog-mode . rainbow-delimiters-mode)
  (org-mode . rainbow-delimiters-mode))

(use-package envrc
  :demand t
  :hook (after-init . envrc-global-mode))

;; (use-package inheritenv
;;   :config
;;   (inheritenv-add-advice 'shell-command-to-string))

(use-package gptel
  :custom
  (gptel-model 'gpt-4.1)
  :config
  (setq gptel-backend (gptel-make-gh-copilot "Copilot"))
  (add-to-list 'gptel-tools
	       (gptel-make-tool
		:function (lambda (query)
			    (with-temp-message (format "Searching for: `%s`" query)
			      ;; provided by nixos
			      (let ((url (format "http://127.0.0.1:8080/search?q=%s&format=json"
						 (url-hexify-string query))))
				(with-temp-buffer
				  (url-insert-file-contents url)
				  (let ((json-response (json-read)))
				    (mapconcat (lambda (result)
						 (format "%s - %s\n%s" (cdr (assoc 'title result)) (cdr (assoc 'url result)) (cdr (assoc 'content result))))
					       (cdr (assoc 'results json-response))
					       "\n\n"))))))
		:name "search_web"
		:description "Searches the web using SearXNG metasearch engine and returns formatted results including titles, URLs, and content excerpts."
		:args (list
		       '(:name "query"
			       :type string
			       :description "The search query to execute against the search engine."))
		:category "web"
		:include t)))

(defun th/vterm (&optional projectile)
  (if projectile
      (projectile-run-vterm t)
    (vterm t))
  (end-of-buffer)
  (evil-append-line 1))

(use-package vterm
  :hook (vterm-mode . th/turn-off-line-numbers)
  :commands (vterm)
  :general
  (:states '(normal visual motion) :keymaps 'override :prefix "SPC"
	   "ov" '((lambda () (interactive)
		    (select-window (th/intelligent-split t))
		    (th/vterm)) :wk "vterm")
	   "oV" '((lambda () (interactive)
		    (th/vterm)) :wk "vterm in this window")
	   "pov" '((lambda () (interactive)
		     (select-window (th/intelligent-split t))
		     (th/vterm t)) :wk "vterm")
	   "poV" '((lambda () (interactive)
		     (th/vterm t)) :wk "vterm in this window")))

(defun vterm-evil-insert ()
  (interactive)
  (vterm-goto-char (point))
  (call-interactively #'evil-insert))

(defun vterm-evil-append ()
  (interactive)
  (vterm-goto-char (1+ (point)))
  (call-interactively #'evil-append))

(defun vterm-evil-delete ()
  "Provide similar behavior as `evil-delete'."
  (interactive)
  (let ((inhibit-read-only t)
        )
    (cl-letf (((symbol-function #'delete-region) #'vterm-delete-region))
      (call-interactively 'evil-delete))))

(defun vterm-evil-change ()
  "Provide similar behavior as `evil-change'."
  (interactive)
  (let ((inhibit-read-only t))
    (cl-letf (((symbol-function #'delete-region) #'vterm-delete-region))
      (call-interactively 'evil-change))))

(general-def :states '(normal) :keymaps 'vterm-mode-map
  "a" 'vterm-evil-append
  "d" 'vterm-evil-delete
  "i" 'vterm-evil-insert
  "c" 'vterm-evil-change)

(use-package eshell-vterm
  :demand t
  :after eshell
  :config 
  (eshell-vterm-mode))

(use-package fish-completion)

(defun th/eshell (&optional projectile &rest args)
  (if projectile
      (projectile-run-eshell t)
    (eshell t))
  (end-of-buffer)
  (evil-append-line 1))

(use-package eshell
  :ensure nil
  :after (fish-completion)
  :commands (eshell projectile-run-eshell)
  :custom
  (eshell-history-size 10000000)
  :config
  (add-to-list 'eshell-modules-list 'eshell-elecslash)
  (add-to-list 'eshell-modules-list 'eshell-tramp)

  (setq eshell-visual-commands '("nix"
				 "nix-build"
				 "nixos-rebuild"
				 "nh"
				 "deploy"
				 "btop"
				 "htop"))
  :hook
  (eshell-mode . th/turn-off-line-numbers)
  (eshell-mode . fish-completion-mode)
  :general-config
  (:states '(normal visual) :keymaps 'eshell-mode-map
	   "A" (lambda () (interactive) (end-of-buffer) (evil-append-line 1)))
  (:states '(normal visual insert) :keymaps 'eshell-mode-map
	   "C->" (lambda () (interactive) 
		   (insert (concat "> #<buffer " (read-buffer "Send to: ") ">")))
	   "C-p" (lambda () (interactive)
		   (insert (read-file-name "Insert path: "))))
  (:keymaps 'eshell-mode-map :states '(normal visual motion)
	    "RET" (lambda () (interactive)
		    (unless (ignore-errors (browse-url))
		      (evil-ret))))
  :general
  (:states '(normal visual insert emacs motion) :prefix "SPC" :keymaps 'override :global-prefix "C-SPC"
	   "oe" '("eshell" . (lambda () (interactive) 
			       (select-window (th/intelligent-split t)) 
			       (th/eshell)))
	   "oE" '("eshell in this window" . (lambda () (interactive) (th/eshell)))
	   "poe" '("eshell" . (lambda () (interactive) 
				(select-window (th/intelligent-split t))
				(th/eshell t)))
	   "poE" '("eshell in this window" . (lambda () (interactive) (th/eshell t)))))

(defun eshell/v (&rest args)
  (select-window (th/intelligent-split t))
  (apply 'eshell-exec-visual args))

(defalias 'eshell/V 'eshell-exec-visual)

(defun eshell/c ()
  (eshell/cd
   (read-file-name "Change directory: ")))

(defvar eshell-nix-shell-active nil
  "Show <nix-shell> in the eshell prompt.")

(defun th/eshell-prompt ()
  (concat
   (if eshell-nix-shell-active
       (propertize "<nix-shell> " 'face '(:foreground "green"))
     "")
   (abbreviate-file-name (eshell/pwd))
   
   (if (magit-toplevel)
       (propertize (format "  %s" (magit-get-current-branch)) 'face '(:foreground "#cba6f7"))
     "")

   (propertize " λ" 'face
	       (if (string-match (rx
				  "/sudo:root"
				  (* nonl))
				 (eshell/pwd))
		   '(:foreground "red")
		 '(:foreground "purple")))
   (propertize " " 'face
	       'default)))

(setq eshell-prompt-function #'th/eshell-prompt)
(setq eshell-prompt-regexp
      (rx line-start
	  (*?
	   nonl)
	  "λ "))

;; (setq eshell-banner-message
;;       (let ((os (with-temp-buffer
;; 		  (insert-file-contents "/etc/os-release")
;; 		  (goto-char (point-min))
;; 		  (re-search-forward "PRETTY_NAME=\"\\(.*\\)\"")
;; 		  (match-string 1))))
;; 	(concat os "\n\n")))

(defun eshell/nix-shell (&rest args)
  (if (member "--run" args)
      (eshell-command-result
       (concat "*nix-shell " (mapconcat 'identity args " ")))
    (let* ((output (shell-command-to-string
		    (format "nix-shell %s --run \"env\""
			    (mapconcat 'identity args " "))))
	   (lines (split-string output "\n" t))
	   (environment (mapcar (lambda (line)
				  (s-split-up-to "=" line 1))
				lines)))
      (dolist (env environment)
	(when (= 2 (length env))
	  (if (string= (car env) "PATH")
	      (eshell-set-path (cadr env))
	    (ignore-errors
	      (eshell-set-variable (car env) (cadr env))))
	  
	  (setq-local eshell-nix-shell-active t))))))

(defmacro re (&rest rx-sexp) ;; Stolen from https://youtube.com/watch?v=9xLeqwl_7n0
  "Convert rx expression RX-SEXP to pcre compatible regexp."
  `(rxt-elisp-to-pcre (rx ,@rx-sexp)))

(defalias 'eshell/less 'view-file)

(defun eshell/exit ()
  (evil-quit)
  (throw 'eshell-terminal t))
(defalias 'eshell/e 'eshell/exit)

(use-package vertico
  :demand t
  :custom
  (vertico-resize t)
  (vertico-cycle t)
  :general-config
  ;; evil-want-minibuffer t
  (:keymaps 'vertico-map :states '(normal visual)
	    "j" #'vertico-next
	    "k" #'vertico-previous
	    "gg" #'vertico-first
	    "G" #'vertico-last)
  (:keymaps 'vertico-map :states '(normal visual insert)
	    "RET" #'vertico-exit
	    "C-u" #'vertico-quick-exit
	    "C-j" #'vertico-next
	    "C-k" #'vertico-previous
	    "C-l" #'vertico-quick-jump)
  (:keymaps 'vertico-map :states '(insert)
	    "<backspace>" #'vertico-directory-delete-char
	    "DEL" #'vertico-directory-delete-char)
  (:keymaps 'override :states '(normal visual insert)
	    "C-c c" #'vertico-buffer-mode)
  :config
  (vertico-mode))

(use-package consult
  :demand t
  :custom
  (consult-preview-excluded-buffers '(major-mode . exwm-mode))
  (consult-line-start-from-top nil)
  :general-config
  ;; (:states '(normal visual)
  ;; 	   "/" #'evil-search-forward
  ;; 	   "?" #'evil-search-backward
  ;; "C-/" #'evil-search-forward
  ;; "C-?" #'evil-search-backward
  ;; )
  (:states '(normal visual motion) :keymaps 'override :prefix "SPC"
	   "sg" '("grep" . (lambda () (interactive)
			     (consult-ripgrep (expand-file-name ""))))
	   "f" '("recent file" . consult-recent-file)
	   "sf" '("find" . consult-fd)
	   "si" '("imenu" . consult-imenu)
	   "bs" '("switch" . consult-buffer)
	   "bo" '("open buffer in new window" (lambda () (interactive)
						(select-window (th/intelligent-split t))
						(consult-buffer)))
	   ))

(use-package consult-flycheck
  :general
  (:states '(normal visual motion) :keymaps 'override :prefix "SPC"
	   "sd" '("flycheck" . consult-flycheck)
	   ))

(defun advice!-consult-exwm-preview-fix (&rest _args)
  "Kludge to stop EXWM buffers from stealing focus during Consult previews."
  (when (derived-mode-p 'exwm-mode)
    (when-let ((mini (active-minibuffer-window)))
      (select-window (active-minibuffer-window)))))

(advice-add
 #'consult--buffer-preview :after #'advice!-consult-exwm-preview-fix)

(defun advice!-consult-grep-evil-search-history (ret)
  "Add the selected item to the evil search history."
  (when ret ;; return value is nil if you quit early
    (let ((search (if (string= (substring (car consult--grep-history) 0 1) "#")
		      (substring (car consult--grep-history) 1 nil)
		    (car consult--grep-history))))
      (add-to-history 'regexp-search-ring search)
      (add-to-history 'evil-ex-search-history search)
      (setq evil-ex-search-pattern (list search t t))
      (setq evil-ex-search-direction 'forward))
    ret))
(advice-add 'consult--grep :filter-return #'advice!-consult-grep-evil-search-history)

(defun advice!-consult-line-evil-search-history (ret)
  "Add the selected item to the evil search history."
  (when ret ;; return value is nil if you quit early
    (let ((search (car consult--line-history)))
      (add-to-history 'regexp-search-ring search)
      (add-to-history 'evil-ex-search-history search)
      (setq evil-ex-search-pattern (list search t t))
      (setq evil-ex-search-direction 'forward))
    ret))
(advice-add 'consult-line :filter-return #'advice!-consult-line-evil-search-history)

(use-package wgrep)

(use-package embark
  ;; :after wgrep
  :demand t
  :general-config
  (
   "C-;" #'embark-act
   "C-a" #'embark-select))

(use-package embark-consult
  :after embark
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package orderless
  :demand t
  :after (vertico consult)
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :demand t
  :config
  (marginalia-mode))

(defun advice!-crm-indicator (args)
  (cons (format "[CRM%s] %s"
		(replace-regexp-in-string
		 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
		 crm-separator)
		(car args))
	(cdr args)))
(advice-add #'completing-read-multiple :filter-args #'advice!-crm-indicator)

(setq minibuffer-prompt-properties '(read-only t cursor-intangible-mode t face minibuffer-prompt)
      enable-recursive-minibuffers t)

(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(general-def :states '(normal visual motion) :keymaps 'override :prefix "SPC"
  "m" '(:ignore t :wk "media"))

(use-package simple-mpc
  :demand t
  :hook (simple-mpc-mode . th/turn-off-line-numbers)
  :custom
  (simple-mpc-volume-step-size 3)
  ;; :general
  ;; (:states '(normal visual motion) :keymaps 'override :prefix "SPC" 
  ;;   "m" '(:ignore t :wk "media")
  ;;   "mm" '("open simple-mpc" . simple-mpc)
  ;;   "ms" '("search" . simple-mpc-query)
  ;;   "mp" '("play/pause" . simple-mpc-toggle)
  ;;   "mC" '("clear" . simple-mpc-clear-current-playlist)
  ;;   "mP" '("playlist" . simple-mpc-view-current-playlist)
  ;;   "ma" '("load playlist" . simple-mpc-load-playlist)
  ;;   "mh" '("prev" . simple-mpc-prev)
  ;;   "ml" '("next" . simple-mpc-next)))
  )

(use-package empv
  :demand t
  :general-config
  (:keymaps 'empv-youtube-results-mode :states '(normal visual insert)
	    "RET" 'empv-youtube-results-play-current)
  :init 
  (setq empv-invidious-instance "https://yewtu.be/api/v1")
  (setq empv-volume-step 3)
  (setq empv-radio-channels '(("nowhere.moe Cyberia" . "https://radio.nowhere.moe/radio/cyberia.mp3")
			      ("nowhere.moe Focus" . "https://radio.nowhere.moe/radio/focus.mp3")
			      ("nowhere.moe Nihilism" . "https://radio.nowhere.moe/radio/nihilism.mp3")
			      ("nowhere.moe Psychedelia" . "https://radio.nowhere.moe/radio/psychedelia.mp3"))))

(defun eshell/yt (&rest args)
  (empv-youtube (mapconcat (lambda (s) (format "%s " s)) args)))

(use-package transient)

;; (defun media-menu--empv-remove-playlist-item ()
;;   (interactive)
;;   (empv--playlist-select-item-and
;;    (empv-playlist-remove item)))

(transient-define-prefix empv-menu ()
  "Transient menu for empv."
  [["Menu"
    ("q" "Quit" transient-quit-one)]
   ["Playback"
    ("p" "Toggle" empv-toggle :transient t)
    ("v" "Toggle Video" empv-toggle-video :transient t)

    ("j" "Previous" empv-playlist-prev :transient t)
    ("k" "Next" empv-playlist-next :transient t)

    ("x" "Close MPV" empv-exit :transient t)]
   ["Playlist"
    ("Y" "Search Youtube" empv-youtube)
    ("f" "Play File" empv-play-file)
    ("s" "Select From Playlist" empv-playlist-select)]
   ["Settings"
    ("y" "Toggle Single" empv-toggle-file-loop :transient t)
    ("r" "Toggle Repeat" empv-toggle-playlist-loop :transient t)
    
    ("-" "Volume Down" empv-volume-down :transient t)
    ("=" "Volume Up" empv-volume-up :transient t)]])

(transient-define-prefix mpd-menu ()
  "Transient menu for empv."
  [["Menu"
    ("q" "Quit" transient-quit-one)]
   ["Playback"
    ("p" "Toggle" simple-mpc-toggle :transient t)
    
    ("j" "Previous" simple-mpc-prev :transient t)
    ("k" "Next" simple-mpc-next :transient t)]
   ["Playlist"
    ("l" "Load playlist" simple-mpc-load-playlist :transient t)
    ("s" "Search" simple-mpc-query)
    
    ("c" "View playlist" simple-mpc-view-current-playlist)
    ("C" "Clear playlist" simple-mpc-clear-current-playlist :transient t)]
   ["Settings"
    ("y" "Toggle Single" (lambda () (interactive)
			   (simple-mpc-call-mpc nil "single")) :transient t)
    ("r" "Toggle Repeat" simple-mpc-toggle-repeat :transient t)

    ("-" "Volume Down" simple-mpc-decrease-volume :transient t)
    ("=" "Volume Up" simple-mpc-increase-volume :transient t)]])

(defmacro media-menu--mpv-or-mpd-action (mpv mpd &optional mpv-args mpd-args)
  "If the mpv playlist is not empty, call MPV, else call MPD.
MPV is called with MPV-ARGS and MPD is called with MPD-ARGS."
  `(empv--send-command
    '("get_property_string" "playlist")
    (lambda (result)
      (if (> (length (json-parse-string result)) 0)
	  (apply ,mpv ,mpv-args)
	(apply ,mpd ,mpd-args)))))

(defun media-menu--toggle ()
  (interactive)
  (media-menu--mpv-or-mpd-action #'empv-toggle #'simple-mpc-toggle))

(defun media-menu--volume-increase()
  (interactive)
  (media-menu--mpv-or-mpd-action #'empv-volume-up #'simple-mpc-increase-volume))

(defun media-menu--volume-decrease()
  (interactive)
  (media-menu--mpv-or-mpd-action #'empv-volume-down #'simple-mpc-decrease-volume))

(transient-define-prefix media-menu ()
  "Transient menu for simple-mpc and empv."
  [["Menu"
    ("q" "Quit" transient-quit-one)
    ("e" "Open MPV menu" empv-menu)
    ("m" "Open MPD menu" mpd-menu)]
   ["Playback"
    ("p" "toggle playback" media-menu--toggle :transient t)
    ("p" "toggle playback" media-menu--toggle :transient t)
    ]
   ["Settings"
    ("-" "Volume down" media-menu--volume-decrease :transient t)
    ("=" "Volume up" media-menu--volume-increase :transient t)]])

(general-def :states '(normal visual motion) :keymaps 'override :prefix "SPC" 
  "m" '("media menu" . media-menu))

(use-package separedit)

(use-package ultra-scroll
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0) 
  :config
  (ultra-scroll-mode 1))

(use-package tramp-sh
  :ensure nil ;; part of emacs
  :config
  (setq tramp-remote-path
	(append tramp-remote-path
 		'(tramp-own-remote-path))))

(use-package dired
  :ensure nil
  :demand t
  :hook (dired-mode . hl-line-mode)
  :hook (dired-mode . auto-revert-mode)
  :custom
  (dired-dwim-target t)
  (dired-mouse-drag-files t)
  (dired-listing-switches "-alh")
  :general-config
  (:keymaps 'dired-mode-map :states '(normal insert visual motion)
	    "SPC" nil
	    "q" 'evil-quit
	    "<backspace>" 'dired-up-directory
	    "C-<return>" (lambda () (interactive) (empv-play (dired-get-filename))))
  (:keymaps 'dired-mode-map :states '(normal visual motion) :prefix "SPC"
	    "oe" '("eshell in this window" . (lambda () (interactive) (th/eshell))))
  :config
  (unless (display-graphic-p)
    (general-def dired-mode-map "DEL" 'dired-up-directory)))

(use-package dired-du)

(use-package openwith
  :custom
  (openwith-associations `((,(rx nonl (or ".mkv"
					  ".mp4"
					  ".mov"
					  ".webm"))
			    . ("mpv" (file)))
			   ))
  :config
  (openwith-mode))

(defvar th/first-server-frame-created nil)
(defun th--unless-first-server-frame-created (func)
  (unless th/first-server-frame-created
    (funcall func)
    (setq th/first-server-frame-created t)))

(use-package solaire-mode
  :hook
  (after-init . (lambda ()
		  (when (display-graphic-p) (solaire-global-mode +1))))
  (server-after-make-frame . (lambda ()
			       (when (display-graphic-p) (solaire-global-mode +1)))))

(use-package nyan-mode
  :custom
  (nyan-animate-nyancat t)
  (nyan-wavy-trail t))

(defun th/mode-line ()
  (dolist (face '(mode-line mode-line-active mode-line-inactive))
    (setf (alist-get face solaire-mode-remap-alist) nil))

  (set-face-attribute 'mode-line-active nil :inherit 'mode-line-inactive :foreground (face-attribute 'default :foreground))
  
  (mapc (lambda (face)
	  (set-face-attribute face nil
			      :box `(:line-width (1 . 10) :color ,(face-background face nil t) :style nil)
			      :height 110))
	'(mode-line-active mode-line-inactive))

  (setq mode-line-format nil)
  (kill-local-variable 'mode-line-format)
  (force-mode-line-update)

  (let* ((default-face `(:foreground ,(face-attribute 'default :foreground)))
	 (okay-face `(:foreground ,(face-attribute 'match :foreground)))
	 (error-face `(:foreground ,(face-attribute 'error :foreground)))
	 (warning-face `(:foreground ,(face-attribute 'warning :foreground)))
	 (emphasize-face `(:foreground ,(face-attribute 'font-lock-keyword-face :foreground)))

	 (envrc-none (propertize "" 'face default-face))
	 (envrc-on (propertize "" 'face okay-face))
	 (envrc-error (propertize "" 'face error-face)))
    (setq-default mode-line-format
		  `(
		    "   "
		    (:eval
		     (propertize "%b" 'face 'bold))
		    "   L%l   " ;; line number

		    (:eval
		     (when (file-remote-p default-directory)
		       (let* ((vec (tramp-dissect-file-name default-directory))
			      (user (or (tramp-file-name-user vec) ""))
			      (host (tramp-file-name-host vec)))
			 (propertize (format "%s@%s   " user host) 'face ,emphasize-face))))

		    (:eval
		     (pcase envrc--status
		       ('none ,envrc-none)
		       ('on ,envrc-on)
		       (_ ,envrc-error)))

		    "   "
		    
		    (:eval
		     (when (and (not (file-remote-p default-directory)) (magit-toplevel))
		       (propertize (format "  %s   " (magit-get-current-branch)) 'face ',emphasize-face)))
		    
		    (eglot--managed-mode eglot--mode-line-format "")
		    (eglot--managed-mode "   " "")
		    
		    (flycheck-mode
		     (:eval
		      (when (and (eq flycheck-last-status-change 'finished))
			(let-alist (flycheck-count-errors flycheck-current-errors)
			  (concat
			   (when (and (not .error) (not .warning:?) (not .warning))
			     (propertize "" 'face ',okay-face))
			   (when .error
			     (propertize (format " %s" .error) 'face ',error-face))
			   (when (or .warning:? .warning)
			     (propertize (format "%s %s" (if .error " " "") (+ (or .warning:? 0) (or .warning 0))) 'face ',warning-face))))))
		     
		     "")

		    (flycheck-mode "   " "")

		    ;; "%="
		    (:eval (nyan-create))))))

(add-hook 'enable-theme-functions
	  (lambda (_theme) (th/mode-line)))

;; (add-hook 'window-setup-hook #'th/mode-line)
;; (add-hook 'server-after-make-frame-hook #'th/mode-line)

(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-vibrant t)
  (doom-themes-org-config))

(use-package ligature
  :config
  (ligature-set-ligatures 'prog-mode
			  '("==" "===" "!=" "!==" "&&" "||"))
  (global-ligature-mode t))

(use-package all-the-icons)

(use-package kind-icon
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(defun th--ati-dired ()
  (when (display-graphic-p)
    (th--unless-first-server-frame-created
     (lambda () (add-hook 'dired-mode-hook #'all-the-icons-dired-mode)))))
(use-package all-the-icons-dired
  :after all-the-icons
  :diminish all-the-icons-dired-mode
  :hook (dired-mode . (lambda ()
			(when (display-graphic-p)
			  (all-the-icons-dired-mode)))))

(defun th--ati-ibuffer ()
  (when (display-graphic-p)
    (th--unless-first-server-frame-created
     (lambda () (add-hook 'ibuffer-mode-hook #'all-the-icons-ibuffer-mode)))))
(use-package all-the-icons-ibuffer
  :after all-the-icons
  :diminish all-the-icons-ibuffer-mode
  :hook (ibuffer-mode . (lambda ()
			  (when (display-graphic-p)
			    (all-the-icons-ibuffer-mode)))))

;; TODO implement conditional system
;; (use-package dmenu
;;   :demand t)

;; ;; custom package by me, defined in default.nix
;; ;; allows for per monitor workspaces to be bound to mod+1-9, like on dwm
;; (use-package dwm-workspaces
;;   :demand t)

;; (use-package exwm
;;   :demand t)

;; (use-package exwm-firefox-evil
;;   :config
;;   (add-hook 'exwm-manage-finish-hook #'exwm-firefox-evil-activate-if-firefox))

;; (when (and (getenv "EMACS_ENABLE_EXWM") (executable-find "wmctrl"))
;;   (unless (eq (call-process "wmctrl" nil nil nil "-m") 0)
;;     (progn
;;       (require 'exwm-randr)
;;       (require 'exwm-systemtray)
;;       (require 'exwm-xim)

;;       (dwm-workspaces--init)

;;       (defun th/exwm-shell-cmd (command) (start-process-shell-command (car (split-string command " ")) nil command))

;;       (defun th/keyboard-layout ()
;; 	(interactive)
;; 	(let* ((output (shell-command-to-string "setxkbmap -query"))
;; 	       (layout (nth 2 (split-string output "\n"))))
;; 	  (if (string-match-p "us" layout)
;; 	      (shell-command-to-string "setxkbmap fi")
;; 	    (shell-command-to-string "setxkbmap us"))))

;;       (mapc 'th/exwm-shell-cmd
;; 	    '("xset r rate 300 50"
;; 	      "dbus-update-activation-environment --verbose --systemd DBUS_SESSION_BUS_ADDRESS DISPLAY XAUTHORITY &"
;; 	      "gnome-keyring-daemon"))

;;       (when (file-directory-p "/sys/class/power_supply/BAT0/")
;; 	(display-battery-mode))

;;       (setq display-time-format "%H:%M:%S - %d %b %Y (%a)"
;; 	    display-time-default-load-average nil

;; 	    mouse-autoselect-window t
;; 	    focus-follow-mouse t

;; 	    exwm-input-line-mode-passthrough t
;; 	    exwm-workspace-show-all-buffers t)
;;       (display-time-mode 1)

;;       (dolist (k `(
;;                    escape
;;                    ))
;;         (cl-pushnew k exwm-input-prefix-keys))


;;       (defun advice!-exwm-input--on-ButtonPress-line-mode (buffer button-event)
;; 	"Handle button events in line mode.
;; BUFFER is the `exwm-mode' buffer the event was generated
;; on. BUTTON-EVENT is the X event converted into an Emacs event.

;; The return value is used as event_mode to release the original
;; button event."
;; 	(with-current-buffer buffer
;; 	  (let ((read-event (exwm-input--mimic-read-event button-event)))
;; 	    (exwm--log "%s" read-event)
;; 	    (if (and read-event
;; 		     (exwm-input--event-passthrough-p read-event))
;; 		;; The event should be forwarded to emacs
;; 		(progn
;; 		  (exwm-input--cache-event read-event)
;; 		  (exwm-input--unread-event button-event)

;; 		  xcb:Allow:ReplayPointer)
;; 	      ;; The event should be replayed
;; 	      xcb:Allow:ReplayPointer))))

;;       (advice-add 'exwm-input--on-ButtonPress-line-mode :override #'advice!-exwm-input--on-ButtonPress-line-mode)

;;       (setq exwm-input-global-keys
;; 	    `((,(kbd "s-i") . exwm-input-toggle-keyboard)
;; 	      (,(kbd "s-f") . exwm-layout-toggle-fullscreen)
;; 	      (,(kbd "s-S-F") . exwm-floating-toggle-floating)
;; 	      (,(kbd "s-d") . dmenu)
;; 	      (,(kbd "s-SPC") . th/keyboard-layout)
;; 	      (,(kbd "<XF86AudioPlay>") . simple-mpc-toggle)

;; 	      ;; (,(kbd "M-x") . execute-extended-command)

;; 	      (,(kbd "s-.") . dwm-workspaces--select-previous-monitor)
;; 	      (,(kbd "s-,") . dwm-workspaces--select-next-monitor)

;; 	      (,(kbd "s-1") . (lambda () (interactive) (dwm-workspaces--switch-by-index 1)))
;; 	      (,(kbd "s-2") . (lambda () (interactive) (dwm-workspaces--switch-by-index 2)))
;; 	      (,(kbd "s-3") . (lambda () (interactive) (dwm-workspaces--switch-by-index 3)))
;; 	      (,(kbd "s-4") . (lambda () (interactive) (dwm-workspaces--switch-by-index 4)))
;; 	      (,(kbd "s-5") . (lambda () (interactive) (dwm-workspaces--switch-by-index 5)))
;; 	      (,(kbd "s-6") . (lambda () (interactive) (dwm-workspaces--switch-by-index 6)))
;; 	      (,(kbd "s-7") . (lambda () (interactive) (dwm-workspaces--switch-by-index 7)))
;; 	      (,(kbd "s-8") . (lambda () (interactive) (dwm-workspaces--switch-by-index 8)))
;; 	      (,(kbd "s-9") . (lambda () (interactive) (dwm-workspaces--switch-by-index 9)))

;; 	      ;; (,(kbd "s-!") . (lambda () (interactive) (dwm-workspaces--move-window-by-index 1)))
;; 	      ;; (,(kbd "s-@") . (lambda () (interactive) (dwm-workspaces--move-window-by-index 2)))
;; 	      ;; (,(kbd "s-#") . (lambda () (interactive) (dwm-workspaces--move-window-by-index 3)))
;; 	      ;; (,(kbd "s-$") . (lambda () (interactive) (dwm-workspaces--move-window-by-index 4)))
;; 	      ;; (,(kbd "s-%") . (lambda () (interactive) (dwm-workspaces--move-window-by-index 5)))
;; 	      ;; (,(kbd "s-^") . (lambda () (interactive) (dwm-workspaces--move-window-by-index 6)))
;; 	      ;; (,(kbd "s-&") . (lambda () (interactive) (dwm-workspaces--move-window-by-index 7)))
;; 	      ;; (,(kbd "s-*") . (lambda () (interactive) (dwm-workspaces--move-window-by-index 8)))
;; 	      ;; (,(kbd "s-(") . (lambda () (interactive) (dwm-workspaces--move-window-by-index 9)))

;;               ;; ,@(mapc (lambda (i)
;;               ;;           (,(kbd (format "s-%d" i)) .
;;               ;;            (lambda () (interactive)
;;               ;;              (dwm-workspaces--switch-by-index ,i))))
;;               ;;         (number-sequence 1 9))

;;               ,@(cl-mapcar (lambda (c n)
;;                              (,(kbd (format "s-%c" c)) .
;;                               (lambda () (interactive)
;;                                 (dwm-workspaces--move-window-by-index ,n))))
;;                            '(?! ?@ ?# ?$ ?% ?^ ?& ?* ?\()
;;                            (number-sequence 1 9))
;; 	      ))

;;       (mapc (lambda (keybind)
;; 	      (global-set-key (car keybind) (cdr keybind)))
;; 	    exwm-input-global-keys)

;;       ;; https://github.com/minad/corfu/discussions/408
;;       (defun advice!corfu-make-frame-with-monitor-awareness (func frame x y width height)
;; 	"Advice `corfu--make-frame` to be monitor-aware, adjusting X and Y according to the focused monitor."
;; 	(let* ((workarea (nth exwm-workspace-current-index exwm-workspace--workareas))
;; 	       (mon-x (oref workarea x))
;; 	       (mon-y (oref workarea y)))
;; 	  (funcall func frame (+ mon-x x) (+ mon-y y) width height)))

;;       (advice-add 'corfu--make-frame :around #'advice!corfu-make-frame-with-monitor-awareness)

;;       (general-def :states '(normal visual motion) :keymaps 'override :prefix "SPC"
;; 	"y" '(:ignore t :wk "exwm")
;; 	"yd" '("dmenu" . dmenu)
;; 	"yf" '("toggle floating" . exwm-floating-toggle-floating))

;;       (general-define-key 
;;        :states '(normal visual visual replace motion emacs operator-pending)
;;        :keymaps 'exwm-mode-map
;;        "<mouse-1>" (lambda () (interactive) (exwm-input--fake-key 'down))
;;        "<mouse-2>" nil
;;        "<mouse-3>" nil
;;        "<down-mouse-1>" nil
;;        "<down-mouse-2>" nil
;;        "<down-mouse-3>" nil

;;        ;; "i" 'exwm-input-release-keyboard

;;        "h" (lambda () (interactive) (exwm-input--fake-key 'left))
;;        "j" (lambda () (interactive) (exwm-input--fake-key 'down))
;;        "k" (lambda () (interactive) (exwm-input--fake-key 'up))
;;        "l" (lambda () (interactive) (exwm-input--fake-key 'right)))

;;       (setq exwm-workspace-warp-cursor t
;; 	    exwm-layout-show-all-buffers t
;; 	    mouse-autoselect-window t
;; 	    focus-follows-mouse t)

;;       (setq ibuffer-saved-filter-groups
;; 	    '(("default"
;; 	       ("Process" (mode . comint-mode))
;; 	       )))

;;       (add-hook 'ibuffer-mode-hook
;; 		(lambda ()
;; 		  (ibuffer-switch-to-saved-filter-groups "default")))

;;       (add-hook 'exwm-update-class-hook
;; 		(lambda ()
;; 		  (if exwm-class-name
;; 		      (exwm-workspace-rename-buffer exwm-class-name)
;; 		    (exwm-workspace-rename-buffer (generate-new-buffer-name "EXWM - Unknown window")))
;; 		  (exwm-workspace-rename-buffer (format "EXWM - %s" exwm-class-name))))

;;       (exwm-xim-mode 1)
;;       (exwm-randr-mode 1)
;;       (exwm-enable)
;;       (exwm-systemtray-mode 1))))
