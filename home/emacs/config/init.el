(require 'diminish)
(require 'undo-fu)
(require 'evil)
(require 'evil-collection)
(require 'evil-better-visual-line)
(require 'smartparens)
(require 'evil-smartparens)
(require 'which-key)
(require 'general)

(require 'org-bullets)
(require 'solaire-mode)
(require 'doom-themes)
(require 'hl-todo)
(require 'flymake)
(require 'eglot)
(require 'company)
(require 'company-box)
(require 'treesit-auto)

(require 'rustic)
(require 'lua-mode)
(require 'csharp-mode)
(require 'gdscript-mode)
(require 'nix-mode)
(require 'rainbow-delimiters)

(require 'projectile)
(require 'ibuffer-projectile)
(require 'magit)
(require 'vterm)
(require 'eshell-vterm)
(require 'fish-completion)
(require 'vertico)
(require 'orderless)
(require 'consult)
(require 'marginalia)
(require 'popper)
(require 'simple-mpc)
(require 'empv)

;; emacs

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

(global-display-line-numbers-mode 1)
(global-visual-line-mode t)

(recentf-mode)

(dolist (mode '(vterm-mode-hook
		simple-mpc-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(global-set-key (kbd "s-`") #'(lambda () (interactive) (insert "`")))
(global-set-key (kbd "<escape>") #'keyboard-escape-quit)

;; dired

(define-key dired-mode-map (kbd "SPC") nil)
(define-key dired-mode-map (kbd "<backspace>") #'dired-up-directory)

;; functions

(defun list-active-modes ()
  (interactive)
  (let ((active-modes))
    (mapc (lambda (mode) (condition-case nil
			     (if (and (symbolp mode) (symbol-value mode))
				 (add-to-list 'active-modes mode))
			   (error nil)))
	  minor-mode-list)
    (message "Active modes: %s" active-modes)))

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

;; window config thing

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

;; evil

(setq evil-want-integration t
      evil-want-keybinding nil
      evil-vsplit-window-right t
      evil-split-window-below t
      evil-undo-system 'undo-fu)

(evil-mode)

;; which-key

(which-key-setup-side-window-bottom)
(which-key-mode)

;; general

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
  "ot" '(eshell :wk "open")
  
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
  "ee" '(eval-expression :wk "eval expression")
  
  "ec" '(:ignore t :wk "config")
  "eco" '((lambda () (interactive) (find-file (expand-file-name "config.org" user-emacs-directory))) :wk "open config")
  "ecc" '(recompile-config :wk "recompile config")
  "ecl" '(only-load-config :wk "load config")
  "ecL" '(reload-config :wk "recompile and load config"))

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
 :states 'motion
 "<escape>" nil)

(general-define-key
 "C-=" 'text-scale-increase
 "C--" 'text-scale-decrease
 "<C-wheel-up>" 'text-scale-increase
 "<C-wheel-down>" 'text-scale-decrease)

;; diminish

(diminish which-key-mode)
(diminish font-lock-mode)
(diminish visual-line-mode)


