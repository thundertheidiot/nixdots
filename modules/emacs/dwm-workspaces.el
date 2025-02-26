;;; dwm-workspaces.el --- DWM/Awesome like workspaces for EXWM

;; Author: Thunder
;; Version: 1.0
;; Package-Requires: ((exwm "0.28") ("dash" "2.19.1"))
;; Keywords: exwm

;;; Commentary:

;; This package gives you DWM-like workspace functionality inside EXWM
;; How workspaces work inside DWM:
;; Each monitor gets assigned 9 workspaces, 1-9, pressing MOD+number takes you to that numbered workspace,
;; MOD+comma/period cycles through the monitors.

;;; Code:

(defvar dwm-workspaces--workspaces-per-monitor 9
  "The amount of workspaces designated for each monitor.")

(defvar dwm-workspaces--monitor-list '()
  "List of monitors.")
(defvar dwm-workspaces--monitor-workspace '()
  "Currently focused workspace index based on monitor index.")

(defvar dwm-workspaces--workspaces-real '()
  "List of the real EXWM workspaces.")
(defvar dwm-workspaces--workspaces-display '()
  "List of the displayed workspaces.")

(defvar dwm-workspaces--current-monitor 0
  "Current focused monitor, do not touch this!
If you change this, the package will very likely stop functioning well.")

;;;###autoload
(defun dwm-workspaces--init ()
  "Init dwm-workspaces.  This should also work as a reset function.
Be careful though, all of your workspaces will be deleted."
  (interactive)
  (setq dwm-workspaces--monitor-list (dwm-workspaces--get-monitor-list))
  (setq dwm-workspaces--workspaces-real '()
	dwm-workspaces--workspaces-display '()
	dwm-workspaces--monitor-workspace '()
	exwm-randr-workspace-monitor-plist '())

  ;; exwm has to be loaded
  (require 'exwm-randr)

  (add-hook 'focus-in-hook #'dwm-workspaces--focus-changed)

  (mapc
   (lambda (w)
     (exwm-workspace-delete w))
   exwm-workspace--list)

  (mapc
   (lambda (i)
     (push 1 dwm-workspaces--monitor-workspace))
   dwm-workspaces--monitor-list)

  (let ((real-amount 1))
    (mapc (lambda (name)
	    (let ((display-list `(,name))
		  (real-list `(,name)))
	      (setq display-list (append display-list (number-sequence 1 dwm-workspaces--workspaces-per-monitor)))
	      (setq real-list (append real-list (number-sequence real-amount (+ real-amount dwm-workspaces--workspaces-per-monitor))))
	      (setq real-amount (+ real-amount dwm-workspaces--workspaces-per-monitor 1))
	      
	      (push display-list dwm-workspaces--workspaces-display )
	      (push real-list dwm-workspaces--workspaces-real)))
	  dwm-workspaces--monitor-list))

  (setq exwm-workspace-number (+ (* (safe-length dwm-workspaces--monitor-list) dwm-workspaces--workspaces-per-monitor) 1))

  (mapc (lambda (real-workspaces)
	  (let ((monitor (car real-workspaces))
		(workspaces (cdr real-workspaces))
		(monitor-workspaces-list '()))
	    (mapc (lambda (i)
		    (exwm-workspace-add i)
		    (setq monitor-workspaces-list (append monitor-workspaces-list `(,i ,monitor))))
		  workspaces)
	    (setq exwm-randr-workspace-monitor-plist (append exwm-randr-workspace-monitor-plist monitor-workspaces-list))))
	dwm-workspaces--workspaces-real)

  (setq dwm-workspaces--workspaces-real (reverse dwm-workspaces--workspaces-real)
	dwm-workspaces--workspaces-display (reverse dwm-workspaces--workspaces-display)))

(defun dwm-workspaces--get-monitor-list ()
  "Get the list of monitors using xrandr."
  (mapcar (lambda (monitor-string)
	    (car (last (split-string monitor-string " "))))
	  (butlast
	   (cdr (split-string
		 (shell-command-to-string "xrandr --listactivemonitors")
		 "\n")))))

(defun dwm-workspaces--focus-changed ()
  "This function will run on `focus-in-hook'."
  (let ((index exwm-workspace-current-index)
	(idx 0))
    (mapc (lambda (list)
	    (when (-contains-p list index)
	      (setq dwm-workspaces--current-monitor idx))
	    (setq idx (+ 1 idx)))
	  dwm-workspaces--workspaces-real)))

(defun dwm-workspaces--get-current-real-workspace-by-monitor-index (monitor-index)
  "Return the stored real EXWM workspace number for the monitor MONITOR-INDEX."
  (nth (- (nth monitor-index dwm-workspaces--monitor-workspace) 1)
       (cdr (nth monitor-index dwm-workspaces--workspaces-real))))

;;;###autoload
(defun dwm-workspaces--select-from-current-monitor-by-index (index)
  "Return the EXWM workspace that corresponds to INDEX on the current monitor."
  (nth (- index 1)
       (cdr
	(nth
	 dwm-workspaces--current-monitor
	 dwm-workspaces--workspaces-real))))

;;;###autoload
(defun dwm-workspaces--select-next-monitor ()
  "Focus the next monitor, wrap around if you are on the last monitor."
  (interactive)
  (setq dwm-workspaces--current-monitor (+ dwm-workspaces--current-monitor 1))
  (when (eq dwm-workspaces--current-monitor (safe-length dwm-workspaces--monitor-list))
    (setq dwm-workspaces--current-monitor 0))
  (exwm-workspace-switch (dwm-workspaces--get-current-real-workspace-by-monitor-index dwm-workspaces--current-monitor)))

;;;###autoload
(defun dwm-workspaces--select-previous-monitor ()
  "Focus the previous monitor, wrap around if you are on the first monitor."
  (interactive)
  (setq dwm-workspaces--current-monitor (- dwm-workspaces--current-monitor 1))
  (when (eq dwm-workspaces--current-monitor -1)
    (setq dwm-workspaces--current-monitor (- (safe-length dwm-workspaces--monitor-list) 1)))
  (exwm-workspace-switch (dwm-workspaces--get-current-real-workspace-by-monitor-index dwm-workspaces--current-monitor)))

;;;###autoload
(defun dwm-workspaces--switch-by-index (index)
  "Switch to workspace INDEX on the currently selected monitor.
This is meant to be bound to a key."
  (let ((list-cdr (nthcdr dwm-workspaces--current-monitor dwm-workspaces--monitor-workspace))
	(list-car (take dwm-workspaces--current-monitor dwm-workspaces--monitor-workspace)))
    (setcar list-cdr index)
    (setq dwm-workspaces--monitor-workspace (append list-car list-cdr)))
  (exwm-workspace-switch (dwm-workspaces--select-from-current-monitor-by-index index)))

;;;###autoload
(defun dwm-workspaces--move-window-by-index (index)
  "Move the current window to workspace INDEX on the currently selected monitor.
This is meant to be bound to a key."
  (exwm-workspace-move-window (dwm-workspaces--select-from-current-monitor-by-index index)))

(provide 'dwm-workspaces)
;;; dwm-workspaces.el ends here
