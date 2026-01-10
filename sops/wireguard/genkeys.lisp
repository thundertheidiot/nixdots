#!/usr/bin/env -S sbcl --script

(require :asdf)
(asdf:load-system :shasht)

(defun wg (command &optional stdin)
  (let ((stdin (if stdin
		   (make-string-input-stream stdin)
		   nil)))
    (uiop:run-program
     `("wg" ,command)
     :input stdin
     :output '(:string :stripped t))))

(defun make-keys (host)
  (let* ((private-key (wg "genkey"))
	 (public-key (wg "pubkey" private-key)))
    (list
     (cons :name host)
     (cons :private-key private-key)
     (cons :public-key public-key))))

(defmacro write-var (name)
  `(with-open-file (out (pathname ,(format nil "~A.json" (string-downcase (symbol-name name))))
			:direction :output
			:if-exists :supersede
			:if-does-not-exist :create)
     (shasht:write-json* ,name
			 :stream out
			 :alist-as-object t)))

(defun geta (key item)
  (cdr (assoc key item)))

(defun format-for-json (key list)
  (mapcar (lambda (host)
	    (cons (geta :name host) (geta key host)))
	  list))

(let* ((hosts (mapcar #'make-keys '("vps" "home" "home2" "framework" "desktop")))
       (private-keys (format-for-json :private-key hosts))
       (public-keys (format-for-json :public-key hosts)))
  (write-var private-keys)
  (write-var public-keys))

(uiop:run-program
 `("sops" "--in-place" "--encrypt" "private-keys.json")
 :output t
 :error-output t)

(with-open-file (out #p"preshared-key"
		     :direction :output
		     :if-exists :supersede
		     :if-does-not-exist :create)
  (format out (wg "genpsk")))

(uiop:run-program
 `("sops" "--in-place" "--encrypt" "--output-type" "binary" "preshared-key")
 :output t
 :error-output t)
