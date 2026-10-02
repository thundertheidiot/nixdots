#!/usr/bin/env -S sbcl --script
;; overengineered must be stripped down

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

(defun geta (key item)
  (cdr (assoc key item)))

(defun format-for-json (key list)
  (mapcar (lambda (host)
			(cons (geta :name host) (geta key host)))
		  list))

(defun read-keys (directory)
  (let* ((public (shasht:read-json
                  (uiop:read-file-string (merge-pathnames "public-keys.json" directory))
                  t nil t))
         (private (shasht:read-json
                   (uiop:run-program
                    `("sops" "--decrypt" ,(namestring (merge-pathnames "private-keys.json" directory)))
                    :output :string :error-output t)
                   t nil t)))
    (unless (and (hash-table-p public) (hash-table-p private)
                 (= (hash-table-count public) (hash-table-count private)))
      (error "Public and private keys must be objects with identical host names."))
    (sort
     (loop for name being the hash-keys of public
           for pubkey = (gethash name public)
           for privkey = (gethash name private)
           collect
           (progn
             (unless (and (stringp pubkey) (stringp privkey)
                          (string= (string-trim '(#\Space #\Tab #\Newline #\Return) pubkey)
                                   (wg "pubkey" privkey)))
               (error "Key pair for ~A is missing or inconsistent." name))
             (list (cons :name name)
                   (cons :private-key privkey)
                   (cons :public-key pubkey))))
     #'string< :key (lambda (host) (geta :name host)))))

(defun write-keys (directory hosts)
  (let* ((public (merge-pathnames "public-keys.json" directory))
         (private (merge-pathnames "private-keys.json" directory))
         (temporary (uiop:ensure-directory-pathname
                     (uiop:run-program
                      `("mktemp" "-d" ,(namestring (merge-pathnames ".genkeys.XXXXXX" directory)))
                      :output '(:string :stripped t))))
         (staged-public (merge-pathnames "public.json" temporary))
         (staged-private (merge-pathnames "private.json" temporary))
         (replacing nil)
         (committed nil))
    (unwind-protect
        (progn
          (uiop:copy-file public (merge-pathnames "public.backup" temporary))
          (uiop:copy-file private (merge-pathnames "private.backup" temporary))
          ;; Private keys go straight to SOPS through stdin, never to a plaintext file.
          (with-open-file (out staged-private :direction :output :if-exists :error)
						  (uiop:run-program
						   `("sops" "--encrypt" "--input-type" "json" "--output-type" "json"
							 "--filename-override" ,(namestring private) "/dev/stdin")
						   :input (make-string-input-stream
								   (shasht:write-json (cons :object-alist (format-for-json :private-key hosts)) nil))
						   :output out :error-output t))
          (with-open-file (out staged-public :direction :output :if-exists :error)
						  (shasht:write-json* (cons :object-alist (format-for-json :public-key hosts))
											  :stream out :pretty t))
          (setf replacing t)
          (uiop:rename-file-overwriting-target staged-private private)
          (uiop:rename-file-overwriting-target staged-public public)
          (setf committed t))
      ;; Two renames are not crash-atomic; read-keys detects interrupted updates.
      (when (and replacing (not committed))
        (uiop:rename-file-overwriting-target (merge-pathnames "private.backup" temporary) private)
        (uiop:rename-file-overwriting-target (merge-pathnames "public.backup" temporary) public))
      (uiop:delete-directory-tree temporary :validate t))))

(defun key-command (directory command name)
  (let* ((hosts (read-keys directory))
         (host (find name hosts :test #'equal :key (lambda (host) (geta :name host)))))
    (cond
     ((string= command "list")
      (dolist (host hosts) (format t "~A~%" (geta :name host))))
     (t
      (when (or (zerop (length name))
                (not (every (lambda (char) (or (find char "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
                                               (find char "-_."))) name)))
        (error "Host names must contain only letters, digits, hyphens, underscores or dots."))
      (cond
       ((string= command "add")
        (when host (error "Host ~A already exists; use rotate to replace its keys." name))
        (setf hosts (append hosts (list (make-keys name)))))
       ((string= command "remove")
        (unless host (error "Unknown host ~A." name))
        (setf hosts (remove host hosts)))
       ((string= command "rotate")
        (unless host (error "Unknown host ~A." name))
        (setf hosts (substitute (make-keys name) host hosts))))
      (write-keys directory hosts)
      (format t "~A: ~A~%" command name)))))

(defun main (args)
  (let ((directory (if (probe-file "sops/wireguard/") #p"sops/wireguard/" #p"./")))
    (when (equal (first args) "--directory")
      (unless (second args) (error "Expected a directory after --directory."))
      (setf directory (uiop:ensure-directory-pathname (second args))
            args (cddr args)))
    (unless (or (equal args '("list"))
                (and (= (length args) 2)
                     (member (first args) '("add" "remove" "rotate") :test #'equal)))
      (error "Usage: wireguard-keys [--directory PATH] list | add HOST | remove HOST | rotate HOST"))
    (setf directory (truename directory))
    (let ((lock (merge-pathnames ".genkeys.lock/" directory)))
      (unless (zerop (nth-value 2 (uiop:run-program `("mkdir" ,(namestring lock))
													:ignore-error-status t :error-output nil)))
        (error "Another key operation is running, or ~A is a stale lock." lock))
      (unwind-protect
          (key-command directory (first args) (second args))
        (uiop:delete-directory-tree lock :validate t)))))

(handler-case (main (uiop:command-line-arguments))
  (error (condition)
		 (format *error-output* "~A~%" condition)
		 (uiop:quit 1)))
