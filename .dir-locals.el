((nix-mode .
	   ((eval
	     . (setq-local eglot-workspace-configuration
			   `(:nixd (:options
				    (:nixos (:expr ,(format "(builtins.getFlake \"%s\").nixosConfigurations.desktop.options"
							    (or (projectile-project-root)
								(vc-root-dir)
								default-directory)))))))))))
