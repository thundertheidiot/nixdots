((nil . ((eglot-workspace-configuration .
					(:nixd (:options (:nixos (:expr
								  "(builtins.getFlake (builtins.toString ./.)).nixosConfigurations.desktop.options"))
							 (:home-manager (:expr
									 "(builtins.getFlake (builtins.ToString ./.)).nixosConfigurations.desktop.options.home-manager.users.type.getSubOptions []"))))))))
