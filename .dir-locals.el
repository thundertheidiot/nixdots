;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

((nix-mode . ((lsp-nix-nixd-home-manager-options-expr . "(builtins.getFlake (builtins.toString ./.)).nixosConfigurations.desktop.options.home-manager.users.type.getSubOptions []")
			  (lsp-nix-nixd-nixos-options-expr . "(builtins.getFlake (builtins.toString ./.)).nixosConfigurations.desktop.options"))))
