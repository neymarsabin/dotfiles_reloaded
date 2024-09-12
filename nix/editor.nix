{ config, pkgs, ... }: 

{ 
	programs.neovim = { 
		enable = true;
	};

	home.file."${config.home.homeDirectory}/.config/nvim".source = ../dots/nvim;
}
