local packer = require("utils.packer")
local set = require("utils.set")

set.vars {
	loaded_python_provider = 0,
	loaded_python3_provider = 0,
	loaded_perl_provider = 0,
	loaded_ruby_provider = 0,
	loaded_node_provider = 0,
}

packer.startup(function (use)
	use "wbthomason/packer.nvim"

	use "andymass/vim-matchup"
	use {
		"folke/which-key.nvim",
		config = function ()
			require("which-key").setup()
			require("config.keys").setup()
		end,
	}
	use {
		"neovim/nvim-lspconfig",
		config = function ()
			require("config.lsp").setup()
		end,
	}
	use {
		"nvim-telescope/telescope.nvim",
		requires = {
			"kyazdani42/nvim-web-devicons",
			"nvim-lua/plenary.nvim",
		},
	}
	use {
		"nvim-treesitter/nvim-treesitter",
		config = function ()
			require("nvim-treesitter.configs").setup {
				ensure_installed = "maintained",
				highlight = {enable = true},
				matchup = {enable = true},
			}
		end,
	}
	use "rebelot/kanagawa.nvim"
	use "tpope/vim-commentary"
	use {
		"tpope/vim-surround",
		requires = "tpope/vim-repeat",
	}

	-- TODO: some other plugins to try out
	--[=[
	use "ggandor/lightspeed.nvim"
	use "hrsh7th/nvim-cmp"
	use "jose-elias-alvarez/null-ls.nvim"
	use "lewis6991/gitsigns.nvim"
	use "nvim-lua/lsp-status.nvim"
	use "nvim-lualine/lualine.nvim"
	use "nvim-neorg/neorg"
	use "onsails/lspkind-nvim"
	use "phaazon/hop.nvim"
	]=]--
end)

-- customization
set.vars {
	mapleader = ",",
	maplocalleader = ",",
}

set.opts {
	number = true,
	relativenumber = true,
	list = true,
	listchars = {tab = "» ", nbsp = "␣", trail = "·"},
	tabstop = 4,
	shiftwidth = 4,
}

-- TODO: make this prettier.
vim.cmd [[call matchadd('ColorColumn', '\%81v.')]]
