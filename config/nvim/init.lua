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
		"nvim-telescope/telescope-ui-select.nvim",
		requires = "nvim-telescope/telescope.nvim",
		config = function ()
			require("telescope").load_extension("ui-select")
		end,
	}
	use {
		"nvim-treesitter/nvim-treesitter",
		config = function ()
			require("nvim-treesitter.configs").setup {
				ensure_installed = {
					"c",
					"comment",
					"fennel",
					"hare",
					"javascript",
					"lua",
					"ocaml",
					"rust",
					"solidity",
					"typescript",
				},
				highlight = {enable = true},
				matchup = {enable = true},
			}
		end,
	}
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
	use "rebelot/kanagawa.nvim"
	--]=]
end)

set.vars {
	mapleader = ",",
	maplocalleader = ",",
}

set.opts {
	number = true,
	relativenumber = true,
	cursorline = true,
	cursorlineopt = "both",
	list = true,
	listchars = {tab = "» ", nbsp = "␣", trail = "·"},
	expandtab = false,
	tabstop = 8,
	shiftwidth = 8,
}

vim.cmd [[
	" TODO: make this prettier.
	call matchadd('ColorColumn', '\%81v.')
]]
