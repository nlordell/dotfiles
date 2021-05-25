local utils = require("utils")

utils.vars {
	loaded_python_provider = 0,
	loaded_python3_provider = 0,
	loaded_perl_provider = 0,
	loaded_ruby_provider = 0,
	loaded_node_provider = 0,
}

require("packer").startup(function (use)
	use "wbthomason/packer.nvim"

	use "andymass/vim-matchup"
	use {
		"folke/todo-comments.nvim",
		requires = "nvim-lua/plenary.nvim",
	}
	use "folke/lsp-colors.nvim"
	use {
		"folke/tokyonight.nvim",
		config = function ()
			vim.g.tokyonight_colors = {
				bg_dark = "#121212",
				bg = "#1e1e1e",
				bg_highlight = "#242424",
			}
			vim.cmd [[colorscheme tokyonight]]
		end,
	}
	use {
		"folke/which-key.nvim",
		config = [[require("which-key").setup()]],
	}
	use {
		"hoob3rt/lualine.nvim",
		requires = "kyazdani42/nvim-web-devicons",
		config = function ()
			require("lualine").setup {
				options = {
					theme = "tokyonight",
					section_separators = "",
					component_separators = "",
				},
			}
		end
	}
	use {
		"neovim/nvim-lspconfig",
		config = [[require("config.lsp").setup()]],
	}
	use {
		"nvim-telescope/telescope.nvim",
		requires = {
			"kyazdani42/nvim-web-devicons",
			"nvim-lua/popup.nvim",
			"nvim-lua/plenary.nvim",
		},
	}
	use {
		"nvim-treesitter/nvim-treesitter",
		config = function ()
			require("nvim-treesitter.configs").setup {
				ensure_installed = "maintained",
				highlight = {enable = true},
			}
		end,
	}
	use "sheerun/vim-polyglot"
	use "tpope/vim-commentary"
	use "tpope/vim-surround"

	-- TODO: some other plugins to try out
	--[=[
	use "dense-analysis/ale"
	use "folke/trouble.nvim"
	use "glepnir/lspsaga.nvim"
	use "junegunn/vim-peekaboo"
	use "lukas-reineke/indent-blankline.nvim",
	use "neovim/nvim-lspconfig"
	use "nvim-lua/lsp-status.nvim"
	use "nvim-lua/completion-nvim"
	use "onsails/lspkind-nvim"
	]=]--
end)

-- customization
utils.vars {
	mapleader = ",",
	maplocalleader = ",",
}
utils.opts {
	inccommand = "nosplit",
	showmode = false,
	smarttab = true,
}
utils.wopts {
	number = true,
	relativenumber = true,
	list = true,
	listchars = "tab:» ,nbsp:␣,trail:·",
}
utils.bopts {
	autoindent = true,
	copyindent = true,
	expandtab = false,
	tabstop = 4,
	shiftwidth = 4,
}

-- TODO: make this prettier.
vim.cmd [[call matchadd('ColorColumn', '\%81v.')]]

require("which-key").register({
	f = {
		name = "find",
		b = {"<cmd>Telescope buffers<cr>", "Buffer"},
		c = {"<cmd>Telescope commands<cr>", "Command"},
		f = {"<cmd>Telescope find_files<cr>", "File"},
		g = {"<cmd>Telescope git_files<cr>", "Git File"},
		m = {"<cmd>Telescope git_status<cr>", "Git Modified File"},
		s = {"<cmd>Telescope treesitter<cr>", "Symbol"},
		w = {"<cmd>Telescope grep_string<cr>", "Word"},
		p = {"<cmd>Telescope planets<cr>", "Planet"},
		["/"] = {"<cmd>Telescope live_grep<cr>", "Search"},
	},
}, {prefix = "<leader>"})

-- TODO(nlordell): final touches
--[[
 * configure LSP signs and tweak keymaps:
   <https://github.com/wbthomason/dotfiles/blob/linux/neovim/.config/nvim/lua/config/lsp.lua>
 * configure colours:
   <https://github.com/wbthomason/dotfiles/blob/linux/neovim/.config/nvim/plugin/colors.vim>
 * configure a colour scheme.
]]--
