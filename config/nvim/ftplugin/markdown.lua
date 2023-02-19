local set = require("utils.set")
local keys = require("which-key")

keys.register({
	a = {
		name = "lsp (emulation)",
		f = {"<cmd>exec 'w' | exec 'silent !deno fmt %' | redraw!<cr>", "Format File"},
	}
}, {
	prefix = "<leader>",
	buffer = vim.api.nvim_get_current_buf(),
})

set.locl {
	expandtab = true,
	shiftwidth = 2,
	tabstop = 2,
}
