local set = require("utils.set")
local keys = require("which-key")

keys.add({
	buffer = vim.api.nvim_get_current_buf(),
	{"<leader>af", "<cmd>exec 'w' | exec 'silent !deno fmt % --prose-wrap never' | redraw!<cr>", desc = "Format File"},
})

set.locl {
	expandtab = true,
	shiftwidth = 2,
	tabstop = 2,
}
