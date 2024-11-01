local set = require("utils.set")
local keys = require("which-key")

keys.add({
	buffer = vim.api.nvim_get_current_buf(),
	{"<leader>d", group = "deno"},
	{"<leader>dF", "<cmd>!deno fmt<cr>", desc = "Format Workspace"},
	{"<leader>df", "<cmd>!deno fmt %<cr>", desc = "Format File"},
	{"<leader>dr", "<cmd>!deno run %<cr>", desc = "Run"},
})

set.locl {
	expandtab = true,
	shiftwidth = 2,
	tabstop = 2,
}
