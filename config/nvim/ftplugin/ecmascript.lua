local set = require("utils.set")
local keys = require("which-key")

keys.register({
	d = {
		name = "deno",
		F = {"<cmd>!deno fmt<cr>", "Format Workspace"},
		r = {"<cmd>!deno run %<cr>", "Run"},
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
