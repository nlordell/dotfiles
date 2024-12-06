local set = require("utils.set")
local keys = require("which-key")

keys.add({
	buffer = vim.api.nvim_get_current_buf(),
	{"<leader>d", group = "dune"},
	{"<leader>dF", "<cmd>!dune fmt<cr>", desc = "Format Project"},
	{"<leader>dT", "<cmd>!dune test<cr>", desc = "Run Project Testt"},
	{"<leader>db", "<cmd>!dune build<cr>", desc = "Build Project"},
})

set.locl {
	expandtab = true,
	shiftwidth = 2,
	tabstop = 2,
}
