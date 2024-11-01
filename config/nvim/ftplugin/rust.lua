local set = require("utils.set")
local keys = require("which-key")

keys.add({
	buffer = vim.api.nvim_get_current_buf(),
	{"<leader>c", group = "cargo"},
	{"<leader>cb", "<cmd>!cargo build<cr>", desc = "Build"},
	{"<leader>cc", "<cmd>!cargo clippy<cr>", desc = "Check"},
	{"<leader>cC", "<cmd>!cargo clippy --workspace --all-features --all-targets<cr>", desc = "Check Workspace"},
	{"<leader>cF", "<cmd>!cargo fmt --all<cr>", desc = "Format Workspace"},
	{"<leader>cr", "<cmd>!cargo run<cr>", desc = "Run"},
	{"<leader>ct", "<cmd>!cargo test<cr>", desc = "Test"},
	{"<leader>cT", "<cmd>!cargo test --workspace --all-features<cr>", desc = "Test Workspace"},
})

set.locl {
	expandtab = true,
	shiftwidth = 4,
	tabstop = 4,
	spell = true,
	spellcapcheck = "",
}
