local set = require("utils.set")
local keys = require("which-key")

keys.register({
	c = {
		name = "cargo",
		b = {"<cmd>!cargo build<cr>", "Build"},
		c = {"<cmd>!cargo clippy<cr>", "Check"},
		C = {"<cmd>!cargo clippy --workspace --all-features --all-targets<cr>", "Check Workspace"},
		F = {"<cmd>!cargo fmt --all<cr>", "Format Workspace"},
		r = {"<cmd>!cargo run<cr>", "Run"},
		t = {"<cmd>!cargo test<cr>", "Test"},
		T = {"<cmd>!cargo test --workspace --all-features<cr>", "Test Workspace"},
	}
}, {
	prefix = "<leader>",
})

set.opts {
	expandtab = true,
}
