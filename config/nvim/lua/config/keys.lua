local keys = require("which-key")

local function indentation(prefix, name, setter)
	return {
		{prefix, group = name .. " indentation"},
		{prefix .. "2", "<cmd>" .. setter .." expandtab shiftwidth=2 tabstop=2<cr>", desc = "2-space"},
		{prefix .. "4", "<cmd>" .. setter .." expandtab shiftwidth=4 tabstop=4<cr>", desc = "4-space"},
		{prefix .. "t", "<cmd>" .. setter .." noexpandtab shiftwidth=4 tabstop=4<cr>", desc = "small tabs"},
		{prefix .. "T", "<cmd>" .. setter .." noexpandtab shiftwidth=8 tabstop=8<cr>", desc = "big tabs"},
	}
end

local function setup()
	keys.add({
		{"<leader>?", group = "help"},
		{"<leader>??", function () keys.show({ global = false }) end, desc = "Buffer Local Keymaps"},
		{"<leader>?g", function () keys.show() end, desc = "All Keymaps"},
	})

	keys.add({
		{"<leader>a", group = "lsp"},
		{"<leader>ax", "<cmd>LspStart<cr>", desc = "Start LSP server"},
	})

	keys.add({
		{"<leader>f", group = "find" },
		{"<leader>fb", "<cmd>Telescope buffers<cr>", desc = "Buffer"},
		{"<leader>fc", "<cmd>Telescope commands<cr>", desc = "Command"},
		{"<leader>ff", "<cmd>Telescope find_files<cr>", desc = "File"},
		{"<leader>fF", "<cmd>Telescope git_files<cr>", desc = "Git File"},
		{"<leader>fg", "<cmd>Telescope live_grep<cr>", desc = "Live Grep"},
		{"<leader>fm", "<cmd>Telescope git_status<cr>", desc = "Git Modified File"},
		{"<leader>fs", "<cmd>Telescope treesitter<cr>", desc = "Symbol"},
		{"<leader>fr", "<cmd>Telescope grep_string<cr>", desc = "String"},
		{"<leader>fp", "<cmd>Telescope planets<cr>", desc = "Planet"},
	})

	keys.add(indentation("<leader>x", "buffer", "setlocal"))
	keys.add(indentation("<leader>X", "global", "set"))

	keys.add({
		{"Y", "0vg_\"+y", desc = "Copy Line To Clipboard"},
	})
	for _, mode in ipairs({"n", "v"}) do
		keys.add({
			mode = mode,
			{"<leader>y", "\"+y", desc = "Copy To Clipboard"},
		})
	end
end

return {
	setup = setup,
}
