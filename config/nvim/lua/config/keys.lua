local keys = require("which-key")

local function indentation(name, setter)
	return {
		name = name .. " indentation",
		["2"] = {"<cmd>" .. setter .." expandtab shiftwidth=2 tabstop=2<cr>", "2-space"},
		["4"] = {"<cmd>" .. setter .." expandtab shiftwidth=4 tabstop=4<cr>", "4-space"},
		t = {"<cmd>" .. setter .." noexpandtab shiftwidth=4 tabstop=4<cr>", "small tabs"},
		T = {"<cmd>" .. setter .." noexpandtab shiftwidth=8 tabstop=8<cr>", "big tabs"},
	}
end

local function setup()
	keys.register({
		Y = {"0vg_\"+y", "Copy Line To Clipboard"}
	})

	keys.register({
		a = {
			name = "lsp",
			x = {"<cmd>LspStart<cr>", "Start LSP server"},
		},
		f = {
			name = "find",
			b = {"<cmd>Telescope buffers<cr>", "Buffer"},
			c = {"<cmd>Telescope commands<cr>", "Command"},
			f = {"<cmd>Telescope find_files<cr>", "File"},
			F = {"<cmd>Telescope git_files<cr>", "Git File"},
			g = {"<cmd>Telescope live_grep<cr>", "Live Grep"},
			m = {"<cmd>Telescope git_status<cr>", "Git Modified File"},
			s = {"<cmd>Telescope treesitter<cr>", "Symbol"},
			r = {"<cmd>Telescope grep_string<cr>", "String"},
			p = {"<cmd>Telescope planets<cr>", "Planet"},
		},
		x = indentation("buffer", "setlocal"),
		X = indentation("global", "set"),
	}, {
		prefix = "<leader>",
	})

	for _, mode in ipairs({"n", "v"}) do
		keys.register({
			y = {"\"+y", "Copy To Clipboard"},
		}, {
			mode = mode,
			prefix = "<leader>",
		})
	end
end

return {
	setup = setup,
}
