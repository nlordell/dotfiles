local keys = require("which-key")

local function setup()
	keys.register({
		Y = {"0vg_\"+y", "Copy Line To Clipboard"}
	})

	keys.register({
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
		x = {
			name = "indentation",
			["2"] = {"<cmd>set expandtab shiftwidth=2 tabstop=2<cr>", "2-space"},
			["4"] = {"<cmd>set expandtab shiftwidth=4 tabstop=4<cr>", "4-space"},
			t = {"<cmd>set noexpandtab shiftwidth=4 tabstop=4<cr>", "small tabs"},
			T = {"<cmd>set noexpandtab shiftwidth=8 tabstop=8<cr>", "big tabs"},
		},
	}, {
		prefix = "<leader>",
	})
end

return {
	setup = setup,
}
