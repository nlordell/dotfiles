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
	}, {
		prefix = "<leader>",
	})
end

return {
	setup = setup,
}
