local utils = require("utils")

if vim.env.NVIM_DENO == "off" then
	require("which-key").register({
		y = {
			name = "yarn",
			b = {"<cmd>!yarn build<cr>", "Build"},
			t = {"<cmd>!yarn test<cr>", "Test"},
		}
	}, {prefix = "<leader>"})
end

utils.bbopts {
	expandtab = true,
	shiftwidth = 2,
}
