local lspconfig = require("lspconfig")
local keys = require("which-key")

local function on_attach(client, buffer)
	vim.bo[buffer].omnifunc = "v:lua.vim.lsp.omnifunc"

	keys.register({
		a = {
			name = "lsp",
			a = {"<cmd>lua vim.lsp.buf.code_action()<cr>", "Actions"},
			d = {"<cmd>lua vim.lsp.buf.definition()<cr>", "Declaration"},
			D = {"<cmd>Telescope lsp_definitions<cr>", "Definition"},
			e = {"<cmd>lua vim.diagnostic.open_float()<cr>", "Show Diagnostic"},
			f = {"<cmd>lua vim.lsp.buf.formatting()<cr>", "Format"},
			i = {"<cmd>Telescope lsp_implementations<cr>", "Implementation"},
			k = {"<cmd>lua vim.lsp.buf.hover()<cr>", "Hover"},
			K = {"<cmd>lua vim.lsp.buf.signature_help()<cr>", "Help"},
			l = {"<cmd>Telescope diagnostics bufnr="..buffer.."<cr>", "Document Diagnostics"},
			L = {"<cmd>Telescope diagnostics<cr>", "Diagnostics"},
			n = {"<cmd>lua vim.lsp.buf.rename()<cr>", "Rename"},
			r = {"<cmd>Telescope lsp_references<cr>", "References"},
			s = {"<cmd>Telescope lsp_document_symbols<cr>", "Document Symbols"},
			S = {"<cmd>Telescope lsp_workspace_symbols<cr>", "Workspace Symbols"},
			["["] = {"<cmd>lua vim.diagnostic.goto_prev()<cr>", "Previous Diagnostic" },
			["]"] = {"<cmd>lua vim.diagnostic.goto_next()<cr>", "Next Diagnostic" },
		}
	}, {
		prefix = "<leader>",
		buffer = buffer,
	})
end

local function setup()
	lspconfig.rust_analyzer.setup{
		on_attach = on_attach,
		settings = {
			["rust-analyzer.checkOnSave.command"] = "clippy",
		}
	}

	lspconfig.denols.setup {
		on_attach = on_attach,
		init_options = {
			lint = true,
		},
	}
end

return {
	setup = setup,
}
