local lspconfig = require("lspconfig")
local keys = require("which-key")

local function on_attach(client, buffer)
	vim.bo[buffer].omnifunc = "v:lua.vim.lsp.omnifunc"

	keys.add({
		buffer = buffer,
		{"<leader>aa", "<cmd>lua vim.lsp.buf.code_action()<cr>", desc = "Actions"},
		{"<leader>ad", "<cmd>lua vim.lsp.buf.definition()<cr>", desc = "Declaration"},
		{"<leader>aD", "<cmd>Telescope lsp_definitions<cr>", desc = "Definition"},
		{"<leader>ae", "<cmd>lua vim.diagnostic.open_float()<cr>", desc = "Show Diagnostic"},
		{"<leader>af", "<cmd>lua vim.lsp.buf.format{async=true}<cr>", desc = "Format"},
		{"<leader>ai", "<cmd>Telescope lsp_implementations<cr>", desc = "Implementation"},
		{"<leader>ak", "<cmd>lua vim.lsp.buf.hover()<cr>", desc = "Hover"},
		{"<leader>aK", "<cmd>lua vim.lsp.buf.signature_help()<cr>", desc = "Help"},
		{"<leader>al", "<cmd>Telescope diagnostics bufnr="..buffer.."<cr>", desc = "Document Diagnostics"},
		{"<leader>aL", "<cmd>Telescope diagnostics<cr>", desc = "Diagnostics"},
		{"<leader>an", "<cmd>lua vim.lsp.buf.rename()<cr>", desc = "Rename"},
		{"<leader>ar", "<cmd>Telescope lsp_references<cr>", desc = "References"},
		{"<leader>as", "<cmd>Telescope lsp_document_symbols<cr>", desc = "Document Symbols"},
		{"<leader>aS", "<cmd>Telescope lsp_workspace_symbols<cr>", desc = "Workspace Symbols"},
		{"<leader>a[", "<cmd>lua vim.diagnostic.goto_prev()<cr>", desc = "Previous Diagnostic" },
		{"<leader>a]", "<cmd>lua vim.diagnostic.goto_next()<cr>", desc = "Next Diagnostic" },
	})
end

local function setup()
	lspconfig.rust_analyzer.setup {
		autostart = false,
		on_attach = on_attach,
		settings = {
			["rust-analyzer.checkOnSave.command"] = "clippy",
		}
	}

	lspconfig.ocamllsp.setup {
		autostart = false,
		on_attach = on_attach,
	}

	lspconfig.denols.setup {
		autostart = false,
		on_attach = on_attach,
		init_options = {
			lint = true,
		},
	}
end

return {
	setup = setup,
}
