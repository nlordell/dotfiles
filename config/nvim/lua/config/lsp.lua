local lspconfig = require("lspconfig")

local function on_attach(client, buffer)
	vim.bo[buffer].omnifunc = "v:lua.vim.lsp.omnifunc"

	require("which-key").register({
		a = {
			name = "lsp",
			a = {"<cmd>Telescope lsp_code_actions<cr>", "Actions"},
			d = {"<cmd>lua vim.lsp.buf.definition()<cr>", "Declaration"},
			D = {"<cmd>Telescope lsp_definitions<cr>", "Definition"},
			e = {"<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<cr>", "Show Diagnostic"},
			f = {"<cmd>lua vim.lsp.buf.formatting()<cr>", "Format"},
			i = {"<cmd>Telescope lsp_implementations<cr>", "Implementation"},
			k = {"<cmd>lua vim.lsp.buf.hover()<cr>", "Hover"},
			K = {"<cmd>lua vim.lsp.buf.signature_help()<cr>", "Help"},
			l = {"<cmd>Telescope lsp_document_diagnostics<cr>", "Document Diagnostics"},
			L = {"<cmd>Telescope lsp_workspace_diagnostics<cr>", "Workspace Diagnostics"},
			n = {"<cmd>lua vim.lsp.buf.rename()<cr>", "Rename"},
			r = {"<cmd>Telescope lsp_references<cr>", "References"},
			s = {"<cmd>Telescope lsp_document_symbols<cr>", "Document Symbols"},
			S = {"<cmd>Telescope lsp_workspace_symbols<cr>", "Workspace Symbols"},
			["["] = {"<cmd>lua vim.lsp.diagnostic.goto_prev()<cr>", "Previous Diagnostic" },
			["]"] = {"<cmd>lua vim.lsp.diagnostic.goto_next()<cr>", "Next Diagnostic" },
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

	if vim.env.NVIM_DENO ~= "off" then
		lspconfig.denols.setup {
			on_attach = on_attach,
			init_options = {lint = true},
		}
	else
		lspconfig.tsserver.setup {
			on_attach = function (client, buffer)
				client.resolved_capabilities.document_formatting = false
				on_attach(client, buffer)
			end
		}
		lspconfig.diagnosticls.setup {
			filetypes = {"javascript", "typescript", "typescriptreact"},
			init_options = {
				linters = {
					eslint = {
						sourceName = "eslint",
						rootPatterns = {"package.json"},
						command = "./node_modules/.bin/eslint",
						args = {"--stdin", "--stdin-filename", "%filepath", "--format", "json"},
						debounce = 100,
						parseJson = {
							errorsRoot = "[0].messages",
							line = "line",
							column = "column",
							endLine = "endLine",
							endColumn = "endColumn",
							message = "${message} [${ruleId}]",
							security = "severity",
						},
						securities = {
							[2] = "error",
							[1] = "warning",
						},
					},
				},
				filetypes = {
					javascript = "eslint",
					typescript = "eslint",
					typescriptreact = "eslint",
				},
				formatters = {
					prettier = {
						rootPatterns = {"package.json"},
						command = "./node_modules/.bin/prettier",
						args = {"--stdin-filepath", "%filepath"},
					},
				},
				formatFiletypes = {
					javascript = "prettier",
					typescript = "prettier",
					typescriptreact = "prettier",
				},
			},
		}
	end
end

return {setup = setup}
