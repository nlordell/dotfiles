local function scoped_vars(scopes)
	return function (vars)
		for _, s in ipairs(scopes) do
			for k, v in pairs(vars) do
				s[k] = v
			end
		end
	end
end

local function scoped_opts(scopes)
	return function (options)
		for _, s in ipairs(scopes) do
			for k, v in pairs(options) do
				s[k] = v
			end
		end
	end
end

return {
	vars = scoped_vars({vim.g}),
	opts = scoped_opts({vim.o}),
	bopts = scoped_opts({vim.o, vim.bo}),
	bbopts = scoped_opts({vim.bo}),
	wopts = scoped_opts({vim.o, vim.wo}),
}
