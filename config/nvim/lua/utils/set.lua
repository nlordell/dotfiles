local function scoped_setter(scopes)
	return function (properties)
		for _, s in ipairs(scopes) do
			for k, v in pairs(properties) do
				s[k] = v
			end
		end
	end
end

return {
	vars = scoped_setter({vim.g}),
	opts = scoped_setter({vim.opt}),
	locl = scoped_setter({vim.opt_local}),
}
