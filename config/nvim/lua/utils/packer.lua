local function startup(...)
	local ok, packer = pcall(require, "packer")
	if not ok then
		print("packer not installed")
		return nil
	end

	return packer.startup(...)
end

return {
	startup = startup,
}
