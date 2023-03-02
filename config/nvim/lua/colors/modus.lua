-- Modus Vivendi port to Neovim/Lua.
-- <https://protesilaos.com/emacs/modus-themes>

local set = require("utils.set")

local vivendi = {
--- Basic value

	["bg-main"] = "#000000",
	["bg-dim"] = "#1e1e1e",
	["fg-main"] = "#ffffff",
	["fg-dim"] = "#989898",
	["fg-alt"] = "#c6daff",
	["bg-active"] = "#535353",
	["bg-inactive"] = "#303030",
	["border"] = "#646464",

--- Common accent foregrounds

	["red"] = "#ff5f59",
	["red-warmer"] = "#ff6b55",
	["red-cooler"] = "#ff7f9f",
	["red-faint"] = "#ff9580",
	["red-intense"] = "#ff5f5f",
	["green"] = "#44bc44",
	["green-warmer"] = "#70b900",
	["green-cooler"] = "#00c06f",
	["green-faint"] = "#88ca9f",
	["green-intense"] = "#44df44",
	["yellow"] = "#d0bc00",
	["yellow-warmer"] = "#fec43f",
	["yellow-cooler"] = "#dfaf7a",
	["yellow-faint"] = "#d2b580",
	["yellow-intense"] = "#efef00",
	["blue"] = "#2fafff",
	["blue-warmer"] = "#79a8ff",
	["blue-cooler"] = "#00bcff",
	["blue-faint"] = "#82b0ec",
	["blue-intense"] = "#338fff",
	["magenta"] = "#feacd0",
	["magenta-warmer"] = "#f78fe7",
	["magenta-cooler"] = "#b6a0ff",
	["magenta-faint"] = "#caa6df",
	["magenta-intense"] = "#ff66ff",
	["cyan"] = "#00d3d0",
	["cyan-warmer"] = "#4ae2f0",
	["cyan-cooler"] = "#6ae4b9",
	["cyan-faint"] = "#9ac8e0",
	["cyan-intense"] = "#00eff0",

--- Uncommon accent foregrounds

	["rust"] = "#db7b5f",
	["gold"] = "#c0965b",
	["olive"] = "#9cbd6f",
	["slate"] = "#76afbf",
	["indigo"] = "#9099d9",
	["maroon"] = "#cf7fa7",
	["pink"] = "#d09dc0",

--- Common accent backgrounds

	["bg-red-intense"] = "#9d1f1f",
	["bg-green-intense"] = "#2f822f",
	["bg-yellow-intense"] = "#7a6100",
	["bg-blue-intense"] = "#1640b0",
	["bg-magenta-intense"] = "#7030af",
	["bg-cyan-intense"] = "#2266ae",

	["bg-red-subtle"] = "#620f2a",
	["bg-green-subtle"] = "#00422a",
	["bg-yellow-subtle"] = "#4a4000",
	["bg-blue-subtle"] = "#242679",
	["bg-magenta-subtle"] = "#552f5f",
	["bg-cyan-subtle"] = "#004065",

	["bg-red-nuanced"] = "#2c0614",
	["bg-green-nuanced"] = "#001904",
	["bg-yellow-nuanced"] = "#221000",
	["bg-blue-nuanced"] = "#0f0e39",
	["bg-magenta-nuanced"] = "#230631",
	["bg-cyan-nuanced"] = "#041529",

--- Uncommon accent backgrounds

	["bg-ochre"] = "#442c2f",
	["bg-lavender"] = "#38325c",
	["bg-sage"] = "#0f3d30",

--- Graphs

	["bg-graph-red-0"] = "#b52c2c",
	["bg-graph-red-1"] = "#702020",
	["bg-graph-green-0"] = "#4fd100",
	["bg-graph-green-1"] = "#007800",
	["bg-graph-yellow-0"] = "#f1e00a",
	["bg-graph-yellow-1"] = "#b08600",
	["bg-graph-blue-0"] = "#2fafef",
	["bg-graph-blue-1"] = "#1f2f8f",
	["bg-graph-magenta-0"] = "#bf94fe",
	["bg-graph-magenta-1"] = "#5f509f",
	["bg-graph-cyan-0"] = "#47dfea",
	["bg-graph-cyan-1"] = "#00808f",

--- Special purpose

	["bg-completion"] = "#2f447f",
	["bg-hover"] = "#004f70",
	["bg-hover-secondary"] = "#654a39",
	["bg-hl-line"] = "#2f3849",
	["bg-region"] = "#5a5a5a",
	["fg-region"] = "#ffffff",

	["bg-char-0"] = "#0050af",
	["bg-char-1"] = "#7f1f7f",
	["bg-char-2"] = "#625a00",

	["bg-mode-line-active"] = "#505050",
	["fg-mode-line-active"] = "#ffffff",
	["border-mode-line-active"] = "#959595",
	["bg-mode-line-inactive"] = "#2d2d2d",
	["fg-mode-line-inactive"] = "#969696",
	["border-mode-line-inactive"] = "#606060",

	["modeline-err"] = "#ffa9bf",
	["modeline-warning"] = "#dfcf43",
	["modeline-info"] = "#9fefff",

	["bg-tab-bar"] = "#313131",
	["bg-tab-current"] = "#000000",
	["bg-tab-other"] = "#545454",

--- Diffs

	["bg-added"] = "#00381f",
	["bg-added-faint"] = "#002910",
	["bg-added-refine"] = "#034f2f",
	["bg-added-fringe"] = "#237f3f",
	["fg-added"] = "#a0e0a0",
	["fg-added-intense"] = "#80e080",

	["bg-changed"] = "#363300",
	["bg-changed-faint"] = "#2a1f00",
	["bg-changed-refine"] = "#4a4a00",
	["bg-changed-fringe"] = "#8a7a00",
	["fg-changed"] = "#efef80",
	["fg-changed-intense"] = "#c0b05f",

	["bg-removed"] = "#4f1119",
	["bg-removed-faint"] = "#380a0f",
	["bg-removed-refine"] = "#781a1f",
	["bg-removed-fringe"] = "#b81a1f",
	["fg-removed"] = "#ffbfbf",
	["fg-removed-intense"] = "#ff9095",

	["bg-diff-context"] = "#1a1a1a",

--- Paren match

	["bg-paren-match"] = "#2f7f9f",
	["bg-paren-expression"] = "#453040",
}

local function highlighter(palette)
	return function(name, val)
		-- TODO: Automatically approximate RGB to ANSI-256
		for _,f in ipairs({ "bg", "fg", "sp" }) do
			if val[f] ~= nil then
				local n = val[f]
				local c = palette[n]
				assert(c ~= nil, "unknown color '" .. n .. "'")
				val[f] = c
			else
				val[f] = "NONE"
			end
		end
		vim.api.nvim_set_hl(0, name, val)
	end
end

local function setup(palette)
	vim.cmd [[
		highlight clear
	]]

	set.vars {
		colors_name = "modus-vivendi",
	}

	set.opts {
		background = "dark",
		termguicolors = true,
	}

	local hl = highlighter(palette)

	hl("ColorColumn", { fg = "fg-main", bg = "bg-active" })
	hl("Conceal", { fg = "olive" }) -- TODO
	hl("CurSearch", { fg = "fg-main", bg = "bg-yellow-intense" })
	-- hl("Cursor", { ... })
	-- hl("lCursor", { ... })
	-- hl("CursorIM", { ... })
	-- hl("CursorColumn", { ... })
	hl("CursorLine", { bg = "bg-hl-line" })
	hl("Directory", { fg = "blue-cooler" })
	hl("DiffAdd", { fg = "fg-added", bg = "bg-added" })
	hl("DiffChange", { fg = "fg-changed", bg = "bg-changed" })
	hl("DiffDelete", { fg = "fg-removed", bg = "bg-removed" })
	hl("DiffText", { fg = "fg-changed", bg = "bg-changed-refine" })
	hl("EndOfBuffer", { fg = "bg-inactive" })
	hl("TermCursor", { fg = "bg-main", bg = "fg-main" })
	hl("TermCursorNC", { fg = "bg-main", bg = "fg-dim" })
	hl("ErrorMsg", { fg = "red" })
	hl("WinSeparator", { fg = "border", bg = "bg-dim" })
	hl("Folded", { bg = "bg-inactive" })
	-- hl("FoldColumn", { ... })
	hl("SignColumn", { bg = "bg-dim" })
	hl("IncSearch", { fg = "fg-main", bg = "bg-yellow-subtle" })
	hl("Substitute", { fg = "fg-main", bg = "bg-red-intense" })
	hl("LineNr", { fg = "fg-dim", bg = "bg-dim" })
	hl("CursorLineNr", { fg = "fg-main", bg = "bg-active" })
	-- hl("LineNrAbove", { ... })
	-- hl("LineNrBelow", { ... })
	-- hl("CursorLineSign", { ... })
	-- hl("CursorLineFold", { ... })
	hl("MatchParen", { bg = "bg-paren-match" })
	hl("ModeMsg", { fg = "fg-main" }) -- TODO
	hl("MsgArea", { fg = "fg-main", bg = "bg-main" })
	hl("MsgSeparator", { fg = "bg-active" }) -- TODO
	hl("MoreMsg", { fg = "cyan-cooler" })
	hl("NonText", { fg = "cyan" })
	hl("Normal", { fg = "fg-main", bg = "bg-main" })
	-- hl("NormalFloat", { ... })
	-- hl("NormalNC", { ... })
	hl("Pmenu", { fg = "fg-main", bg = "bg-inactive" })
	hl("PmenuSel", { bg = "bg-completion" })
	hl("PmenuSbar", { bg = "bg-dim" })
	hl("PmenuThumb", { bg = "fg-dim" })
	hl("Question", { fg = "cyan-cooler" })
	-- hl("QuickFixLine", { ... })
	hl("Search", { fg = "fg-main", bg = "bg-cyan-intense" })
	hl("SpecialKey", { fg = "olive" }) -- TODO
	hl("SpellBad", { sp = "red-faint", undercurl = true })
	hl("SpellCap", { sp = "yellow-faint", undercurl = true })
	hl("SpellLocal", { sp = "cyan-faint", undercurl = true })
	hl("SpellLocal", { sp = "cyan-faint", undercurl = true })
	hl("StatusLine", { fg = "fg-mode-line-active", bg = "bg-mode-line-active" })
	hl("StatusLineNC", { fg = "fg-mode-line-inactive", bg = "bg-mode-line-inactive" })
	hl("TabLine", { bg = "bg-tab-other" })
	hl("TabLineFill", { bg = "bg-tab-bar" })
	hl("TabLine", { bg = "bg-tab-current" })
	hl("Title", { fg = "cyan-cooler" })
	hl("Visual", { fg = "fg-region", bg = "bg-region" })
	hl("VisualNOS", { fg = "fg-main", bg = "bg-hover-secondary" })
	hl("WarningMsg", { fg = "yellow-warmer" })
	hl("Whitespace", { fg = "fg-dim" })
	hl("WildMenu", { bg = "bg-inactive" })
	hl("WinBar", { fg = "fg-main", bg = "bg-hover" })
	hl("WinBarNC", { bg = "bg-dim" })
end

return {
	vivendi = function() setup(vivendi) end,
}
