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
		-- TODO(nlordell): Automatically approximate RGB to ANSI-256
		for _,f in ipairs({ "bg", "fg", "sp" }) do
			if val[f] ~= nil then
				local n = val[f]
				local c = palette[n]
				assert(c ~= nil, "unknown color '" .. n .. "'")
				val[f] = c
			end
		end
		vim.api.nvim_set_hl(0, name, val)
	end
end

local function setup(palette)
	vim.cmd [[
		if exists("colors_name")
			highlight clear
		endif
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
--[[
Conceal		Placeholder characters substituted for concealed
		text (see 'conceallevel').
							*hl-CurSearch*
CurSearch	Used for highlighting a search pattern under the cursor
		(see 'hlsearch').
]]
	-- Cursor does nothing, cause we don't manually specify highlight groups
--[[
							*hl-Cursor*
Cursor		Character under the cursor.
lCursor		Character under the cursor when |language-mapping|
		is used (see 'guicursor').
							*hl-CursorIM*
CursorIM	Like Cursor, but used when in IME mode. *CursorIM*
]]

--[[
							*hl-CursorColumn*
CursorColumn	Screen-column at the cursor, when 'cursorcolumn' is set.
]]
	hl("CursorLine", { bg = "bg-hl-line" })
--[[
							*hl-Directory*
Directory	Directory names (and other special names in listings).
							*hl-DiffAdd*
DiffAdd		Diff mode: Added line. |diff.txt|
							*hl-DiffChange*
DiffChange	Diff mode: Changed line. |diff.txt|
							*hl-DiffDelete*
DiffDelete	Diff mode: Deleted line. |diff.txt|
							*hl-DiffText*
DiffText	Diff mode: Changed text within a changed line. |diff.txt|
							*hl-EndOfBuffer*
EndOfBuffer	Filler lines (~) after the end of the buffer.
		By default, this is highlighted like |hl-NonText|.
							*hl-TermCursor*
TermCursor	Cursor in a focused terminal.
							*hl-TermCursorNC*
TermCursorNC	Cursor in an unfocused terminal.
							*hl-ErrorMsg*
ErrorMsg	Error messages on the command line.
							*hl-WinSeparator*
WinSeparator	Separators between window splits.
							*hl-Folded*
Folded		Line used for closed folds.
							*hl-FoldColumn*
FoldColumn	'foldcolumn'
							*hl-SignColumn*
SignColumn	Column where |signs| are displayed.
							*hl-IncSearch*
IncSearch	'incsearch' highlighting; also used for the text replaced with
		":s///c".
							*hl-Substitute*
Substitute	|:substitute| replacement text highlighting.

]]
	hl("LineNr", { fg = "fg-dim", bg = "bg-dim" })
	hl("CursorLineNr", { fg = "fg-main", bg = "bg-active" })
--[[
LineNrAbove	Line number for when the 'relativenumber'
		option is set, above the cursor line.
							*hl-LineNrBelow*
LineNrBelow	Line number for when the 'relativenumber'
		option is set, below the cursor line.
							*hl-CursorLineNr*
CursorLineSign	Like SignColumn when 'cursorline' is set for the cursor line.
							*hl-CursorLineFold*
CursorLineFold	Like FoldColumn when 'cursorline' is set for the cursor line.
]]

--[[
							*hl-MatchParen*
MatchParen	Character under the cursor or just before it, if it
		is a paired bracket, and its match. |pi_paren.txt|

							*hl-ModeMsg*
ModeMsg		'showmode' message (e.g., "-- INSERT --").
							*hl-MsgArea*
MsgArea		Area for messages and cmdline.
							*hl-MsgSeparator*
MsgSeparator	Separator for scrolled messages, `msgsep` flag of 'display'.
							*hl-MoreMsg*
MoreMsg		|more-prompt|
							*hl-NonText*
NonText		'@' at the end of the window, characters from 'showbreak'
		and other characters that do not really exist in the text
		(e.g., ">" displayed when a double-wide character doesn't
		fit at the end of the line). See also |hl-EndOfBuffer|.
]]
	hl("Normal", { fg = "fg-main", bg = "bg-main" })
--[[
NormalFloat	Normal text in floating windows.
							*hl-NormalNC*
NormalNC	Normal text in non-current windows.
							*hl-Pmenu*
Pmenu		Popup menu: Normal item.
							*hl-PmenuSel*
PmenuSel	Popup menu: Selected item.
							*hl-PmenuSbar*
PmenuSbar	Popup menu: Scrollbar.
							*hl-PmenuThumb*
PmenuThumb	Popup menu: Thumb of the scrollbar.
							*hl-Question*
Question	|hit-enter| prompt and yes/no questions.
							*hl-QuickFixLine*
QuickFixLine	Current |quickfix| item in the quickfix window. Combined with
                |hl-CursorLine| when the cursor is there.
							*hl-Search*
Search		Last search pattern highlighting (see 'hlsearch').
		Also used for similar items that need to stand out.
							*hl-SpecialKey*
SpecialKey	Unprintable characters: Text displayed differently from what
		it really is. But not 'listchars' whitespace. |hl-Whitespace|
							*hl-SpellBad*
SpellBad	Word that is not recognized by the spellchecker. |spell|
		Combined with the highlighting used otherwise.
							*hl-SpellCap*
SpellCap	Word that should start with a capital. |spell|
		Combined with the highlighting used otherwise.
							*hl-SpellLocal*
SpellLocal	Word that is recognized by the spellchecker as one that is
		used in another region. |spell|
		Combined with the highlighting used otherwise.
							*hl-SpellRare*
SpellRare	Word that is recognized by the spellchecker as one that is
		hardly ever used. |spell|
		Combined with the highlighting used otherwise.
							*hl-StatusLine*
StatusLine	Status line of current window.
							*hl-StatusLineNC*
StatusLineNC	Status lines of not-current windows.
		Note: If this is equal to "StatusLine", Vim will use "^^^" in
		the status line of the current window.
							*hl-TabLine*
TabLine		Tab pages line, not active tab page label.
							*hl-TabLineFill*
TabLineFill	Tab pages line, where there are no labels.
							*hl-TabLineSel*
TabLineSel	Tab pages line, active tab page label.
							*hl-Title*
Title		Titles for output from ":set all", ":autocmd" etc.
							*hl-Visual*
Visual		Visual mode selection.
							*hl-VisualNOS*
VisualNOS	Visual mode selection when vim is "Not Owning the Selection".
							*hl-WarningMsg*
WarningMsg	Warning messages.
							*hl-Whitespace*
Whitespace	"nbsp", "space", "tab", "multispace", "lead" and "trail"
		in 'listchars'.
							*hl-WildMenu*
WildMenu	Current match in 'wildmenu' completion.
							*hl-WinBar*
WinBar		Window bar of current window.
							*hl-WinBarNC*
WinBarNC	Window bar of not-current windows.
]]
end

return {
	vivendi = function() setup(vivendi) end,
}
