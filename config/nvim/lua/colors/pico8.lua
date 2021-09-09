local palette = {
	black = "#000000",
	dark_blue = "#1d2b53",
	dark_purple = "#7e2553",
	dark_green = "#008751",
	brown = "#ab5236",
	dark_grey = "#5f574f",
	light_grey = "#c2c3c7",
	white = "#fff1e8",
	red = "#ff004d",
	orange = "#ffa300",
	yellow = "#ffec27",
	green = "#00e436",
	blue = "#29adff",
	indigo = "#83769c",
	pink = "#ff77a8",
	peach = "#ffccaa",
}

function colorscheme()
	
end

function terminal()
	vim.g.terminal_color_0 = palette.black
	vim.g.terminal_color_8 = palette.dark_grey

	vim.g.terminal_color_1 = palette.red
	vim.g.terminal_color_9 = palette.red

	vim.g.terminal_color_2 = palette.green
	vim.g.terminal_color_10 = palette.green

	vim.g.terminal_color_3 = palette.orange
	vim.g.terminal_color_11 = palette.orange

	vim.g.terminal_color_4 = palette.blue
	vim.g.terminal_color_12 = palette.blue

	vim.g.terminal_color_5 = palette.pink
	vim.g.terminal_color_13 = palette.pink

	vim.g.terminal_color_6 = palette.indigo
	vim.g.terminal_color_14 = palette.indigo

	vim.g.terminal_color_7 = palette.white
	vim.g.terminal_color_15 = palette.white
end

return {
	palette = palette,
	colorscheme = colorscheme,
}
