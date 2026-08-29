-- inline diff overlay for code claude changed, driven by its Stop hook
--
-- renders in the buffer itself rather than a side-by-side view: changed lines
-- get a background highlight, deleted lines come back as red virtual lines,
-- and word_diff narrows it to the characters that actually moved
local M = {}

local enabled = false

local function socket_path()
	local root = vim.fs.root(vim.fn.getcwd(), ".git") or vim.fn.getcwd()
	local dir = vim.fn.stdpath("cache") .. "/claude-diff"
	vim.fn.mkdir(dir, "p")
	return dir .. "/" .. (root:gsub("[^%w]", "_")) .. ".sock"
end

local function set(state)
	local ok, gs = pcall(require, "gitsigns")
	if not ok then
		return false
	end

	gs.toggle_linehl(state)
	gs.toggle_deleted(state)
	gs.toggle_word_diff(state)
	enabled = state
	return true
end

function M.show()
	-- claude wrote to disk behind us, so reload before diffing
	vim.cmd("checktime")

	if set(true) then
		vim.notify("Claude edits — ]h / [h to walk hunks, <leader>hv to hide", vim.log.levels.INFO)
	end
end

function M.hide()
	set(false)
end

function M.toggle()
	set(not enabled)
end

-- listen on a per-repo socket so the Stop hook can call show() from outside
function M.listen()
	local sock = socket_path()

	if vim.fn.filereadable(sock) == 1 or vim.fn.getftype(sock) == "socket" then
		-- a dead nvim leaves its socket behind; claim it only if nothing answers
		local chan = pcall(vim.fn.sockconnect, "pipe", sock, {})
		if not chan then
			vim.fn.delete(sock)
		else
			return
		end
	end

	pcall(vim.fn.serverstart, sock)
end

return M
