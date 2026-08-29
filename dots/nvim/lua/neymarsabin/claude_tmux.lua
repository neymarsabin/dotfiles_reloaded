-- send text from neovim to the claude code pane in the same tmux window
local M = {}

-- look only at the window this neovim lives in: the claude pane is the one
-- sitting beside us, and a match from any other window would paste text
-- somewhere off screen
local function find_claude_pane()
	if not vim.env.TMUX then
		return nil, "not running inside tmux"
	end

	local self_pane = vim.env.TMUX_PANE or ""
	if self_pane == "" then
		return nil, "$TMUX_PANE is unset"
	end

	-- -t scopes to our window; a bare list-panes reports the active window,
	-- which is the wrong one whenever nvim sits in a background window
	local cmd = "tmux list-panes -t " .. vim.fn.shellescape(self_pane) .. " -F '#{pane_id}\t#{pane_title}'"
	local lines = vim.fn.systemlist(cmd)

	if vim.v.shell_error ~= 0 then
		return nil, "this window no longer exists (stale $TMUX_PANE " .. self_pane .. ")"
	end

	local others = {}

	for _, line in ipairs(lines) do
		local id, title = line:match("^(%S+)\t(.*)$")
		if id and id ~= self_pane then
			-- claude code marks its terminal title with a leading ✳, while
			-- pane_current_command reports its version string, not "claude"
			if title:find("✳", 1, true) then
				return id
			end
			table.insert(others, id)
		end
	end

	-- two-pane layout: the only other pane is the one we want
	if #others == 1 then
		return others[1]
	end

	return nil, ("no Claude pane among %d in this window"):format(#lines)
end

--- @param text string
function M.send(text)
	if text == "" then
		vim.notify("Nothing to send", vim.log.levels.WARN)
		return
	end

	local pane, why = find_claude_pane()
	if not pane then
		vim.notify("Send to Claude failed: " .. why, vim.log.levels.WARN)
		return
	end

	-- load-buffer from stdin avoids shell quoting entirely, -p pastes in
	-- bracketed mode so multi-line text does not submit line by line
	vim.fn.system({ "tmux", "load-buffer", "-b", "nvim-claude", "-" }, text)
	vim.fn.system({ "tmux", "paste-buffer", "-d", "-p", "-b", "nvim-claude", "-t", pane })
	vim.fn.system({ "tmux", "select-pane", "-t", pane })
end

-- send the current visual selection, called while still in visual mode
function M.send_selection()
	local lines = vim.fn.getregion(vim.fn.getpos("v"), vim.fn.getpos("."), { type = vim.fn.mode() })
	M.send(table.concat(lines, "\n"))
end

-- send whatever is on the system clipboard, copied from anywhere
function M.send_clipboard()
	M.send(vim.fn.getreg("+"))
end

return M
