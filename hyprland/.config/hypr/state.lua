-- Runtime state shared between the toggle scripts and the config.
-- Reads ~/.config/hypr/state (KEY=VALUE lines, maintained by
-- toggle-darkmode / toggle-ecomode); falls back to defaults when missing.

local defaults = {
  theme = "gruvbox", -- gruvbox | flatwhite | black
  powermode = "normal", -- normal | eco
}

local state = {}
for key, value in pairs(defaults) do
  state[key] = value
end

local path = os.getenv("HOME") .. "/.config/hypr/state"
local file = io.open(path, "r")
if file then
  for line in file:lines() do
    local key, value = line:match("^%s*([%w_]+)%s*=%s*(%S+)%s*$")
    if key == "theme" or key == "powermode" then
      state[key] = value
    end
  end
  file:close()
end

return state
