-- Theme colors, selected by state.theme.
-- toggle-darkmode writes gruvbox (dark) / flatwhite (light),
-- toggle-ecomode writes black (OLED) while eco mode is active.

local state = require("state")

local themes = {
  gruvbox = { -- dark (default)
    active_border = "rgba(458588ee)",
    inactive_border = "rgba(504945aa)",
    shadow_color = "rgba(1d2021ee)",
    background = "rgb(282828)",
  },
  flatwhite = { -- light
    active_border = "rgba(6a4dffee)",
    inactive_border = "rgba(dcd3c6aa)",
    shadow_color = "rgba(93836c66)",
    background = "rgb(f7f3ee)",
  },
  black = { -- OLED, used by eco mode
    active_border = "rgba(458588ee)",
    inactive_border = "rgba(504945aa)",
    shadow_color = "rgba(000000ee)",
    background = "rgb(000000)",
  },
}

local t = themes[state.theme] or themes.gruvbox

hl.config({
  general = {
    col = {
      active_border = t.active_border,
      inactive_border = t.inactive_border,
    },
  },
  decoration = {
    shadow = { color = t.shadow_color },
  },
  misc = {
    background_color = t.background,
  },
})
