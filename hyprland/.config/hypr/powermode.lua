-- Monitor rules per power mode, selected by state.powermode.
-- toggle-ecomode switches between normal (DP-2 @ 165Hz) and eco (both @ 60Hz).

local state = require("state")

local eco = state.powermode == "eco"

hl.monitor({
  output = "DP-1",
  mode = "3840x2160@60",
  position = "2560x0",
  scale = 1.5,
})

hl.monitor({
  output = "DP-2",
  mode = eco and "3840x2160@60" or "3840x2160@164.99",
  position = "0x0",
  scale = 1.5,
})
