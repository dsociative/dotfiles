-- Hyprland config (Lua format — .conf is deprecated since 0.56.1, removed in 0.57).
-- Theme and monitor power mode are selected by ~/.config/hypr/state,
-- written by toggle-darkmode / toggle-ecomode and read via state.lua.

--########################
--### REQUIRES / STATE ###
--########################

-- Monitors (per power mode)
require("powermode")

--### MY PROGRAMS ###

local terminal = "kitty"
local menu = "fuzzel"

local mainMod = "SUPER"

--################
--### AUTOSTART ###
--################

hl.on("hyprland.start", function()
  hl.exec_cmd("waybar")
end)

--#############################
--### ENVIRONMENT VARIABLES ###
--#############################

hl.env("XCURSOR_SIZE", "24")
hl.env("HYPRCURSOR_SIZE", "24")
hl.env("STEAM_FORCE_DESKTOPUI_SCALING", "1.5")
hl.env("GBM_BACKEND", "nvidia-drm")
hl.env("__GLX_VENDOR_LIBRARY_NAME", "nvidia")
hl.env("__EGL_VENDOR_LIBRARY_FILENAMES", "/usr/share/glvnd/egl_vendor.d/10_nvidia.json")
hl.env("LIBVA_DRIVER_NAME", "nvidia")

--#####################
--### LOOK AND FEEL ###
--#####################

hl.config({
  general = {
    gaps_in = 2,
    gaps_out = 1,
    border_size = 1,

    resize_on_border = false,
    allow_tearing = false,

    layout = "scrolling",
  },

  decoration = {
    rounding = 1,

    active_opacity = 1.0,
    inactive_opacity = 1.0,

    shadow = {
      enabled = false,
      range = 4,
      render_power = 3,
    },

    blur = {
      enabled = false,
      size = 3,
      passes = 1,
      vibrancy = 0.1696,
    },
  },
})

-- Theme colors: borders, shadow, background (gruvbox/flatwhite/black)
require("theme")

-- Animation curves
hl.curve("easeOutQuint", { type = "bezier", points = { { 0.23, 1 }, { 0.32, 1 } } })
hl.curve("easeInOutCubic", { type = "bezier", points = { { 0.65, 0.05 }, { 0.36, 1 } } })
hl.curve("linear", { type = "bezier", points = { { 0, 0 }, { 1, 1 } } })
hl.curve("almostLinear", { type = "bezier", points = { { 0.5, 0.5 }, { 0.75, 1 } } })
hl.curve("quick", { type = "bezier", points = { { 0.15, 0 }, { 0.1, 1 } } })

-- Animations
hl.animation({ leaf = "global", enabled = true, speed = 10, bezier = "default" })
hl.animation({ leaf = "border", enabled = true, speed = 5.39, bezier = "easeOutQuint" })
hl.animation({ leaf = "windows", enabled = true, speed = 4.79, bezier = "easeOutQuint" })
hl.animation({ leaf = "windowsIn", enabled = true, speed = 4.1, bezier = "easeOutQuint", style = "popin 87%" })
hl.animation({ leaf = "windowsOut", enabled = true, speed = 1.49, bezier = "linear", style = "popin 87%" })
hl.animation({ leaf = "fadeIn", enabled = true, speed = 1.73, bezier = "almostLinear" })
hl.animation({ leaf = "fadeOut", enabled = true, speed = 1.46, bezier = "almostLinear" })
hl.animation({ leaf = "fade", enabled = true, speed = 3.03, bezier = "quick" })
hl.animation({ leaf = "layers", enabled = true, speed = 3.81, bezier = "easeOutQuint" })
hl.animation({ leaf = "layersIn", enabled = true, speed = 4, bezier = "easeOutQuint", style = "fade" })
hl.animation({ leaf = "layersOut", enabled = true, speed = 1.5, bezier = "linear", style = "fade" })
hl.animation({ leaf = "fadeLayersIn", enabled = true, speed = 1.79, bezier = "almostLinear" })
hl.animation({ leaf = "fadeLayersOut", enabled = true, speed = 1.39, bezier = "almostLinear" })
hl.animation({ leaf = "workspaces", enabled = true, speed = 1.94, bezier = "almostLinear", style = "fade" })
hl.animation({ leaf = "workspacesIn", enabled = true, speed = 1.21, bezier = "almostLinear", style = "fade" })
hl.animation({ leaf = "workspacesOut", enabled = true, speed = 1.94, bezier = "almostLinear", style = "fade" })

--###############
--### LAYOUTS ###
--###############

hl.config({
  scrolling = {
    column_width = 0.5,
    fullscreen_on_one_column = true,
    focus_fit_method = 1,
    follow_focus = true,
    follow_min_visible = 0.4,
    explicit_column_widths = "0.333, 0.5, 0.667, 1.0",
    direction = "right",
  },

  -- Keep dwindle/master available for per-workspace layouts
  dwindle = { preserve_split = true },
  master = { new_status = "master" },
})

--################
--### WORKSPACES ###
--################

hl.workspace_rule({ workspace = "1", monitor = "DP-2", default_name = "browser", persistent = true })
hl.workspace_rule({ workspace = "2", monitor = "DP-2", default_name = "editor", persistent = true })
hl.workspace_rule({ workspace = "3", monitor = "DP-2", default_name = "term" })
hl.workspace_rule({ workspace = "4", monitor = "DP-1", default_name = "termoff" })
hl.workspace_rule({ workspace = "9", monitor = "DP-2", default_name = "games", no_border = true, no_rounding = true })

--#############
--### MISC ###
--#############

hl.config({
  misc = {
    force_default_wallpaper = 0,
    disable_hyprland_logo = true,
    enable_anr_dialog = false,
  },

  ecosystem = {
    no_donation_nag = true,
  },

  xwayland = {
    force_zero_scaling = true,
  },
})

--#############
--### INPUT ###
--#############

hl.config({
  input = {
    kb_layout = "us,ru",
    kb_options = "grp:win_space_toggle",

    follow_mouse = 1,
    sensitivity = 0,
  },
})

--###################
--### KEYBINDINGS ###
--###################

-- Core
hl.bind(mainMod .. " + Q", hl.dsp.exec_cmd(terminal))
hl.bind(mainMod .. " + C", hl.dsp.window.close())
hl.bind(mainMod .. " + R", hl.dsp.exec_cmd(menu))
hl.bind(mainMod .. " + V", hl.dsp.window.float({ action = "toggle" }))
hl.bind(mainMod .. " + F", hl.dsp.window.fullscreen())
hl.bind(mainMod .. " + S", hl.dsp.exec_cmd('grim -g "$(slurp -d)" - | wl-copy'))
hl.bind(mainMod .. " + N", hl.dsp.exec_cmd("toggle-darkmode"))
hl.bind(mainMod .. " + B", hl.dsp.exec_cmd("toggle-ecomode"))

-- Power off monitors after a short delay (dpms should not fire directly on a keypress)
hl.bind(mainMod .. " + P", function()
  hl.timer(function()
    hl.dispatch(hl.dsp.dpms({ action = "disable" }))
  end, { timeout = 1000, type = "oneshot" })
end)
hl.bind(mainMod .. " + SHIFT + P", function()
  hl.dispatch(hl.dsp.dpms({ monitor = "DP-1", action = "enable" }))
  hl.dispatch(hl.dsp.dpms({ monitor = "DP-2", action = "enable" }))
end)
hl.bind(mainMod .. " + SHIFT + E", hl.dsp.exit())

-- Vim-style focus
hl.bind(mainMod .. " + H", hl.dsp.focus({ direction = "left" }))
hl.bind(mainMod .. " + L", hl.dsp.focus({ direction = "right" }))
hl.bind(mainMod .. " + K", hl.dsp.focus({ direction = "up" }))
hl.bind(mainMod .. " + J", hl.dsp.focus({ direction = "down" }))

-- Move active workspace to the neighbouring monitor (no wraparound)
hl.bind(mainMod .. " + SHIFT + H", hl.dsp.workspace.move({ monitor = "l" }))
hl.bind(mainMod .. " + SHIFT + L", hl.dsp.workspace.move({ monitor = "r" }))

-- Scroll layout: column resize
hl.bind(mainMod .. " + D", hl.dsp.layout("colresize +conf"))
hl.bind(mainMod .. " + SHIFT + D", hl.dsp.layout("colresize -conf"))
hl.bind(mainMod .. " + Minus", hl.dsp.layout("colresize -0.1"))
hl.bind(mainMod .. " + Equal", hl.dsp.layout("colresize +0.1"))

-- Scroll layout: center focused column
hl.bind(mainMod .. " + T", hl.dsp.layout("center"))

-- Switch workspaces with mainMod + [0-9]
-- Move active window to a workspace with mainMod + SHIFT + [0-9]
for i = 1, 10 do
  local key = i % 10 -- 10 maps to key 0
  hl.bind(mainMod .. " + " .. key, hl.dsp.focus({ workspace = i }))
  hl.bind(mainMod .. " + SHIFT + " .. key, hl.dsp.window.move({ workspace = i }))
end

-- Scroll through existing workspaces with mainMod + scroll
hl.bind(mainMod .. " + mouse_down", hl.dsp.focus({ workspace = "e+1" }))
hl.bind(mainMod .. " + mouse_up", hl.dsp.focus({ workspace = "e-1" }))

-- Move/resize windows with mainMod + LMB/RMB and dragging
hl.bind(mainMod .. " + mouse:272", hl.dsp.window.drag(), { mouse = true })
hl.bind(mainMod .. " + mouse:273", hl.dsp.window.resize(), { mouse = true })

-- Multimedia keys (locked: work on lockscreen, repeating: repeat when held)
hl.bind("XF86AudioRaiseVolume", hl.dsp.exec_cmd("wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%+"), { locked = true, repeating = true })
hl.bind("XF86AudioLowerVolume", hl.dsp.exec_cmd("wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-"), { locked = true, repeating = true })
hl.bind("XF86AudioMute", hl.dsp.exec_cmd("wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"), { locked = true, repeating = true })
hl.bind("XF86AudioMicMute", hl.dsp.exec_cmd("wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle"), { locked = true, repeating = true })
hl.bind("XF86MonBrightnessUp", hl.dsp.exec_cmd("brightnessctl s 10%+"), { locked = true, repeating = true })
hl.bind("XF86MonBrightnessDown", hl.dsp.exec_cmd("brightnessctl s 10%-"), { locked = true, repeating = true })

-- Media keys
hl.bind("XF86AudioNext", hl.dsp.exec_cmd("playerctl next"), { locked = true })
hl.bind("XF86AudioPause", hl.dsp.exec_cmd("playerctl play-pause"), { locked = true })
hl.bind("XF86AudioPlay", hl.dsp.exec_cmd("playerctl play-pause"), { locked = true })
hl.bind("XF86AudioPrev", hl.dsp.exec_cmd("playerctl previous"), { locked = true })

--##############################
--### WINDOWS AND WORKSPACES ###
--##############################

-- Steam games
hl.window_rule({
  name = "steam-fullscreen",
  match = { class = "^steam_app_\\d+$" },
  fullscreen = true,
})
hl.window_rule({
  name = "steam-monitor",
  match = { class = "^steam_app_\\d+$" },
  monitor = "DP-2",
})
hl.window_rule({
  name = "steam-workspace",
  match = { class = "^steam_app_\\d+$" },
  workspace = "9",
})

hl.window_rule({
  -- Ignore maximize requests from all apps
  name = "suppress-maximize",
  match = { class = ".*" },
  suppress_event = "maximize",
})

hl.window_rule({
  -- Fix some dragging issues with XWayland
  name = "no-focus-ghost",
  match = {
    class = "^$",
    title = "^$",
    xwayland = true,
    float = true,
    fullscreen = false,
    pin = false,
  },
  no_focus = true,
})
