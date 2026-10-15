# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

Personal dotfiles repository for an Arch Linux setup, managed with GNU Stow. Each top-level directory is a stow package — run `stow <package>` from `~/dotfiles` to symlink into `$HOME`.

## Structure

Repository uses GNU Stow layout. Each package mirrors the home directory hierarchy:

- `emacs/.emacs.d/` - Emacs init.el and modular config split into feature files
- `kitty/.config/kitty/` - Kitty terminal config with dual color themes
- `gnupg/.gnupg/` - GPG agent config
- `gtk/.themes/` - Gruvbox/Flatwhite GTK3 themes
- `hyprland/.config/hypr/` - Hyprland Wayland compositor config
- `waybar/.config/waybar/` - Waybar status bar config and styling
- `scripts/.local/bin/` - Shell scripts for theme switching and dependency installation

## Emacs Modules

The Emacs config is split by concern in `emacs/.emacs.d/config/`. Each file provides a named feature loaded from `init.el`:

- **config.el** - Core settings, UI, evil mode, keybindings, completion (vertico/orderless/consult/embark), org-mode with babel, projectile, treemacs
- **lsp.el** - Eglot for Rust (with rust-analyzer/clippy), go-mode with goimports, evil keybindings for navigation
- **ai.el** - gptel with Claude/DeepSeek backends, MCP hub integration
- **unstable.el** - Experimental: terraform, k8s, combobulate (structural editing), treesit grammars, org-publish, org-crypt, reverse-im (Russian keyboard support), SPC-leader evil keybindings

## Hyprland

Wayland compositor (scrolling layout). Dual 4K monitors (DP-1, DP-2) at scale 1.5. Keyboard layout: us,ru with Win+Space toggle. Vim-style navigation (Mod+HJKL). Key binds: Mod+Q terminal (kitty), Mod+R launcher (fuzzel), Mod+S screenshot (grim+slurp), Mod+F fullscreen, Mod+C close, Mod+N toggle darkmode, Mod+B eco mode. Config is Lua (`hyprland.lua`, required since Hyprland 0.55; `.conf` removed in 0.57): `powermode.lua` sets monitors and `theme.lua` sets colors based on `~/.config/hypr/state` (runtime file, KEY=VALUE: `theme=gruvbox|flatwhite|black`, `powermode=normal|eco`), read via `state.lua`. Spawns waybar at startup.

## Waybar

Top bar with modules: custom/windows, language indicator (pulsing red on RU), custom/ecomode (🍃 in eco mode), pulseaudio, cpu, memory, temperature, clock, tray. Font: CaskaydiaCove Nerd Font (see the CSS). Uses `hyprland/language`.

`custom/windows` replaces `hyprland/workspaces` + `hyprland/window`: it is fed by `scripts/.local/bin/waybar-windows` (one instance per bar, Hyprland IPC sockets, no polling) and shows this output's workspaces (bold pills, the active one inverted) with a short label per window (18 chars max, full titles in the tooltip; scroll switches workspaces). Kitty labels are derived from the window title, which kitty shell integration / Claude Code / remote shells already maintain: `󰚩` Claude Code busy, orange `󰚩` Claude waiting for input, `󰣀` ssh, dimmed `` shell at prompt, `` running command; the dimmed suffix is the shell's cwd read from `/proc`. Colors come from the `style.css` symlink target (flatwhite → light palette, otherwise dark).

## Scripts

- **toggle-darkmode** - Toggles system-wide dark/light theme: dconf, kitty, emacs, waybar, hyprland, GTK theme
- **toggle-ecomode** - Toggles eco power mode (Mod+B): 60Hz monitors, CPU/GPU power caps, dark theme
- **install-deps** - Installs all Arch Linux packages required by these configs (pacman + AUR)
- **waybar-windows** - Waybar `custom/windows` module: Hyprland workspaces with per-window status labels (python3, stdlib only)

## Key Conventions

- Evil mode (vim keybindings) is the primary editing paradigm
- Font: Zed Mono Extended / ZedMono Nerd Font Mono
- Two color schemes: doom-gruvbox (dark) and doom-flatwhite (light), toggled via `scripts/.local/bin/toggle-darkmode`
- Tab width: 2 spaces, no tabs
- UTF-8 everywhere
- Russian keyboard layout support via reverse-im

## Installation

```bash
cd ~/dotfiles
install-deps  # or: scripts/.local/bin/install-deps
```

## Tests

`python -m unittest discover tests` — unit tests for `waybar-windows` (pure `label`/`render` functions; no waybar or Hyprland needed).

## Maintaining install-deps

When adding new external tools or dependencies to any config, update `scripts/.local/bin/install-deps` accordingly.

## Credentials

API keys (Anthropic, DeepSeek, Google) are stored in `~/.authinfo` and read via `auth-source` in `emacs/.emacs.d/config/ai.el`.

## Private Config

Machine-specific and sensitive configs live in `~/.config/dotfiles-private/` (not tracked in git). Scripts and Emacs configs load files from there when present.

Files:
- `forge.el` — Emacs Lisp, loaded by Emacs at startup. Corporate GitLab/GitHub hosts for forge, org-mode settings, or anything environment-specific

Format convention: shell configs use `KEY=value`, Emacs configs use `.el` files with standard Lisp.
