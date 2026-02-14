# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

Personal dotfiles repository for an Arch Linux setup, managed with GNU Stow. Each top-level directory is a stow package — run `stow <package>` from `~/dotfiles` to symlink into `$HOME`.

## Structure

Repository uses GNU Stow layout. Each package mirrors the home directory hierarchy:

- `alacritty/.config/alacritty/` - Alacritty terminal config with color theme files (gruvbox dark, flatwhite light)
- `emacs/.emacs.d/` - Emacs init.el and modular config split into feature files
- `niri/.config/niri/` - Niri Wayland compositor config (KDL format)
- `waybar/.config/waybar/` - Waybar status bar config and styling
- `scripts/.local/bin/` - Shell scripts for theme switching

## Emacs Modules

The Emacs config is split by concern in `emacs/.emacs.d/config/`. Each file provides a named feature loaded from `init.el`:

- **config.el** - Core settings, UI, evil mode, keybindings, completion (vertico/orderless/consult/embark), org-mode with babel, projectile, treemacs
- **lsp.el** - Eglot for Rust (with rust-analyzer/clippy), go-mode with goimports, evil keybindings for navigation
- **ai.el** - gptel with Claude/DeepSeek backends, MCP hub integration
- **unstable.el** - Experimental: terraform, k8s, combobulate (structural editing), treesit grammars, org-publish, org-crypt, reverse-im (Russian keyboard support), SPC-leader evil keybindings

## Niri

Wayland compositor (scrollable tiling). Dual 4K monitors (DP-1, DP-2) at scale 1.5. Keyboard layout: us,ru with Win+Space toggle. Vim-style navigation (Mod+HJKL). Key binds: Mod+Q terminal (alacritty), Mod+R launcher (fuzzel), Mod+S screenshot (grim+slurp), Mod+F maximize, Mod+C close. Spawns waybar at startup.

## Waybar

Top bar with modules: workspaces, window title, language indicator (pulsing red on RU), pulseaudio, cpu, memory, temperature, clock, tray. Font: Zed Mono. Has hyprland modules left over from migration — active modules are niri-specific (`niri/language`).

## Scripts

- **toggle-darkmode** - Toggles system-wide dark/light theme: switches GNOME color-scheme (dconf), Alacritty color theme (gruvbox/flatwhite), and Emacs theme (doom-gruvbox/doom-flatwhite) via emacsclient
- **mattermost_theme_switch** - Switches Mattermost sidebar theme to match dark/light mode, reads auth tokens from Firefox cookies. Endpoint config in `~/.config/dotfiles-private/mattermost.conf`

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
stow alacritty emacs niri waybar scripts
```

## Credentials

API keys (Anthropic, DeepSeek, Google) are stored in `~/.authinfo` and read via `auth-source` in `emacs/.emacs.d/config/ai.el`.
