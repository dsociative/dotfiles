# Keybindings Reference
## Niri (config.kdl)

### Apps & Actions
| Key               | Action                          |
|-------------------|---------------------------------|
| `Mod+Q`           | Terminal (alacritty)            |
| `Mod+R`           | Launcher (fuzzel)               |
| `Mod+S`           | Screenshot (grim+slurp+wl-copy) |
| `Mod+N`           | Toggle darkmode                 |
| `Mod+O`           | Toggle overview                 |
| `Mod+C`           | Close window                    |
| `Mod+Shift+Slash` | Hotkey overlay                  |
| `Super+Alt+L`     | Lock screen (swaylock)          |
| `Super+Alt+S`     | Toggle screen reader (orca)     |
| `Mod+Shift+E`     | Quit (confirm)                  |
| `Ctrl+Alt+Delete` | Quit                            |
| `Mod+Shift+P`     | Power off monitors              |
| `Mod+Escape`      | Toggle shortcuts inhibitor      |
| `Print`           | Screenshot                      |
| `Ctrl+Print`      | Screenshot screen               |
| `Alt+Print`       | Screenshot window               |

### Media Keys
| Key                     | Action          |
|-------------------------|-----------------|
| `XF86AudioRaiseVolume`  | Volume up       |
| `XF86AudioLowerVolume`  | Volume down     |
| `XF86AudioMute`         | Toggle mute     |
| `XF86AudioMicMute`      | Toggle mic mute |
| `XF86MonBrightnessUp`   | Brightness up   |
| `XF86MonBrightnessDown` | Brightness down |

### Navigation (arrows / vim HJKL)
| Key                 | Action               |
|---------------------|----------------------|
| `Mod+H/Left`        | Focus column left    |
| `Mod+J/Down`        | Focus window down    |
| `Mod+K/Up`          | Focus window up      |
| `Mod+L/Right`       | Focus column right   |
| `Mod+Home`          | Focus first column   |
| `Mod+End`           | Focus last column    |
| `Mod+U / Page_Down` | Focus workspace down |
| `Mod+I / Page_Up`   | Focus workspace up   |
| `Mod+1..9`          | Focus workspace N    |

### Move Windows
| Key                      | Action                        |
|--------------------------|-------------------------------|
| `Mod+Ctrl+H/Left`        | Move column left              |
| `Mod+Ctrl+J/Down`        | Move window down              |
| `Mod+Ctrl+K/Up`          | Move window up                |
| `Mod+Ctrl+L/Right`       | Move column right             |
| `Mod+Ctrl+Home`          | Move column to first          |
| `Mod+Ctrl+End`           | Move column to last           |
| `Mod+Ctrl+U / Page_Down` | Move column to workspace down |
| `Mod+Ctrl+I / Page_Up`   | Move column to workspace up   |
| `Mod+Shift+1..9`         | Move column to workspace N    |

### Monitor Navigation
| Key                      | Action                       |
|--------------------------|------------------------------|
| `Mod+Shift+H/Left`       | Focus monitor left           |
| `Mod+Shift+J/Down`       | Focus monitor down           |
| `Mod+Shift+K/Up`         | Focus monitor up             |
| `Mod+Shift+L/Right`      | Focus monitor right          |
| `Mod+Shift+Ctrl+H/Left`  | Move column to monitor left  |
| `Mod+Shift+Ctrl+J/Down`  | Move column to monitor down  |
| `Mod+Shift+Ctrl+K/Up`    | Move column to monitor up    |
| `Mod+Shift+Ctrl+L/Right` | Move column to monitor right |

### Workspace Reorder
| Key                       | Action              |
|---------------------------|---------------------|
| `Mod+Shift+U / Page_Down` | Move workspace down |
| `Mod+Shift+I / Page_Up`   | Move workspace up   |

### Layout
| Key                | Action                      |
|--------------------|-----------------------------|
| `Mod+D`            | Switch preset column width  |
| `Mod+F`            | Maximize column             |
| `Mod+Shift+F`      | Fullscreen window           |
| `Mod+Ctrl+F`       | Expand to available width   |
| `Mod+T`            | Center column               |
| `Mod+Ctrl+C`       | Center visible columns      |
| `Mod+V`            | Toggle floating             |
| `Mod+Shift+V`      | Switch focus float/tiling   |
| `Mod+W`            | Toggle tabbed display       |
| `Mod+Comma`        | Consume window into column  |
| `Mod+Period`       | Expel window from column    |
| `Mod+BracketLeft`  | Consume/expel left          |
| `Mod+BracketRight` | Consume/expel right         |
| `Mod+Minus`        | Column width -10%           |
| `Mod+Equal`        | Column width +10%           |
| `Mod+Shift+Minus`  | Window height -10%          |
| `Mod+Shift+Equal`  | Window height +10%          |
| `Mod+Shift+R`      | Switch preset window height |
| `Mod+Ctrl+R`       | Reset window height         |

### Mouse (with Mod)
| Key                        | Action                   |
|----------------------------|--------------------------|
| `Mod+WheelDown/Up`         | Focus workspace down/up  |
| `Mod+Ctrl+WheelDown/Up`    | Move column to workspace |
| `Mod+WheelRight/Left`      | Focus column right/left  |
| `Mod+Ctrl+WheelRight/Left` | Move column right/left   |

---

## Emacs

### Global (init.el)
| Key     | Command                    |
|---------|----------------------------|
| `C-x .` | Open init.el               |
| `C-x ?` | Open ~/study/docs/todo.org |

### Core (config.el)
| Key             | Command                      |
|-----------------|------------------------------|
| `C-=`           | er/expand-region             |
| `C-M-Backspace` | goto-last-change             |
| `C-:`           | avy-goto-char-2              |
| `M-Up`          | move-dup-move-lines-up       |
| `C-M-Up`        | move-dup-duplicate-up        |
| `C-c >`         | indent-tools-hydra           |
| `F8`            | treemacs                     |
| `F6`            | consult-flycheck             |
| `M-s r`         | consult-ripgrep              |
| `M-N`           | consult-imenu                |
| `M-p p`         | consult-projectile           |
| `C-.`           | embark-act                   |
| `C-;`           | embark-dwim                  |
| `C-h B`         | embark-bindings              |
| `C-\`           | comment-or-uncomment-region  |
| `ESC`           | → C-g (keyboard-escape-quit) |

### LSP (lsp.el)
| Key            | Command                      |
|----------------|------------------------------|
| `C-c l r`      | eglot-reconnect              |
| `C-c l f`      | eglot-format                 |
| `C-c l a`      | eglot-code-actions           |
| `C-c l n`      | flymake-goto-next-error      |
| `C-c l p`      | flymake-goto-prev-error      |
| `gd` (go-mode) | xref-find-definitions        |
| `f` (normal)   | consult-projectile-find-file |
| `F` (go-mode)  | go-goto-function             |
| `R` (go-mode)  | go-goto-return-values        |

### SPC Leader (unstable.el, motion state)
| Key     | Command                           |
|---------|-----------------------------------|
| `` ` `` | consult-buffer                    |
| `SPC p` | consult-projectile-switch-project |
| `SPC v` | projectile-vc                     |
| `SPC b` | consult-buffer                    |
| `SPC h` | evil-window-left                  |
| `SPC j` | evil-window-down                  |
| `SPC k` | evil-window-up                    |
| `SPC l` | evil-window-right                 |
| `SPC o` | consult-eglot-symbols             |
| `SPC r` | xref-find-references              |
| `SPC n` | eglot-rename                      |
| `SPC t` | org-roam-dailies-goto-today       |
| `SPC w` | org-roam-node-find                |
| `SPC e` | org-roam-capture                  |
| `SPC m` | eglot-find-implementation         |
| `SPC a` | eglot-code-actions                |

### Combobulate (unstable.el, normal state)
| Key            | Command                                 |
|----------------|-----------------------------------------|
| `] f`          | combobulate-navigate-next               |
| `[ f`          | combobulate-navigate-previous           |
| `g K`          | combobulate-navigate-up                 |
| `g J`          | combobulate-navigate-down               |
| `] d`          | combobulate-navigate-end-of-defun       |
| `[ d`          | combobulate-navigate-beginning-of-defun |
| `g v`          | combobulate-mark-node-dwim              |
| `, m`          | combobulate                             |
| `g c`          | combobulate-clone-node-dwim             |
| `g d`          | combobulate-kill-node-dwim              |
| `g x`          | combobulate-splice-node-dwim            |
| `g r / g R`    | combobulate-drag-down / drag-up         |
| `g h` (visual) | combobulate-mark-node-dwim              |

### Other (unstable.el)
| Key       | Command              |
|-----------|----------------------|
| `C-c m`   | smerge prefix        |
| `C-c c`   | hydra/smerge         |
| `C-x C-n` | dired-sidebar-toggle |
| `C-c o`   | combobulate prefix   |

---

## Waybar (config.jsonc)
| Action           | Handler     |
|------------------|-------------|
| PulseAudio click | pavucontrol |

---

## Occupied Mod+ Keys (Niri)
C, D, F, H, I, J, K, L, N, O, Q, R, S, T, U, V, W, 1-9,
Comma, Period, Minus, Equal, BracketLeft, BracketRight,
Home, End, Escape
