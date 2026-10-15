# Keybindings Reference
## Hyprland (hyprland.lua)

### Apps & Actions
| Key           | Action                         |
|---------------|--------------------------------|
| `Mod+Q`       | Terminal (kitty)               |
| `Mod+R`       | Launcher (fuzzel)              |
| `Mod+S`       | Screenshot region (grim+slurp) |
| `Mod+C`       | Close window                   |
| `Mod+V`       | Toggle floating                |
| `Mod+F`       | Fullscreen                     |
| `Mod+N`       | Toggle darkmode                |
| `Mod+B`       | Toggle eco mode                |
| `Mod+P`       | Power off monitors (dpms off)  |
| `Mod+Shift+P` | Power on monitors (dpms on)    |
| `Mod+Shift+E` | Exit Hyprland                  |

### Navigation (vim HJKL)
| Key     | Action      |
|---------|-------------|
| `Mod+H` | Focus left  |
| `Mod+J` | Focus down  |
| `Mod+K` | Focus up    |
| `Mod+L` | Focus right |

### Layout (scrolling)
| Key           | Action                    |
|---------------|---------------------------|
| `Mod+D`       | Cycle preset column width |
| `Mod+Shift+D` | Cycle preset (reverse)    |
| `Mod+Minus`   | Column width -10%         |
| `Mod+Equal`   | Column width +10%         |
| `Mod+T`       | Center focused column     |

### Workspaces
| Key              | Action                    |
|------------------|---------------------------|
| `Mod+1..0`       | Switch to workspace 1..10 |
| `Mod+Shift+1..0` | Move window to workspace  |
| `Mod+Scroll`     | Cycle workspaces          |

### Mouse (with Mod)
| Key       | Action        |
|-----------|---------------|
| `Mod+LMB` | Move window   |
| `Mod+RMB` | Resize window |

### Media Keys
| Key                     | Action          |
|-------------------------|-----------------|
| `XF86AudioRaiseVolume`  | Volume up       |
| `XF86AudioLowerVolume`  | Volume down     |
| `XF86AudioMute`         | Toggle mute     |
| `XF86AudioMicMute`      | Toggle mic mute |
| `XF86MonBrightnessUp`   | Brightness up   |
| `XF86MonBrightnessDown` | Brightness down |
| `XF86AudioPlay/Pause`   | Play / pause    |
| `XF86AudioNext/Prev`    | Next / prev     |

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
| Action                   | Handler             |
|--------------------------|---------------------|
| PulseAudio click         | pavucontrol         |
| Scroll on windows module | workspace e-1 / e+1 |

---

## Occupied Mod+ Keys (Hyprland)
B, C, D, F, H, J, K, L, N, P, Q, R, S, T, V, 0-9, Minus, Equal
Mod+Shift: D, E, P, 0-9
