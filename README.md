# Emacs Configuration

A personal, portable Emacs configuration optimized for productive work. Minimal, vanilla approach (Emacs 29+) with no OS-specific integration.

## Quick Start

```bash
git clone <this-repo> ~/my-emacs
cd ~/my-emacs
./install.sh
emacs
```

Packages install automatically on first launch. No manual steps, no tribal knowledge required.

## Features

### Window Management

| Key | Function | Behavior |
|-----|----------|----------|
| `C-c b` | Jump to window on left | Directional navigation (from window-jump) |
| `C-c f` | Jump to window on right | |
| `C-c p` | Jump to window above | |
| `C-c n` | Jump to window below | |
| `C-c m` | Maximize window | Enlarge current window (others stay visible) |
| `C-c C-<left>/<right>` | Undo/redo window layout | Built-in winner-mode |
| `<up>/<down>/<left>/<right>` | Resize window | Shrink/enlarge in arrow direction |

### Navigation & Search

| Key | Function | Notes |
|-----|----------|-------|
| `C-c i` | Open init.el | Quick access to config |
| `C-c s` | Project file search | Fuzzy, gitignore-aware (projectile + vertico + orderless) |
| `C-a` (smart) | Jump to indentation or line start | Toggle: first call → indentation, second → column 0 |

### Visual

- **Theme:** Zenburn (dark, warm, easy on the eyes)
- **Mode line:** Telephone-line (Powerline-style arrow segments)
- **UI:** Minimal — no menu bar, toolbar, or scrollbar
- **Git indicators:** diff-hl (left margin, colored for added/changed/removed)

### Quality of Life

- **Undo tree:** Visual undo history (`C-x u`)
- **Package management:** use-package (declarative, bundled since Emacs 29)
- **Completion:** Vertico + Orderless + Marginalia (vertical list, fuzzy matching, colorful annotations)
- **Error resilience:** Failed package installs never abort startup (logged to `*Messages*` instead)
- **File organization:** Backup/autosave files centralized in `~/.config/emacs/var/`

## Architecture

- **`init.el`** — Main config; bootstraps package.el, loads lisp modules, declares packages
- **`lisp/init-functions.el`** — Custom functions and macros (`use-package!`, smart C-a, open-init)
- **`lisp/keybindings.el`** — All global keybindings (C-c commands, arrow keys, remaps)
- **`site-lisp/window-jump.el`** — Vendored directional window navigation (chumpy-windows), no network dependency
- **`test/`** — ERT tests for functions and keybindings (run with `./run-tests.sh`)

## Testing

Run the test suite:

```bash
./run-tests.sh
```

Tests are hermetic (no network, no external packages) and cover:
- Keybinding contract (all C-c/arrow/smart-C-a mappings)
- Custom function behavior (smart C-a toggle, my-open-init-file)
- Package loading resilience (use-package! error handling)

## Requirements

- **GNU Emacs 29.1** or later (for bundled `use-package`)
- Packages auto-install from MELPA on first launch
- No OS-specific code; works on macOS and Linux

## Dependencies

All packages auto-install from MELPA via `init.el`. First launch may take a minute or two while packages download:

- `zenburn-theme` — Dark, warm color scheme
- `diff-hl` — Git diff gutter indicators
- `undo-tree` — Visual undo history
- `vertico` — Vertical minibuffer completion
- `orderless` — Flexible fuzzy matching
- `marginalia` — Completion annotations
- `projectile` — Project-aware file discovery
- `telephone-line` — Powerline-style mode line

Local/bundled:
- `window-jump` (from chumpy-windows) — Directional window navigation

## License

This code is licensed under the **MIT License**. See `LICENSE` for details.

**Exception:** `site-lisp/window-jump.el` is third-party code by Steven Thomas (from the [chumpy-windows](https://github.com/chumpage/chumpy-windows) project) and is licensed under the **GNU General Public License v3**. See `site-lisp/LICENSE-window-jump` for details.

