# Yazi Configuration

A productivity-focused configuration for [Yazi](https://github.com/soimort/yazi), a blazing fast terminal file manager.

## Quick Start

### Prerequisites
```bash
# Install yazi (via your package manager or cargo)
brew install yazi  # macOS
pacman -S yazi     # Arch Linux
cargo install yazi # From source
```

### Optional Dependencies (for full functionality)
```bash
# Preview support
brew install chafa imagemagick     # Image previews
brew install bat highlight         # Code syntax highlighting
brew install zathura              # PDF viewer

# File operations
brew install fzf rg              # Search integration
brew install atool               # Archive extraction
```

## Configuration Files

### `yazi.toml` - Main Configuration
- **Manager**: Controls file browser layout and behavior
- **Preview**: Image and file preview settings
- **Open Rules**: Define how to open different file types
- **Tasks**: Thread pool and performance settings

### `keymap.toml` - Keybindings
Navigation is vim-style by default:
- `j/k` - Move up/down
- `h/l` - Go back/forward
- `v` - Toggle selection
- `y` - Yank (copy)
- `p` - Paste
- `d` - Delete
- `/` - Search with fd
- `?` - Search with rg
- `` ` `` - Open shell
- `.` - Toggle hidden files

Custom bindings for productivity:
- `<Space>` - Toggle selection
- `<C-a>` - Select all
- `<C-u>` - Deselect all
- `<C-o>` - Open file
- `<C-h>` - Toggle hidden files

### `theme.toml` - Visual Appearance
- Based on Catppuccin color scheme (modern and easy on the eyes)
- Customizable colors and styles
- Icons support with Nerd Fonts

### `plugins/` - Lua Scripts
- Extend functionality with Lua scripts
- Git integration, fzf integration, etc.

## Tips & Tricks

### Customizing the Theme
Edit `theme.toml` to change:
- Colors for different file types
- Background and foreground colors
- Selection highlight colors

### Adding Custom File Openers
In `yazi.toml`, modify the `[open.rules]` section:
```toml
# Add a custom rule for a file type
{ name = "markdown", mime = "text/markdown", use = "markdown-viewer" }

# Define the opener command
markdown-viewer = ["glow", "-p", "{}"]
```

### Extending with Plugins
Create new Lua files in `plugins/` directory:
```lua
-- Custom plugin example
local api = require("yazi.api")
-- Your plugin code here
```

## Integration with Shell

Add to your shell profile (`.bashrc`, `.zshrc`, etc.):
```bash
# Yazi function that changes directory on exit
function yy() {
  local tmp="$(mktemp -t "yazi-cwd.XXXXXX")"
  yazi "$@" --cwd-file="$tmp"
  if cwd=$(cat -- "$tmp"); then
    [ -d "$cwd" ] && builtin cd -- "$cwd"
  fi
  rm -f -- "$tmp"
}
```

Then use `yy` to open yazi and automatically navigate to the selected directory when you exit.

## Troubleshooting

### Images not showing
- Ensure you have `chafa` or `imagemagick` installed
- Check your terminal's capabilities (kitty/WezTerm support sixel, iTerm2 supports native images)

### Keybindings not working
- Make sure the key is available in your terminal
- Check for conflicts with shell/editor keybindings

### File preview is slow
- Increase `image_timeout` in `yazi.toml`
- Disable previews for certain file types

## Resources

- [Yazi Documentation](https://yazi-rs.github.io/)
- [Catppuccin Theme](https://catppuccin.com/)
- [Nerd Fonts](https://www.nerdfonts.com/)
