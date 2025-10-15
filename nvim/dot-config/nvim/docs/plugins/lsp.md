# LSP Plugins

## LSP Configuration
- **LSP Server**: `nvim-lspconfig` with `cmp_nvim_lsp` integration
- **Diagnostic Signs**: Customized with icons for error/warning/hint

## Language Support

### Lua
- **Capabilities**: Full Lua support with `lua_ls`
- **Settings**: 
  - Diagnostics: Global `vim` namespace
  - Workspace: Includes standard Lua libraries and config directory

### Python
- **Primary Server**: `pylsp` with all standard plugins disabled
- **Additional**: 
  - `ruff` for linting and type checking
  - Configured for PEP8 compliance

### Go
- **Server**: `gopls`
- **Enabled Features**: 
  - Staticcheck
  - GoFumpt
  - Unused parameter detection

### Terraform
- **Server**: `terraformls`
- **Features**: Basic syntax and linting support

### Ansible
- **Server**: `ansiblels`
- **Features**: Role/task syntax validation

### JSON
- **Server**: `jsonls`
- **Features**: Basic validation and formatting

### YAML
- **Server**: `yamlls`
- **Features**: 
  - Kubernetes schema support
  - GitHub workflow schema
  - Ansible 2.9 schema

### Bash
- **Server**: `bashls`
- **Features**: Syntax and linting support

## Configuration Notes
- All servers use `cmp_nvim_lsp` for completion
- Diagnostic signs use modern Neovim 0.11+ syntax