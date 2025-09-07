# ZSH config
export ZSH="$HOME/.oh-my-zsh"
ZSH_THEME="robbyrussell"
plugins=(git zoxide colored-man-pages command-not-found sudo fzf vi-mode)
VI_MODE_SET_CURSOR=true
source $ZSH/oh-my-zsh.sh

# Aliases
alias ls='eza --color=always --all --long --git --icons --classify --group-directories-first'
alias cat=bat
alias vim=nvim
alias n=nvim

# Environment Variables
export PATH=~/.npm-global/bin:~/.local/bin:$PATH
export PKG_CONFIG_PATH="/opt/homebrew/lib/pkgconfig"
export LDFLAGS="-L/opt/homebrew/lib"
export CPPFLAGS="-I/opt/homebrew/include"
export DYLD_LIBRARY_PATH="/opt/homebrew/lib:$DYLD_LIBRARY_PATH"

# Starship prompt
eval "$(starship init zsh)"
