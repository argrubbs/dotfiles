export ZSH="$HOME/.oh-my-zsh"
export FZF_BASE="/run/current-system/sw/bin"

ZSH_THEME="robbyrussell"

zstyle ':omz:update' mode auto      # update automatically without asking

ENABLE_CORRECTION="true"

COMPLETION_WAITING_DOTS="true"

HIST_STAMPS="mm/dd/yyyy"

plugins=(git zoxide colored-man-pages command-not-found sudo fzf)

source $ZSH/oh-my-zsh.sh

if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='vim'
else
  export EDITOR='emacs'
fi

if command -v npm &> /dev/null; then
   mkdir -p ~/.npm-global
   npm config set prefix ~/.npm-global
   [[ -d ~/.npm-global/bin ]] && path+=(~/.npm-global/bin)
fi

alias ls='eza --all --group-directories-first --long --hyperlink --icons=always --git --git-repos'

typeset -U path
path_dirs=(
    ~/.npm-global/bin
    ~/.local/bin
    ~/.config/emacs/bin
    /opt/homebrew/bin
)

for dir in $path_dirs; do
    [[ -d $dir ]] && path+=($dir)
done
export PATH

if (( ${PATH[(Ie)"/opt/homebrew/bin"]} ));then
    export PKG_CONFIG_PATH="/opt/homebrew/lib/pkgconfig"
    export LDFLAGS="-L/opt/homebrew/lib"
    export CPPFLAGS="-I/opt/homebrew/include"
fi

export FZF_DEFAULT_OPTS="--preview '[[ -d {} ]] && eza -la {} || bat --style=numbers --color=always {}' --preview-window=right:50%"
export FZF_CTRL_T_OPTS="--preview 'bat --style=numbers --color=always {}'"

export XDG_CONFIG_DIR="~/.config"

if [[ -f "/opt/homebrew/bin/brew" ]]
then
    eval "$(/opt/homebrew/bin/brew shellenv)"
fi

vterm_printf() {
    if [ -n "$TMUX" ] \
        && { [ "${TERM%%-*}" = "tmux" ] \
            || [ "${TERM%%-*}" = "screen" ]; }; then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}
