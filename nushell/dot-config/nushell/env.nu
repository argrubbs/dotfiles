$env.CARAPACE_BRIDGES = 'zsh'
$env.PATH = ($env.PATH | split row (char esep) | prepend ['/opt/homebrew/bin' '/Users/adamgrubbs/.local/bin' '/usr/local/texlive/2022basic/bin/universal-darwin'])
