$env.CARAPACE_BRIDGES = 'zsh'
$env.PATH = ($env.PATH | split row (char esep) | prepend ['/opt/homebrew/bin' '/Users/adamgrubbs/.local/bin'])
