$env.CARAPACE_BRIDGES = 'zsh,fish,bash,inshellisense' # optional
mkdir $"($nu.cache-dir)"
carapace _carapace nushell | save --force $"($nu.cache-dir)/carapace.nu"

$env.PATH = ($env.PATH | split row (char esep) | append ['/Users/adamgrubbs/.local/bin', '/opt/homebrew/bin'])
