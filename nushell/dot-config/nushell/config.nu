source $"($nu.cache-dir)/carapace.nu"
source ~/.zoxide.nu
$env.config.use_ansi_coloring = true
$env.config.ls.use_ls_colors = true
$env.config.ls.clickable_links = true    # Enable clickable file links

let carapace_completer = {|spans|
    carapace $spans.0 nushell ...$spans | from json
}

$env.config = ($env.config | merge {
    completions: {
    	algorithm: fuzzy
        external: {
            enable: true
            max_results: 100
            completer: $carapace_completer
        }
    }
})
