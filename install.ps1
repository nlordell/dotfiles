$ErrorActionPreference = "Stop"

function DotLink {
    param (
        [string]$Name,
        [string]$Location
    )
    
    if (Test-Path $Location) {
        Write-Warning "skipping ${Name}: '$Location' already exists."
        return
    }

    $target = "$PSScriptRoot\$Name"
    New-Item `
        -Path $(Split-Path $Location -Parent) `
        -Name $(Split-Path $Location -Leaf) `
        -Value $target `
        -ItemType Junction `
        | Out-Null
    Write-Host "linked '$target' to '$Location'"
}

DotLink "config\nvim" -Location "$HOME\AppData\Local\nvim"
DotLink "emacs.d" -Location "$HOME\AppData\Roaming\.emacs.d"

#nvim +PlugInstall +qall
