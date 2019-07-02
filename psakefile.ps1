function Get-SubPsakefile {
    Get-ChildItem -Filter 'psakefile.ps1' homotuple, homotuple-only, list-tuple, single-tuple
}

function Invoke-SubPsake ($Task) {
    Get-SubPsakefile | ForEach-Object { Invoke-Psake $_.FullName -Task $Task }
}

Task Format {
    Exec { Invoke-SubPsake -Task Format }
}

Task Lint {
    Exec { Invoke-SubPsake -Task Lint }
}
