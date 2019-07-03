Properties {
    $subpackages = "homotuple", "list-tuple", "single-tuple"
    $excludedPaths = "*\.stack-work\*"
    $crlfFiles = "*.ps1", "TAGS"
}

function NotLikeAny-Object ($Target, $Patterns) {
    $Patterns | ForEach-Object -Begin { $result = $True } -Process { $result = $result -and ($Target -notlike $_) } -End { $result }
}

function ConvertLF-File ($Path) {
    $content = Get-Content -Path $Path -Raw
    if ($content -eq $Null) { $content = "" }
    $content.Replace("`r`n", "`n") | ForEach-Object { [Text.Encoding]::Default.GetBytes($_) } | Set-Content -Path $Path -Encoding Byte
}

function Get-Subpsakefile {
    Get-ChildItem -Filter 'psakefile.ps1' $Subpackages
}

Function Invoke-Subpsake ($Task) {
    Get-SubPsakefile | ForEach-Object { Invoke-Psake $_.FullName -Task $Task }
}

Task Format {
    Invoke-Subpsake -Task Format
}

Task Lint {
    Invoke-Subpsake -Task Lint
}

Task Eol {
    @(Get-ChildItem -File -Exclude ".stack-work" .) + (Get-ChildItem -Recurse -File $subpackages) `
      | Where-Object { `
          (NotLikeAny-Object -Target $_.FullName -Patterns $ExcludedPaths) `
          -and (NotLikeAny-Object -Target $_.Name -Patterns $CrlfFiles) `
        } `
      | ForEach-Object { ConvertLF-File -Path $_.FullName }
}
