Task Format {
    Exec { Get-ChildItem -Filter '*.hs' -Recurse src, test | ForEach-Object { stylish-haskell -i $_.FullName } }
}

Task Lint {
    Exec { hlint src }
}
