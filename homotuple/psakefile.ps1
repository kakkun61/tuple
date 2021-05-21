
Task Format {
    Exec { Get-ChildItem -Filter '*.hs' -Recurse src | ForEach-Object { stylish-haskell -i $_.FullName } }
}

Task Lint {
    Exec { hlint src }
}
