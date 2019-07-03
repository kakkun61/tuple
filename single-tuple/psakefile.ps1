Task Format {
    Exec { Get-ChildItem -Filter '*.hs' -Recurse src | ForEach-Object { stack exec -- stylish-haskell -i $_.FullName } }
}

Task Lint {
    Exec { stack exec -- hlint src }
}
