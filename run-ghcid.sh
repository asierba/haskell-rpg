#!/usr/bin/env bash

ghcid --command "stack ghci haskell-rpg:lib haskell-rpg:test:haskell-rpg-test --ghci-options='-fobject-code -Werror -Wall'" -T=main
