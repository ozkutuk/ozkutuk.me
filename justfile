cache-name := "ozkutuk-blog"

default:
    just --list

alias b := build
build:
    cabal build

alias cl := clean
clean: build
    cabal exec ozkutuk-blog -- clean

alias w := watch
watch: clean
    cabal exec ozkutuk-blog -- watch

push-cachix:
    nix build --json \
    | jq -r '.[].outputs | to_entries[].value' \
    | cachix push {{cache-name}}

alias cr := create
create postname:
    ${EDITOR} content/posts/{{datetime_utc("%F")}}-{{postname}}

