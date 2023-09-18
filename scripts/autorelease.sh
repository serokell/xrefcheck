#!/usr/bin/env bash

# Suggested usage:
# 1. Make sure your repository has `release/default.nix` that produces whatever you want to release.
# 2. Run this script from CI on each commit to master.
# 3. When you think it's time to make a real release, edit the auto release: change the tag, write a description, remove "pre-release" checkmark.

set -euo pipefail

# Project name, inferred from repository name
project=$(basename "$(pwd)")

# The directory in which artifacts will be created
TEMPDIR=$(mktemp -d)
function finish {
    rm -rf "$TEMPDIR"
}
trap finish EXIT

assets_dir=$TEMPDIR/assets

# Build release/default.nix
nix-build release -o "$TEMPDIR"/"$project" --arg timestamp "$(date +\"%Y%m%d%H%M\")"
mkdir -p "$assets_dir"

shopt -s extglob
cp -L "$TEMPDIR"/"$project"/!(*.md) "$assets_dir"

# gh expects 'GH_TOKEN' env variable to be set
gh auth setup-git --hostname github.com

# Delete release if it exists
gh release delete auto-release || true

# Update the tag
git fetch # So that the script can be run from an arbitrary checkout
git tag -f auto-release
git push --force --tags

# Combine all assets
assets=()
for file in $assets_dir/*; do
    echo "$file"
    assets+=("-a" "$file")
done

# Create release
gh release create auto-release --title auto-release --prerelease -F "$TEMPDIR"/"$project"/release-notes.md

# Upload artifacts
gh release upload auto-release "$assets_dir"/*
