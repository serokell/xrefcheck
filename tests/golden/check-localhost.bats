#!/usr/bin/env bats

# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

load 'helpers/bats-support/load'
load 'helpers/bats-assert/load'

# this function is used for:
# - delete all color characters
# - replace socket port with N
# - replace multiple connection retry errors with single one
#   (because at some machine there are one error and at others - two)
prepare () {
  sed -r "s/[[:cntrl:]]\[[0-9]{1,3}m//g" \
    | sed 's/socket: [0-9]*/socket: N/g' \
    | sed 's/Network.Socket.connect: <socket: N>: does not exist (Connection refused),//g'
}

@test "Config: check-localhost=false, CLA --check-localhost not provided" {
  run xrefcheck \
    -c check-localhost/config-check-disabled.yaml \
    -r check-localhost

  assert_output --partial "All repository links are valid."
}

@test "Config: check-localhost=false, CLA --check-localhost provided" {
  xrefcheck \
    -c check-localhost/config-check-disabled.yaml \
    -r check-localhost \
    --check-localhost \
    | prepare > /tmp/check-localhost.test || true

  diff /tmp/check-localhost.test check-localhost/expected.gold \
    --ignore-space-change \
    --ignore-blank-lines \
    --new-file # treat absent files as empty

  rm /tmp/check-localhost.test
}

@test "Config: check-localhost=true, CLA --check-localhost not provided" {
  xrefcheck \
    -c check-localhost/config-check-enabled.yaml \
    -r check-localhost \
    | prepare > /tmp/check-localhost.test || true

  diff /tmp/check-localhost.test check-localhost/expected.gold \
    --ignore-space-change \
    --ignore-blank-lines \
    --new-file # treat absent files as empty

  rm /tmp/check-localhost.test
}

@test "Config: check-localhost=true, CLA --check-localhost provided" {
  xrefcheck \
    -c check-localhost/config-check-enabled.yaml \
    -r check-localhost \
    --check-localhost \
    | prepare > /tmp/check-localhost.test || true

  diff /tmp/check-localhost.test check-localhost/expected.gold \
    --ignore-space-change \
    --ignore-blank-lines \
    --new-file # treat absent files as empty

  rm /tmp/check-localhost.test
}

@test "Config: missing, CLA --check-localhost not provided" {
  run xrefcheck

  assert_output --partial "All repository links are valid."
}

@test "Config: missing, CLA --check-localhost provided" {
  xrefcheck \
    --check-localhost \
    | prepare > /tmp/check-localhost.test || true

  diff /tmp/check-localhost.test check-localhost/expected.gold \
    --ignore-space-change \
    --ignore-blank-lines \
    --new-file # treat absent files as empty

  rm /tmp/check-localhost.test
}
