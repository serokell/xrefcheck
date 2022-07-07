# SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

setup () {
  # change working directory to the location of the running `bats` suite.
  cd "$( dirname "$BATS_TEST_FILENAME")"
}

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
