#!/usr/bin/env bash

# SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

# This script verifies that the repo adheres to the stylish-haskell rules.
#
# It does this by running `make stylish` on the repo and checking
# that no files were affected.

set -euo pipefail

make stylish

# Note: we temporarily disable `-e`;
# otherwise the script would exit when `git diff` returns 1.
set +e
diff=$(git diff --exit-code --name-only)
exitCode=$?
set -e

if [ "$exitCode" != 0 ]; then
    echo "Found files that do not adhere to stylish-haskell."
    echo "Run 'make stylish' on the repository to fix this."
    echo ""
    echo "Offending files:"
    echo "$diff"
    exit 1
fi
