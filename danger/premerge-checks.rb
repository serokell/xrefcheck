# SPDX-FileCopyrightText: 2021 Oxhead Alpha
# SPDX-License-Identifier: LicenseRef-MIT-OA

# Checks that are fine to fail during development, but must be fixed before merging.

require_relative 'helpers'

# Fixup commits
if git.commits.any? &:fixup?
  fail "Some fixup commits are still there."
end

# Work-in-progress commits
if git.commits.any? &:wip?
  fail "WIP commits are still there."
end
