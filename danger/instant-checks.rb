# SPDX-FileCopyrightText: 2022 Oxhead Alpha
# SPDX-License-Identifier: LicenseRef-MIT-OA

# Checks that, when hit, should be fixed as soon as possible.

require_relative 'helpers'
require_relative 'trailing-whitespaces'
require_relative 'commit-style'
require_relative 'branch-name'
require_relative 'licenses'

check_trailing_whitespaces()

# Clean commits history
if git.commits.any? { |c| c.subject =~ /^Merge branch/ }
  fail 'Please, no merge commits. Rebase for the win.'
end

check_commit_style()

# Proper MR content
mr_title_payload = githost.mr_title_payload

unless has_valid_issue_tags(mr_title_payload)
  warn(
    "Inappropriate title for PR.\n"\
    "Should start from issue ID (e.g. `[#123]`) or `[Chore]` tag.\n"\
    "Note: please use `[Chore]` also for tickets tracked internally on YouTrack."
  )
end

check_branch_name()

check_licenses()
