# SPDX-FileCopyrightText: 2022 Oxhead Alpha
# SPDX-License-Identifier: LicenseRef-MIT-OA

require_relative 'helpers'

def check_branch_name
  # Proper branch name
  if branch_match = githost.branch_for_head.match(/([^\/]+)\/([^\-]+)-(.+)/)
    nick, issue_id, desc = branch_match.captures

    # We've decided not to put any restrictions on nickname for now

    unless /^(#\d+|chore)$/.match?(issue_id)
      warn(
        "Bad issue ID in branch name.\n"\
        "Valid format for issue IDs: `#123` or `chore`."
      )
    end

    weird_chars = desc.scan(/[^a-zA-Z\-\d]/)
    unless weird_chars.empty?
      warn(
        "Please, only use letters, digits and dashes in the branch name.
        Found: #{weird_chars}"
      )
    end
  elsif
    warn(
      "Please use `<nickname>/<issue-id>-<brief-description>`` format for branch names.`\n"\
      "Example: `lazyman/#123-my-commit`"
    )
  end
end
