# SPDX-FileCopyrightText: 2021 Oxhead Alpha
# SPDX-License-Identifier: LicenseRef-MIT-OA

### Some convenient extensions and helpers ###

class Danger::Dangerfile
  # Unified `github`/`gitlab` variable.
  def githost
    if defined?(github)
      githost = github
    elsif defined?(gitlab)
      githost = gitlab
    else
      error "Failed to figure out which service are we running from."
    end
  end

  # The original PR title (excludes "Draft" tags).
  def pr_title_payload
    githost.mr_title
      .sub(/^(Draft|WIP): /, "")
      .sub(/^\[Draft\] /, "")
  end
  alias_method :mr_title_payload, :pr_title_payload
end

class Git::Diff::DiffFile
  # When a file is renamed (e.g. with `git mv`) 'path' will return the old
  # path, this is true even if the file was modified a little.
  # However we'd probably like to access the destination path instead, so
  # this parses the new path from the 'file.patch'.
  def destination_path
    rename_match = /(?<=(\nrename to ))(\S)*/.match(self.patch)
    if rename_match.nil?
      self.path
    else
      rename_match.to_s
    end
  end
end

# Add some helpers to Commit class.
class Git::Object::Commit
  # Commit subject (unlike the 'message' field which includes description).
  def subject
    self.message.lines.first.rstrip
  end
  alias_method :message_subject, :subject

  def subject_ticked
    "`" + self.subject.gsub("`", "'") + "`"
  end

  # Commit description.
  # If absent, set to empty string.
  def description
    self.message.lines.drop(1).drop_while{ |s| s == "\n" }.join
  end
  alias_method :message_body, :description

  # Whether there is a blank line between commit subject and body.
  def blank_line_after_subject?
    self.message.lines[1] == "\n"
  end

  # Whether this commit is fixup commit.
  def fixup?
    return /\bfixup!|\bsquash!/.match?(subject)
  end

  # Whether this commit is a temporary commit.
  def wip?
    return /\bwip\b|\btmp\b|\[temporary\]/i.match?(subject)
  end

  # Whether this commit is a minor chore commit.
  # Such commits usually have an obvious purpose and are not related to the
  # business logic.
  def chore?
    return subject.include?("[Chore]")
  end

end

module Danger::Helpers::CommentsHelper
  # By default, every comment for a particular source code also includes
  # the name of the referred file.
  #
  # We don't need this feature.
  # The source code welcomes us to override the respective method, and this is
  # exactly what we do.
  def markdown_link_to_message(_, _)
    ""
  end
end

# Example: `[Chore][#123] My commit`
def issue_tags_pattern
  /^(\[(#\d+|Chore)\])+ (?=\w)/
end

# Whether a string starts with an appropriate ticket tag.
def has_valid_issue_tags(name)
  return name.start_with?(issue_tags_pattern)
end
