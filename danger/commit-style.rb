# SPDX-FileCopyrightText: 2022 Oxhead Alpha
# SPDX-License-Identifier: LicenseRef-MIT-OA

require_relative 'helpers'

def wrap_workarounds(fun)
  return lambda { |msg|
    method(fun).call(msg + "\nSee also \\[Note\\].")
    markdown(
      "\\[Note\\]: Skip this check by adding `wip`, `tmp` or `[temporary]` to the commit subject. "\
      "Fixup commits (marked with `fixup!` or `squash!`) are also exempt from this check.")
  }
end

def check_commit_style (mywarn = wrap_workarounds(:warn), myfail = wrap_workarounds(:fail))
  # Proper commit style
  # Note: we do not use commit_lint plugin because it triggers on fixup commits
  git.commits.each { |commit|
    if commit.fixup? || commit.wip?
      next
    end

    subject = commit.subject
    subject_payload = subject.sub(issue_tags_pattern, "")
    subject_ticked = commit.subject_ticked

    unless has_valid_issue_tags(subject)
      # If any of these substrings is included into commit message,
      # we are fine with issue tag absence.
      exclusions = [
        # In lower-case
        "changelog"
      ]
      if exclusions.none? { |exc| subject.downcase.include?(exc) }
        mywarn.call("In #{commit.sha} message lacks issue id: #{subject_ticked}.")
      end
    end

    if subject_payload.start_with?(" ")
      mywarn.call("Extra space in commit #{commit.sha} subject after the issue tags: #{subject_ticked}.")
    elsif !subject_payload.start_with?(/[A-Z]/)
      mywarn.call("In #{commit.sha} subject does not begin with an uppercase letter: #{subject_ticked}.")
    end

    if subject[-1..-1] == '.'
      mywarn.call("In #{commit.sha} message ends with a dot: #{subject_ticked} :fire_engine:")
    end

    if subject.length > 90
      myfail.call("Nooo, such long commit message names do not work (#{commit.sha}).")
    elsif subject.length > 72
      mywarn.call("In commit #{commit.sha} message is too long (#{subject.length} chars), "\
        "please keep its length within 72 characters.")
    end

    if commit.message_body.empty?
      # If any of these substrings is included into commit message,
      # we are fine with commit description absence.
      exclusions = [
        # In lower-case
        "changelog"
      ]
      unless commit.chore? || exclusions.any? { |exc| subject.downcase.include?(exc) }
        myfail.call(
          "Commit #{commit.sha} lacks description :unamused:\n"\
          "Commits marked as `[Chore]` are exempt from this check."
          )
      end
    else
      # Checks on description

      if !commit.blank_line_after_subject?
        mywarn.call("In #{commit.sha} blank line is missing after the commit's subject.")
      end

      if !commit.chore?
        description_patterns = [
          /^Problem:[ \n].*^Solution:[ \n]/m,
          /And yes, I don't care about templates/
        ]
        unless description_patterns.any? { |pattern| pattern.match?(commit.description) }
          mywarn.call(
            "Description of #{commit.sha} does not follow the template.\n"\
            "Try `Problem:`/`Solution:` structure.\n"\
            "If you really have to, you can add `And yes, I don't care about templates` "\
            "to the commit message body."
          )
        end
      end
    end

  }
end
