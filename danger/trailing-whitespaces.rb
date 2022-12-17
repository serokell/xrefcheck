# SPDX-FileCopyrightText: 2021 Oxhead Alpha
# SPDX-License-Identifier: LicenseRef-MIT-OA

require_relative 'helpers'

# Report that there are some issues with trailing whitespaces or newlines.
def report_trailing_whitespaces_violation
  # This can be safely called multiple times
  # because Danger deduplicates messages.
  fail("Trailing whitespaces and/or incorrect end-of-file newlines detected.")
end

def check_trailing_whitespaces
  git.diff.each do |file|
    if !["new", "modified"].include?(file.type) || file.binary?
      next
    end

    path = file.destination_path
    contents = File.read(path)
    lines = contents.lines

    if contents.empty?
      next
    end

    lines.each.with_index(1) do |line, line_index|
      if line[-1..-1] == "\n"
        line = line[0..-2]
      end
      if /\s$/.match?(line)
        report_trailing_whitespaces_violation
        markdown(
          "I have found some trailing whitespaces here:\n"\
          "```suggestion:-0+0\n"\
          "#{line.rstrip}\n"\
          "```\n",
          file: path, line: line_index
        )
      end
    end

    last_line = lines.last
    unless last_line[-1..-1] == "\n"
      report_trailing_whitespaces_violation
      markdown(
        "I have found a missing newline at the end of this file:\n"\
        "```suggestion:-0+0\n"\
        "#{last_line.rstrip}\n\n"\
        "```",
        file: path, line: lines.length
      )
    end

    trailing_empty_lines = 0
    lines.reverse_each do |line|
      if line == "\n" then
        trailing_empty_lines = trailing_empty_lines + 1
      else
        break
      end
    end
    if trailing_empty_lines == 0
    elsif trailing_empty_lines == 1
      trailing_newline_err_msg = "Extra newline at the end of the file."
    elsif trailing_empty_lines <= 3
      trailing_newline_err_msg = "Yay, that's a combo!"
    else
      pic_url = "https://raw.githubusercontent.com/serokell/resources/ed58049e3724f11cef43d45bf3958878a716fc47/dangerbot/pics/trailing-whitespaces-voilation.jpg"
      trailing_newline_err_msg = "![:thinking:](#{pic_url})"
    end
    if !trailing_newline_err_msg.nil?
      report_trailing_whitespaces_violation
      markdown(
        "#{trailing_newline_err_msg}\n"\
        "```suggestion:-#{trailing_empty_lines - 1}+0\n"\
        "```\n",
        file: path, line: lines.length
      )
    end
  end
end
